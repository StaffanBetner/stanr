#ifndef NEWSTAN_R_OUTPUT_HPP
#define NEWSTAN_R_OUTPUT_HPP

#include <Rcpp.h>
#include <RcppEigen.h>
#include <stan/callbacks/logger.hpp>
#include <stan/callbacks/writer.hpp>
#include <stan/callbacks/structured_writer.hpp>
#include <Eigen/Dense>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <stdexcept>
#include <vector>
#include <string>

namespace newstan {

// Collects parameter samples into an Eigen::MatrixXd. Safe for single-writer
// access per chain during parallel (multi-chain) sampling.
class r_sample_writer : public stan::callbacks::writer {
 private:
  std::vector<std::string> colnames_;
  std::vector<std::string> messages_;  // string messages (e.g., from diagnose)
  Eigen::MatrixXd values_;       // column-major: rows=samples, cols=parameters
  int n_rows_;
  int n_cols_;
  bool initialized_;
  int expected_rows_;

 public:
  explicit r_sample_writer(int expected_rows = 0)
    : n_rows_(0)
    , n_cols_(0)
    , initialized_(false)
    , expected_rows_(expected_rows) {}

 private:
  // Append a single row (Eigen row vector) to the matrix, growing if needed
  template <typename RowType>
  void append_row(const RowType& row) {
    int n = static_cast<int>(row.size());
    if (n_cols_ != 0 && n != n_cols_) {
      throw std::runtime_error(
          "r_sample_writer: row has " + std::to_string(n) +
          " columns, expected " + std::to_string(n_cols_) + ".");
    }
    if (n_cols_ == 0) {
      n_cols_ = n;
      if (expected_rows_ > 0) {
        values_.resize(expected_rows_, n_cols_);
      }
    }
    if (n_rows_ >= static_cast<int>(values_.rows())) {
      int new_rows = std::max(
        static_cast<int>(std::ceil(1.5 * values_.rows())),
        n_rows_ + 100
      );
      Eigen::MatrixXd new_values(new_rows, n_cols_);
      if (n_rows_ > 0) {
        new_values.topLeftCorner(n_rows_, n_cols_) =
          values_.topLeftCorner(n_rows_, n_cols_);
      }
      values_ = std::move(new_values);
    }
    values_.row(n_rows_) = row;
    n_rows_++;
  }

  void check_initialized() const {
    if (!initialized_) {
      throw std::runtime_error(
          "r_sample_writer: received a value row before the column names.");
    }
  }

 public:

  void operator()(const std::vector<std::string>& names) override {
    n_cols_ = static_cast<int>(names.size());
    colnames_ = names;
    // Pre-allocate Eigen matrix with expected capacity
    if (expected_rows_ > 0) {
      values_.resize(expected_rows_, n_cols_);
    }
    initialized_ = true;
  }

  void operator()(const std::vector<double>& state) override {
    check_initialized();
    this->append_row(Eigen::Map<const Eigen::RowVectorXd>(
      state.data(), static_cast<Eigen::Index>(state.size())));
  }

  void operator()() override {
    // Chain separator — do nothing (all chains stacked in same DataFrame)
  }

  void operator()(const std::string& message) override {
    messages_.push_back(message);
  }

  void operator()(const Eigen::MatrixXd& values) override {
    // Handle matrix outputs. Each row is treated as a sample.
    check_initialized();
    for (Eigen::Index i = 0; i < values.rows(); ++i) {
      this->append_row(values.row(i));
    }
  }

  void operator()(const Eigen::Matrix<double, -1, 1>& values) override {
    // Column vector — treat as a single row sample (transposed)
    check_initialized();
    this->append_row(values.transpose());
  }

  void operator()(const Eigen::Matrix<double, 1, -1>& values) override {
    // Row vector — treat as a single row sample (used by pathfinder)
    check_initialized();
    this->append_row(values);
  }

  /**
   * Convert collected data to an R matrix via RcppEigen.
   *
   * MUST be called from the main R thread (not from a TBB worker thread).
   */
  Rcpp::NumericMatrix to_r_matrix() const {
    Rcpp::NumericMatrix r_mat(n_rows_, n_cols_);
    if (n_rows_ > 0) {
      Eigen::Map<Eigen::MatrixXd>(r_mat.begin(), n_rows_, n_cols_) =
        values_.topLeftCorner(n_rows_, n_cols_);
    }
    r_mat.attr("dimnames") = Rcpp::List::create(
      R_NilValue,
      Rcpp::CharacterVector(colnames_.begin(), colnames_.end())
    );
    return r_mat;
  }

  /**
   * Copy the stored columns into preallocated R vectors.  This is intended for
   * assembling multi-chain output on R's main thread without first creating a
   * combined Eigen matrix.
   */
  void copy_to_r_columns(Rcpp::List& columns, int row_offset) const {
    if (n_rows_ == 0) return;
    for (int j = 0; j < n_cols_; ++j) {
      Rcpp::NumericVector column = columns[j];
      std::memcpy(column.begin() + row_offset, values_.col(j).data(),
                  static_cast<size_t>(n_rows_) * sizeof(double));
    }
  }

  const std::vector<std::string>& colnames() const { return colnames_; }
  const std::vector<std::string>& messages() const { return messages_; }
  int n_rows() const { return n_rows_; }
  int n_cols() const { return n_cols_; }

  // Signal that this writer is valid so Stan's concurrent_writer
  // writes directly to us rather than copying via value semantics.
  bool is_valid() const noexcept override { return true; }
};

// The Stan sampler writes diagnostics (energy__, divergent__, etc.) to a
// separate callback even though the sample callback already receives the
// sampler columns.  Sampling currently exposes one combined sample data
// frame, so retaining a second matrix only duplicates memory and work.
class r_discard_writer : public stan::callbacks::writer {
 public:
  void operator()(const std::vector<std::string>&) override {}
  void operator()(const std::vector<double>&) override {}
  void operator()() override {}
  void operator()(const std::string&) override {}
  void operator()(const Eigen::MatrixXd&) override {}
  void operator()(const Eigen::Matrix<double, -1, 1>&) override {}
  void operator()(const Eigen::Matrix<double, 1, -1>&) override {}
};

}  // namespace newstan

#endif  // NEWSTAN_R_OUTPUT_HPP
