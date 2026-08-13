#ifndef STANR_R_OUTPUT_HPP
#define STANR_R_OUTPUT_HPP

#include <cpp11.hpp>
#include <stan/callbacks/logger.hpp>
#include <stan/callbacks/writer.hpp>
#include <stan/callbacks/structured_writer.hpp>
#include <Eigen/Dense>
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <vector>
#include <string>

namespace stanr {

// Collects parameter samples into an Eigen::MatrixXd. Safe for single-writer
// access per chain during parallel (multi-chain) sampling.
// Hidden visibility: compiled both into the package .so and into every
// per-model .so via libstanr_runner.a -- see r_data_context.hpp.
class __attribute__((visibility("hidden"))) r_sample_writer
    : public stan::callbacks::writer {
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
    // Chain separator -- do nothing (all chains stacked in same DataFrame)
  }

  void operator()(const std::string& message) override {
    messages_.push_back(message);
  }

  // A matrix is one sample per row; a column/row vector is a single sample.
  void operator()(const Eigen::MatrixXd& values) override {
    check_initialized();
    for (Eigen::Index i = 0; i < values.rows(); ++i) {
      this->append_row(values.row(i));
    }
  }

  void operator()(const Eigen::Matrix<double, -1, 1>& values) override {
    check_initialized();
    this->append_row(values.transpose());
  }

  void operator()(const Eigen::Matrix<double, 1, -1>& values) override {
    check_initialized();
    this->append_row(values);
  }

  /** Convert collected data to an R matrix. Main R thread only. */
  cpp11::writable::doubles_matrix<> to_r_matrix() const {
    cpp11::writable::doubles_matrix<> r_mat(n_rows_, n_cols_);
    if (n_rows_ > 0) {
      // r_mat.begin() is a cpp11 iterator, not a raw pointer -- go through
      // REAL() for the zero-copy Eigen::Map write.
      Eigen::Map<Eigen::MatrixXd>(REAL(r_mat.data()), n_rows_, n_cols_) =
        values_.topLeftCorner(n_rows_, n_cols_);
    }
    // matrix::attr() returns a plain value, not an assignable proxy like
    // r_vector/list's -- Rf_setAttrib() is required here.
    Rf_setAttrib(r_mat.data(), R_DimNamesSymbol, cpp11::writable::list(
        {R_NilValue, cpp11::as_sexp(colnames_)}));
    return r_mat;
  }

  // Column v's data (first n_rows_ entries valid; buffer may be
  // over-allocated from growth). Main R thread only.
  const double* column_ptr(int v) const { return values_.col(v).data(); }

  // Frees the sample buffer once its chain has been copied out.
  void release() { values_.resize(0, 0); }

  const std::vector<std::string>& colnames() const { return colnames_; }
  const std::vector<std::string>& messages() const { return messages_; }
  int n_rows() const { return n_rows_; }
  int n_cols() const { return n_cols_; }

  // Signal validity so Stan's concurrent_writer writes directly to us.
  bool is_valid() const noexcept override { return true; }
};

}  // namespace stanr

#endif  // STANR_R_OUTPUT_HPP
