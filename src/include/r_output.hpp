#ifndef NEWSTAN_R_OUTPUT_HPP
#define NEWSTAN_R_OUTPUT_HPP

#include <Rcpp.h>
#include <RcppEigen.h>
#include <stan/callbacks/logger.hpp>
#include <stan/callbacks/writer.hpp>
#include <stan/callbacks/structured_writer.hpp>
#include <Eigen/Dense>
#include <vector>
#include <string>
#include <sstream>

namespace newstan {

// ===================================================================
// Sample writer — collects parameter samples using Eigen::MatrixXd
// for cache-friendly, vectorized storage. Thread-safe for single-writer
// access per chain during parallel (multi-chain) sampling.
// ===================================================================

class r_sample_writer : public stan::callbacks::writer {
 private:
  std::vector<std::string> colnames_;
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
    if (!initialized_) return;
    int n = static_cast<int>(state.size());
    if (n != n_cols_) return;

    // Grow matrix if we exceed allocated rows
    if (n_rows_ >= static_cast<int>(values_.rows())) {
      int new_rows = std::max(
        static_cast<int>(std::ceil(1.5 * values_.rows())),
        n_rows_ + 100
      );
      Eigen::MatrixXd new_values(new_rows, n_cols_);
      // Copy existing data
      if (n_rows_ > 0) {
        new_values.topLeftCorner(n_rows_, n_cols_) =
          values_.topLeftCorner(n_rows_, n_cols_);
      }
      values_ = std::move(new_values);
    }

    values_.row(n_rows_) = Eigen::Map<const Eigen::RowVectorXd>(
      state.data(), static_cast<Eigen::Index>(n));
    n_rows_++;
  }

  void operator()() override {
    // Chain separator — do nothing (all chains stacked in same DataFrame)
  }

  void operator()(const std::string& message) override {
    // Comments from Stan are ignored for in-memory collection
  }

  void operator()(const Eigen::MatrixXd& values) override {
    // Matrix output — not used by standard sampling
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
   * Return a copy of the collected data as an Eigen matrix.
   */
  Eigen::MatrixXd to_matrix() const {
    if (n_rows_ == 0) return Eigen::MatrixXd(0, n_cols_);
    return values_.topLeftCorner(n_rows_, n_cols_);
  }

  const std::vector<std::string>& colnames() const { return colnames_; }
  int n_rows() const { return n_rows_; }
  int n_cols() const { return n_cols_; }
};

// ===================================================================
// Diagnostic writer — collects diagnostic columns (energy__, divergent__, etc.)
// Uses Eigen::MatrixXd for cache-friendly storage during parallel sampling.
// ===================================================================

class r_diagnostic_writer : public stan::callbacks::writer {
 private:
  std::vector<std::string> colnames_;
  Eigen::MatrixXd values_;       // column-major: rows=samples, cols=diagnostics
  int n_rows_;
  int n_cols_;
  bool initialized_;
  int expected_rows_;

 public:
  explicit r_diagnostic_writer(int expected_rows = 0)
    : n_rows_(0)
    , n_cols_(0)
    , initialized_(false)
    , expected_rows_(expected_rows) {}

  void operator()(const std::vector<std::string>& names) override {
    n_cols_ = static_cast<int>(names.size());
    colnames_ = names;
    if (expected_rows_ > 0) {
      values_.resize(expected_rows_, n_cols_);
    }
    initialized_ = true;
  }

  void operator()(const std::vector<double>& state) override {
    if (!initialized_) return;
    int n = static_cast<int>(state.size());
    if (n != n_cols_) return;

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

    values_.row(n_rows_) = Eigen::Map<const Eigen::RowVectorXd>(
      state.data(), static_cast<Eigen::Index>(n));
    n_rows_++;
  }

  void operator()() override {}

  void operator()(const std::string& message) override {}

  void operator()(const Eigen::MatrixXd& values) override {}

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

  Eigen::MatrixXd to_matrix() const {
    if (n_rows_ == 0) return Eigen::MatrixXd(0, n_cols_);
    return values_.topLeftCorner(n_rows_, n_cols_);
  }

  const std::vector<std::string>& colnames() const { return colnames_; }
  int n_rows() const { return n_rows_; }
  int n_cols() const { return n_cols_; }
};

// ===================================================================
// Metric writer — collects adaptation metrics as JSON string
// ===================================================================

class r_metric_writer : public stan::callbacks::structured_writer {
 private:
  std::ostringstream ss_;
  int depth_;
  std::vector<bool> has_key_;  // Track if a key was already written in current record

 public:
  r_metric_writer() : depth_(0) {}

  void begin_record() override {
    if (depth_ > 0) ss_ << ",";
    for (int i = 0; i < depth_; ++i) ss_ << "  ";
    has_key_.push_back(false);
    depth_++;
  }

  void begin_record(const std::string& key) override {
    if (depth_ > 0) ss_ << ",";
    for (int i = 0; i < depth_; ++i) ss_ << "  ";
    ss_ << "\"" << key << "\": {";
    ss_ << "\n";
    has_key_.push_back(false);
    depth_++;
  }

  void end_record() override {
    depth_--;
    if (depth_ > 0) {
      ss_ << "\n";
      for (int i = 0; i < depth_ - 1; ++i) ss_ << "  ";
    }
    ss_ << "}";
  }

  void write(const std::string& key) override {
    indent(key);
    ss_ << "null";
  }

  void write(const std::string& key, const std::string& value) override {
    indent(key);
    ss_ << "\"" << value << "\"";
  }

  void write(const std::string& key, bool value) override {
    indent(key);
    ss_ << (value ? "true" : "false");
  }

  void write(const std::string& key, int value) override {
    indent(key);
    ss_ << value;
  }

  void write(const std::string& key, unsigned int value) override {
    indent(key);
    ss_ << value;
  }

  void write(const std::string& key, size_t value) {
    indent(key);
    ss_ << value;
  }

  void write(const std::string& key, double value) override {
    indent(key);
    ss_ << value;
  }

  void write(const std::string& key, const Eigen::VectorXd& vec) override {
    indent(key);
    ss_ << "[";
    for (Eigen::Index i = 0; i < vec.size(); ++i) {
      if (i > 0) ss_ << ", ";
      ss_ << vec[i];
    }
    ss_ << "]";
  }

  void write(const std::string& key, const Eigen::MatrixXd& mat) override {
    indent(key);
    ss_ << "[";
    for (Eigen::Index i = 0; i < mat.rows(); ++i) {
      if (i > 0) ss_ << ", ";
      ss_ << "[";
      for (Eigen::Index j = 0; j < mat.cols(); ++j) {
        if (j > 0) ss_ << ", ";
        ss_ << mat(i, j);
      }
      ss_ << "]";
    }
    ss_ << "]";
  }

  std::string json_string() const { return ss_.str(); }

 private:
  void indent(const std::string& key) {
    if (has_key_.back()) ss_ << ", ";
    has_key_.back() = true;
    for (int i = 0; i < depth_; ++i) ss_ << "  ";
    ss_ << "\"" << key << "\": ";
  }
};

}  // namespace newstan

#endif  // NEWSTAN_R_OUTPUT_HPP
