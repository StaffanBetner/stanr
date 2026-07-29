#ifndef NEWSTAN_R_OUTPUT_HPP
#define NEWSTAN_R_OUTPUT_HPP

#include <Rcpp.h>
#include <stan/callbacks/logger.hpp>
#include <stan/callbacks/writer.hpp>
#include <stan/callbacks/structured_writer.hpp>
#include <Eigen/Dense>
#include <vector>
#include <string>
#include <sstream>

namespace newstan {

// ===================================================================
// Sample writer — collects parameter samples using plain C++ storage
// to avoid thread-unsafe R calls during parallel (multi-chain) sampling.
// ===================================================================

class r_sample_writer : public stan::callbacks::writer {
 private:
  // Plain C++ storage — safe to access from multiple TBB threads
  std::vector<std::string> colnames_;
  std::vector<std::vector<double>> values_;  // column-major: values_[col][row]
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
    values_.resize(n_cols_);
    // Pre-allocate each column to avoid repeated reallocations during sampling
    for (int j = 0; j < n_cols_; ++j) {
      values_[j].reserve(static_cast<size_t>(expected_rows_));
    }
    initialized_ = true;
  }

  void operator()(const std::vector<double>& state) override {
    if (!initialized_) return;
    int n = static_cast<int>(state.size());
    if (n != n_cols_) return;  // Skip rows that don't match

    // Append values to each column
    for (int j = 0; j < n_cols_; ++j) {
      values_[j].push_back(state[j]);
    }
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
   * Convert collected data to an R data.frame.
   *
   * MUST be called from the main R thread (not from a TBB worker thread).
   * This is where we create R objects from our plain C++ storage.
   */
  Rcpp::DataFrame to_dataframe() const {
    if (!initialized_ || n_rows_ == 0) {
      return Rcpp::DataFrame::create();
    }

    Rcpp::List df_list(n_cols_);
    for (int j = 0; j < n_cols_; ++j) {
      Rcpp::NumericVector col(n_rows_);
      for (int k = 0; k < n_rows_; ++k) {
        col[k] = values_[j][k];
      }
      df_list[j] = col;
    }

    Rcpp::DataFrame df = Rcpp::DataFrame(df_list);
    df.names() = Rcpp::CharacterVector(colnames_.begin(), colnames_.end());
    return df;
  }

  /**
   * Convert collected data to an Eigen matrix (rows = samples, cols = parameters).
   * Data is stored column-major internally, so we transpose when building the matrix.
   */
  Eigen::MatrixXd to_matrix() const {
    Eigen::MatrixXd mat(n_rows_, n_cols_);
    for (int j = 0; j < n_cols_; ++j) {
      for (int k = 0; k < n_rows_; ++k) {
        mat(k, j) = values_[j][k];
      }
    }
    return mat;
  }

  const std::vector<std::string>& colnames() const { return colnames_; }
  int n_rows() const { return n_rows_; }
  int n_cols() const { return n_cols_; }
};

// ===================================================================
// Diagnostic writer — collects diagnostic columns (energy__, divergent__, etc.)
// Uses plain C++ storage to avoid thread-unsafe R calls during parallel sampling.
// ===================================================================

class r_diagnostic_writer : public stan::callbacks::writer {
 private:
  // Plain C++ storage — safe to access from multiple TBB threads
  std::vector<std::string> colnames_;
  std::vector<std::vector<double>> values_;  // column-major: values_[col][row]
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
    values_.resize(n_cols_);
    // Pre-allocate each column to avoid repeated reallocations during sampling
    for (int j = 0; j < n_cols_; ++j) {
      values_[j].reserve(static_cast<size_t>(expected_rows_));
    }
    initialized_ = true;
  }

  void operator()(const std::vector<double>& state) override {
    if (!initialized_) return;
    int n = static_cast<int>(state.size());
    if (n != n_cols_) return;

    for (int j = 0; j < n_cols_; ++j) {
      values_[j].push_back(state[j]);
    }
    n_rows_++;
  }

  void operator()() override {}

  void operator()(const std::string& message) override {}

  void operator()(const Eigen::MatrixXd& values) override {}

  Rcpp::DataFrame to_dataframe() const {
    if (!initialized_ || n_rows_ == 0) return Rcpp::DataFrame::create();

    Rcpp::List df_list(n_cols_);
    for (int j = 0; j < n_cols_; ++j) {
      Rcpp::NumericVector col(n_rows_);
      for (int k = 0; k < n_rows_; ++k) {
        col[k] = values_[j][k];
      }
      df_list[j] = col;
    }

    Rcpp::DataFrame df = Rcpp::DataFrame(df_list);
    df.names() = Rcpp::CharacterVector(colnames_.begin(), colnames_.end());
    return df;
  }

  Eigen::MatrixXd to_matrix() const {
    Eigen::MatrixXd mat(n_rows_, n_cols_);
    for (int j = 0; j < n_cols_; ++j) {
      for (int k = 0; k < n_rows_; ++k) {
        mat(k, j) = values_[j][k];
      }
    }
    return mat;
  }

  const std::vector<std::string>& colnames() const { return colnames_; }
  const std::vector<double>& values_col(int j) const { return values_[j]; }
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
