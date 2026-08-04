#ifndef STANR_R_METRIC_WRITER_HPP
#define STANR_R_METRIC_WRITER_HPP

#include <stan/callbacks/structured_writer.hpp>
#include <Eigen/Dense>
#include <string>

namespace stanr {

// Captures the "stepsize"/"inv_metric" pair written by
// base_hmc::write_sampler_state_struct() once per chain after warmup.  Runs
// on the worker thread — must not touch the R API (no Rcpp calls, no R
// allocations), only plain C++/Eigen state.
class r_metric_writer : public stan::callbacks::structured_writer {
 public:
  void write(const std::string& key, double value) override {
    if (key == "stepsize") stepsize_ = value;
  }

  void write(const std::string& key, const Eigen::VectorXd& vec) override {
    if (key != "inv_metric") return;
    inv_metric_vector_ = vec;
    is_dense_ = false;
    has_metric_ = true;
  }

  void write(const std::string& key, const Eigen::MatrixXd& mat) override {
    if (key != "inv_metric") return;
    inv_metric_matrix_ = mat;
    is_dense_ = true;
    has_metric_ = true;
  }

  bool has_metric() const { return has_metric_; }
  bool is_dense() const { return is_dense_; }
  double stepsize() const { return stepsize_; }
  const Eigen::VectorXd& inv_metric_vector() const { return inv_metric_vector_; }
  const Eigen::MatrixXd& inv_metric_matrix() const { return inv_metric_matrix_; }

 private:
  double stepsize_ = 0.0;
  bool has_metric_ = false;
  bool is_dense_ = false;
  Eigen::VectorXd inv_metric_vector_;
  Eigen::MatrixXd inv_metric_matrix_;
};

}  // namespace stanr

#endif  // STANR_R_METRIC_WRITER_HPP
