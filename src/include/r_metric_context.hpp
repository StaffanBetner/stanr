#ifndef NEWSTAN_R_METRIC_CONTEXT_HPP
#define NEWSTAN_R_METRIC_CONTEXT_HPP

#include <stan/io/validate_dims.hpp>
#include <stan/io/var_context.hpp>

#include <complex>
#include <string>
#include <utility>
#include <vector>

namespace newstan {

// A minimal immutable context for an inverse metric.  Unlike
// stan::io::array_var_context, this takes ownership of the already-copied R
// data instead of copying it into a map a second time.
class r_metric_context : public stan::io::var_context {
 private:
  std::vector<double> values_;
  std::vector<size_t> dims_;

 public:
  r_metric_context(std::vector<double> values, std::vector<size_t> dims)
      : values_(std::move(values)), dims_(std::move(dims)) {}

  bool contains_r(const std::string& name) const override {
    return name == "inv_metric";
  }
  std::vector<double> vals_r(const std::string& name) const override {
    return contains_r(name) ? values_ : std::vector<double>{};
  }
  std::vector<std::complex<double>> vals_c(const std::string&) const override {
    return {};
  }
  std::vector<size_t> dims_r(const std::string& name) const override {
    return contains_r(name) ? dims_ : std::vector<size_t>{};
  }
  bool contains_i(const std::string&) const override { return false; }
  std::vector<int> vals_i(const std::string&) const override { return {}; }
  std::vector<size_t> dims_i(const std::string&) const override { return {}; }
  void names_r(std::vector<std::string>& names) const override {
    names.assign(1, "inv_metric");
  }
  void names_i(std::vector<std::string>& names) const override { names.clear(); }
  void validate_dims(const std::string& stage, const std::string& name,
                     const std::string& base_type,
                     const std::vector<size_t>& dims_declared) const override {
    stan::io::validate_dims(*this, stage, name, base_type, dims_declared);
  }
};

}  // namespace newstan

#endif  // NEWSTAN_R_METRIC_CONTEXT_HPP
