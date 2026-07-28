#ifndef NEWSTAN_R_DATA_CONTEXT_HPP
#define NEWSTAN_R_DATA_CONTEXT_HPP

#include <Rcpp.h>
#include <stan/io/var_context.hpp>
#include <stan/io/validate_dims.hpp>
#include <Eigen/Dense>
#include <map>
#include <vector>
#include <string>

namespace newstan {

/**
 * R list -> stan::io::var_context adapter.
 *
 * Zero-copy: holds a reference to the R list, reads values on demand.
 * Dimensions are cached at construction time.
 *
 * Adapted from rstan::io::rlist_ref_var_context.
 */
class r_data_context : public stan::io::var_context {
 private:
  Rcpp::List list_;
  std::map<std::string, std::vector<size_t>> dims_r_;
  std::map<std::string, std::vector<size_t>> dims_i_;
  std::vector<size_t> empty_vec_ui_;

  bool contains_r_only(const std::string& name) const {
    return dims_r_.count(name) > 0;
  }

 public:
  /**
   * Construct from an R list (named list of numeric/integer vectors).
   */
  explicit r_data_context(Rcpp::List list) : list_(list) {
    if (0 == list_.size()) return;

    std::vector<std::string> varnames =
        Rcpp::as<std::vector<std::string>>(list_.names());

    for (R_xlen_t i = 0; i < list_.size(); i++) {
      SEXP ee = list_[i];
      SEXP dim = Rf_getAttrib(ee, R_DimSymbol);
      R_len_t eelen = Rf_length(ee);

      typedef std::map<std::string, std::vector<size_t>>::value_type psd_v_t;

      if (Rf_isInteger(ee)) {
        if (Rf_length(dim) > 0) {
          std::vector<size_t> d;
          std::vector<unsigned int> dim_u =
              Rcpp::as<std::vector<unsigned int>>(dim);
          for (auto v : dim_u) d.push_back(static_cast<size_t>(v));
          dims_i_.insert(psd_v_t(varnames[i], d));
        } else {
          if (1 == eelen) {
            dims_i_.insert(psd_v_t(varnames[i], empty_vec_ui_));
          } else {
            dims_i_.insert(
                psd_v_t(varnames[i], std::vector<size_t>(1, static_cast<size_t>(eelen))));
          }
        }
      } else if (Rf_isNumeric(ee)) {
        if (Rf_length(dim) > 0) {
          std::vector<size_t> d;
          std::vector<unsigned int> dim_u =
              Rcpp::as<std::vector<unsigned int>>(dim);
          for (auto v : dim_u) d.push_back(static_cast<size_t>(v));
          dims_r_.insert(psd_v_t(varnames[i], d));
        } else {
          if (1 == eelen) {
            dims_r_.insert(psd_v_t(varnames[i], empty_vec_ui_));
          } else {
            dims_r_.insert(
                psd_v_t(varnames[i], std::vector<size_t>(1, static_cast<size_t>(eelen))));
          }
        }
      }
      // else: ignore non-numeric data
    }
  }

  // var_context interface
  bool contains_r(const std::string& name) const override {
    return contains_r_only(name) || contains_i(name);
  }

  std::vector<double> vals_r(const std::string& name) const override {
    if (contains_r(name)) {
      SEXP ee = list_[name.c_str()];
      Rcpp::NumericVector nv = Rcpp::as<Rcpp::NumericVector>(ee);
      return std::vector<double>(nv.begin(), nv.end());
    }
    return std::vector<double>();
  }

  std::vector<std::complex<double>> vals_c(const std::string& name) const override {
    return std::vector<std::complex<double>>();
  }

  std::vector<size_t> dims_r(const std::string& name) const override {
    if (contains_r_only(name)) {
      return dims_r_.find(name)->second;
    } else if (contains_i(name)) {
      return dims_i_.find(name)->second;
    }
    return empty_vec_ui_;
  }

  bool contains_i(const std::string& name) const override {
    return dims_i_.count(name) > 0;
  }

  std::vector<int> vals_i(const std::string& name) const override {
    if (contains_i(name)) {
      SEXP ee = list_[name.c_str()];
      return Rcpp::as<std::vector<int>>(ee);
    }
    return std::vector<int>();
  }

  std::vector<size_t> dims_i(const std::string& name) const override {
    if (contains_i(name)) {
      return dims_i_.find(name)->second;
    }
    return empty_vec_ui_;
  }

  void names_r(std::vector<std::string>& names) const override {
    names.resize(0);
    for (auto it = dims_r_.begin(); it != dims_r_.end(); ++it)
      names.push_back(it->first);
  }

  void names_i(std::vector<std::string>& names) const override {
    names.resize(0);
    for (auto it = dims_i_.begin(); it != dims_i_.end(); ++it)
      names.push_back(it->first);
  }

  void validate_dims(const std::string& stage,
                     const std::string& name,
                     const std::string& base_type,
                     const std::vector<size_t>& dims_declared) const override {
    stan::io::validate_dims(*this, stage, name, base_type, dims_declared);
  }
};

}  // namespace newstan

#endif  // NEWSTAN_R_DATA_CONTEXT_HPP
