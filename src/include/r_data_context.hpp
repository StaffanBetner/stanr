#ifndef NEWSTAN_R_DATA_CONTEXT_HPP
#define NEWSTAN_R_DATA_CONTEXT_HPP

#include <Rcpp.h>
#include <stan/io/var_context.hpp>
#include <stan/io/validate_dims.hpp>
#include <Eigen/Dense>
#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <vector>
#include <string>

namespace newstan {

/**
 * R list -> stan::io::var_context adapter.
 *
 * Values and dimensions are copied at construction time so methods can be
 * called safely from Stan's TBB worker threads without touching the R API.
 *
 * Adapted from rstan::io::rlist_ref_var_context.
 */
class r_data_context : public stan::io::var_context {
 private:
  std::map<std::string, std::vector<double>> vals_r_;
  std::map<std::string, std::vector<int>> vals_i_;
  std::map<std::string, std::vector<size_t>> dims_;
  std::vector<size_t> empty_vec_ui_;

 public:
  /**
   * Construct from an R list (named list of numeric/integer vectors).
   */
  explicit r_data_context(Rcpp::List list) {
    if (list.size() == 0) return;
    if (Rf_isNull(list.names())) {
      Rcpp::stop("Stan data and initialization lists must be named.");
    }

    std::vector<std::string> varnames =
        Rcpp::as<std::vector<std::string>>(list.names());
    std::set<std::string> seen_names;

    for (R_xlen_t i = 0; i < list.size(); ++i) {
      const std::string& name = varnames[i];
      if (name.empty()) {
        Rcpp::stop("Stan data and initialization lists cannot contain empty names.");
      }
      if (!seen_names.insert(name).second) {
        Rcpp::stop("Stan data and initialization lists cannot contain duplicate names.");
      }

      SEXP ee = list[i];
      SEXP dim = Rf_getAttrib(ee, R_DimSymbol);
      std::vector<size_t> dims;
      if (Rf_length(dim) > 0) {
        Rcpp::IntegerVector dim_i(dim);
        dims.reserve(dim_i.size());
        for (R_xlen_t j = 0; j < dim_i.size(); ++j) {
          if (dim_i[j] == NA_INTEGER || dim_i[j] < 0) {
            Rcpp::stop("Invalid dimensions for variable '" + name + "'.");
          }
          dims.push_back(static_cast<size_t>(dim_i[j]));
        }
      } else if (Rf_xlength(ee) != 1) {
        dims.push_back(static_cast<size_t>(Rf_xlength(ee)));
      }

      if (Rf_isInteger(ee)) {
        Rcpp::IntegerVector input(ee);
        std::vector<int> ints(input.size());
        std::vector<double> reals(input.size());
        for (R_xlen_t j = 0; j < input.size(); ++j) {
          if (input[j] == NA_INTEGER) {
            Rcpp::stop("Integer variable '" + name + "' contains NA.");
          }
          ints[j] = input[j];
          reals[j] = static_cast<double>(input[j]);
        }
        vals_i_.emplace(name, std::move(ints));
        vals_r_.emplace(name, std::move(reals));
        dims_.emplace(name, std::move(dims));
      } else if (Rf_isNumeric(ee)) {
        Rcpp::NumericVector input(ee);
        std::vector<double> reals(input.begin(), input.end());
        std::vector<int> ints;
        ints.reserve(input.size());
        bool is_integer_valued = true;
        for (R_xlen_t j = 0; j < input.size(); ++j) {
          const double value = input[j];
          if (!std::isfinite(value) || std::trunc(value) != value
              || value < static_cast<double>(std::numeric_limits<int>::min())
              || value > static_cast<double>(std::numeric_limits<int>::max())) {
            is_integer_valued = false;
            break;
          }
          ints.push_back(static_cast<int>(value));
        }
        vals_r_.emplace(name, std::move(reals));
        if (is_integer_valued) {
          vals_i_.emplace(name, std::move(ints));
        }
        dims_.emplace(name, std::move(dims));
      }
      // else: ignore non-numeric data
    }
  }

  // var_context interface
  bool contains_r(const std::string& name) const override {
    return vals_r_.count(name) > 0;
  }

  std::vector<double> vals_r(const std::string& name) const override {
    const auto it = vals_r_.find(name);
    if (it != vals_r_.end()) {
      return it->second;
    }
    return std::vector<double>();
  }

  std::vector<std::complex<double>> vals_c(const std::string& name) const override {
    return std::vector<std::complex<double>>();
  }

  std::vector<size_t> dims_r(const std::string& name) const override {
    const auto it = dims_.find(name);
    if (it != dims_.end()) {
      return it->second;
    }
    return empty_vec_ui_;
  }

  bool contains_i(const std::string& name) const override {
    return vals_i_.count(name) > 0;
  }

  std::vector<int> vals_i(const std::string& name) const override {
    const auto it = vals_i_.find(name);
    if (it != vals_i_.end()) {
      return it->second;
    }
    return std::vector<int>();
  }

  std::vector<size_t> dims_i(const std::string& name) const override {
    const auto it = dims_.find(name);
    if (it != dims_.end()) {
      return it->second;
    }
    return empty_vec_ui_;
  }

  void names_r(std::vector<std::string>& names) const override {
    names.resize(0);
    for (auto it = vals_r_.begin(); it != vals_r_.end(); ++it)
      names.push_back(it->first);
  }

  void names_i(std::vector<std::string>& names) const override {
    names.resize(0);
    for (auto it = vals_i_.begin(); it != vals_i_.end(); ++it)
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
