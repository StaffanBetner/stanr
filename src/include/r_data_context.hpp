#ifndef NEWSTAN_R_DATA_CONTEXT_HPP
#define NEWSTAN_R_DATA_CONTEXT_HPP

#include <Rcpp.h>
#include <stan/io/validate_dims.hpp>
#include <stan/io/var_context.hpp>

#include <cmath>
#include <limits>
#include <map>
#include <optional>
#include <ostream>
#include <set>
#include <streambuf>
#include <string>
#include <utility>
#include <vector>

namespace newstan {

// Generated Stan models retain their constructor's ostream pointer and can
// write through it while evaluating on a native/TBB worker.  This sink has
// static lifetime and deliberately bypasses R's console API.
class null_streambuf : public std::streambuf {
 protected:
  int_type overflow(int_type ch) override { return traits_type::not_eof(ch); }
  std::streamsize xsputn(const char*, std::streamsize n) override { return n; }
};

inline std::ostream& worker_safe_stream() {
  static null_streambuf buffer;
  static std::ostream stream(&buffer);
  return stream;
}

// R list -> stan::io::var_context adapter. Values are copied during
// construction, so Stan worker threads never access the R API.
class r_data_context : public stan::io::var_context {
 private:
  struct value_entry {
    std::vector<double> reals;
    std::optional<std::vector<int>> ints;
    std::vector<size_t> dims;
  };

  // Keep all data associated with a variable in one node.  This stores its
  // name once instead of once per real, integer, and dimension map.
  std::map<std::string, value_entry> values_;

 public:
  explicit r_data_context(Rcpp::List list) {
    if (list.size() == 0) return;
    if (Rf_isNull(list.names())) {
      Rcpp::stop("Stan data and initialization lists must be named.");
    }

    const auto varnames = Rcpp::as<std::vector<std::string>>(list.names());
    std::set<std::string> seen_names;
    for (R_xlen_t i = 0; i < list.size(); ++i) {
      const std::string& name = varnames[i];
      if (name.empty()) {
        Rcpp::stop("Stan data and initialization lists cannot contain empty names.");
      }
      if (!seen_names.insert(name).second) {
        Rcpp::stop("Stan data and initialization lists cannot contain duplicate names.");
      }

      SEXP value = list[i];
      SEXP dim = Rf_getAttrib(value, R_DimSymbol);
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
      } else if (Rf_xlength(value) != 1) {
        dims.push_back(static_cast<size_t>(Rf_xlength(value)));
      }

      if (Rf_isInteger(value)) {
        Rcpp::IntegerVector input(value);
        std::vector<int> ints(input.size());
        std::vector<double> reals(input.size());
        for (R_xlen_t j = 0; j < input.size(); ++j) {
          if (input[j] == NA_INTEGER) {
            Rcpp::stop("Integer variable '" + name + "' contains NA.");
          }
          ints[j] = input[j];
          reals[j] = static_cast<double>(input[j]);
        }
        values_.emplace(name, value_entry{std::move(reals), std::move(ints),
                                          std::move(dims)});
      } else if (Rf_isNumeric(value)) {
        Rcpp::NumericVector input(value);
        std::vector<double> reals(input.begin(), input.end());
        std::vector<int> ints;
        ints.reserve(input.size());
        bool is_integer_valued = true;
        for (R_xlen_t j = 0; j < input.size(); ++j) {
          const double element = input[j];
          if (!std::isfinite(element) || std::trunc(element) != element
              || element < static_cast<double>(std::numeric_limits<int>::min())
              || element > static_cast<double>(std::numeric_limits<int>::max())) {
            is_integer_valued = false;
            break;
          }
          ints.push_back(static_cast<int>(element));
        }
        values_.emplace(name,
                        value_entry{std::move(reals),
                                    is_integer_valued
                                        ? std::optional<std::vector<int>>(
                                              std::move(ints))
                                        : std::nullopt,
                                    std::move(dims)});
      }
    }
  }

  bool contains_r(const std::string& name) const override {
    return values_.contains(name);
  }
  std::vector<double> vals_r(const std::string& name) const override {
    const auto it = values_.find(name);
    return it == values_.end() ? std::vector<double>() : it->second.reals;
  }
  std::vector<std::complex<double>> vals_c(const std::string&) const override {
    return {};
  }
  std::vector<size_t> dims_r(const std::string& name) const override {
    const auto it = values_.find(name);
    return it == values_.end() ? std::vector<size_t>() : it->second.dims;
  }
  bool contains_i(const std::string& name) const override {
    const auto it = values_.find(name);
    return it != values_.end() && it->second.ints.has_value();
  }
  std::vector<int> vals_i(const std::string& name) const override {
    const auto it = values_.find(name);
    return it == values_.end() || !it->second.ints ? std::vector<int>()
                                                   : *it->second.ints;
  }
  std::vector<size_t> dims_i(const std::string& name) const override {
    const auto it = values_.find(name);
    return it == values_.end() ? std::vector<size_t>() : it->second.dims;
  }
  void names_r(std::vector<std::string>& names) const override {
    names.clear();
    names.reserve(values_.size());
    for (const auto& entry : values_) names.push_back(entry.first);
  }
  void names_i(std::vector<std::string>& names) const override {
    names.clear();
    names.reserve(values_.size());
    for (const auto& entry : values_) {
      if (entry.second.ints) names.push_back(entry.first);
    }
  }
  void validate_dims(const std::string& stage, const std::string& name,
                     const std::string& base_type,
                     const std::vector<size_t>& dims_declared) const override {
    stan::io::validate_dims(*this, stage, name, base_type, dims_declared);
  }
};

}  // namespace newstan

#endif  // NEWSTAN_R_DATA_CONTEXT_HPP
