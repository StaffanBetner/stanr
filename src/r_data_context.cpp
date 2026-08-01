#include "include/r_data_context.hpp"

namespace newstan {

r_data_context::r_data_context(Rcpp::List list) {
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
      values_.emplace(name, value_entry{std::move(reals), {},
                                        std::move(ints), std::move(dims)});
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
                                  {},
                                  is_integer_valued
                                      ? std::optional<std::vector<int>>(
                                            std::move(ints))
                                      : std::nullopt,
                                  std::move(dims)});
    } else if (Rf_isComplex(value)) {
      Rcpp::ComplexVector input(value);
      std::vector<std::complex<double>> complexes;
      complexes.reserve(input.size());
      for (R_xlen_t j = 0; j < input.size(); ++j) {
        const Rcomplex element = input[j];
        if (!std::isfinite(element.r) || !std::isfinite(element.i)) {
          Rcpp::stop("Complex variable '" + name
                     + "' contains a non-finite value.");
        }
        complexes.emplace_back(element.r, element.i);
      }
      // Stan's var_context convention represents a complex scalar as a
      // final real/imaginary dimension of size two. vals_c() removes that
      // storage dimension and returns one std::complex per logical value.
      dims.push_back(2);
      values_.emplace(name, value_entry{{}, std::move(complexes),
                                        std::nullopt, std::move(dims)});
    } else {
      Rcpp::stop("Variable '" + name
                 + "' must be an integer, numeric, or complex atomic "
                   "vector or array; tuple/list values are not yet "
                   "supported.");
    }
  }
}

bool r_data_context::contains_r(const std::string& name) const {
  return values_.find(name) != values_.end();
}

std::vector<double> r_data_context::vals_r(const std::string& name) const {
  const auto it = values_.find(name);
  return it == values_.end() ? std::vector<double>() : it->second.reals;
}

std::vector<std::complex<double>> r_data_context::vals_c(
    const std::string& name) const {
  const auto it = values_.find(name);
  return it == values_.end() ? std::vector<std::complex<double>>()
                             : it->second.complexes;
}

std::vector<size_t> r_data_context::dims_r(const std::string& name) const {
  const auto it = values_.find(name);
  return it == values_.end() ? std::vector<size_t>() : it->second.dims;
}

bool r_data_context::contains_i(const std::string& name) const {
  const auto it = values_.find(name);
  return it != values_.end() && it->second.ints.has_value();
}

std::vector<int> r_data_context::vals_i(const std::string& name) const {
  const auto it = values_.find(name);
  return it == values_.end() || !it->second.ints ? std::vector<int>()
                                                 : *it->second.ints;
}

std::vector<size_t> r_data_context::dims_i(const std::string& name) const {
  const auto it = values_.find(name);
  return it == values_.end() ? std::vector<size_t>() : it->second.dims;
}

void r_data_context::names_r(std::vector<std::string>& names) const {
  names.clear();
  names.reserve(values_.size());
  for (const auto& entry : values_) names.push_back(entry.first);
}

void r_data_context::names_i(std::vector<std::string>& names) const {
  names.clear();
  names.reserve(values_.size());
  for (const auto& entry : values_) {
    if (entry.second.ints) names.push_back(entry.first);
  }
}

void r_data_context::validate_dims(const std::string& stage,
                                   const std::string& name,
                                   const std::string& base_type,
                                   const std::vector<size_t>& dims_declared) const {
  stan::io::validate_dims(*this, stage, name, base_type, dims_declared);
}

}  // namespace newstan
