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
      for (R_xlen_t j = 0; j < input.size(); ++j) {
        if (input[j] == NA_INTEGER) {
          Rcpp::stop("Integer variable '" + name + "' contains NA.");
        }
        ints[j] = input[j];
      }
      values_.emplace(name, value_entry{{}, {},
                                        std::move(ints), std::move(dims)});
    } else if (Rf_isNumeric(value)) {
      Rcpp::NumericVector input(value);
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
      if (is_integer_valued) {
        values_.emplace(name, value_entry{{}, {}, std::move(ints),
                                          std::move(dims)});
      } else {
        std::vector<double> reals(input.begin(), input.end());
        values_.emplace(name, value_entry{std::move(reals), {}, std::nullopt,
                                          std::move(dims)});
      }
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
      // `dims` here is the pre-trailing-2 vector: enclosing array dims (for
      // a dotted tuple-slot leaf) or the plain declared dims (top-level).
      const size_t pre_trailing_dims = dims.size();
      // Stan's var_context convention represents a complex scalar as a
      // final real/imaginary dimension of size two. vals_c() removes that
      // storage dimension and returns one std::complex per logical value.
      dims.push_back(2);

      if (name.find('.') == std::string::npos) {
        // Top-level (undotted) complex entry: today's dense storage.
        values_.emplace(name, value_entry{{}, std::move(complexes),
                                          std::nullopt, std::move(dims)});
      } else {
        // Tuple-slot complex leaf (dotted name; Part B/A4 of the
        // complex-tuple interop plan): stanc 2.39's generated reader
        // consumes vals_c() for a tuple-slot complex leaf in per-enclosing-
        // array-element windows that are only half used. Build that
        // windowed layout here so the dotted flat vector produced by the R
        // flattener (Part B) lines up with what the generated reader
        // actually indexes.
        //
        // k = count of enclosing array dims (read from the
        // `newstan_array_dims` attribute the R flattener attaches; default
        // to all pre-trailing dims -- m = 1 -- when absent, which is the
        // manual dotted-name escape hatch's documented limitation).
        size_t k = pre_trailing_dims;
        SEXP array_dims_attr
            = Rf_getAttrib(value, Rf_install("newstan_array_dims"));
        if (!Rf_isNull(array_dims_attr)) {
          if (Rf_length(array_dims_attr) != 1
              || !(Rf_isInteger(array_dims_attr)
                   || Rf_isReal(array_dims_attr))) {
            Rcpp::stop("'newstan_array_dims' attribute for variable '" + name
                       + "' must be a length-1 integer or double.");
          }
          const double k_val = Rf_isInteger(array_dims_attr)
              ? static_cast<double>(INTEGER(array_dims_attr)[0])
              : REAL(array_dims_attr)[0];
          if (!std::isfinite(k_val) || k_val < 0
              || std::trunc(k_val) != k_val) {
            Rcpp::stop("Invalid 'newstan_array_dims' attribute for variable '"
                       + name + "'.");
          }
          k = static_cast<size_t>(k_val);
        }
        if (k > pre_trailing_dims) {
          Rcpp::stop("'newstan_array_dims' attribute for variable '" + name
                     + "' exceeds its declared dimensions.");
        }

        size_t enclosing_elements = 1;
        for (size_t d = 0; d < k; ++d) enclosing_elements *= dims[d];
        if (enclosing_elements == 0) {
          Rcpp::stop("Variable '" + name
                     + "' has a zero-size enclosing array dimension.");
        }
        if (complexes.size() % enclosing_elements != 0) {
          Rcpp::stop(
              "Variable '" + name + "' has " + std::to_string(complexes.size())
              + " complex value(s), not divisible by the "
              + std::to_string(enclosing_elements) + " enclosing array "
                "element(s) implied by its 'newstan_array_dims' attribute.");
        }
        const size_t m = complexes.size() / enclosing_elements;
        const size_t n = enclosing_elements;
        std::vector<std::complex<double>> padded(2 * m * n);
        for (size_t e = 0; e < n; ++e) {
          for (size_t j = 0; j < m; ++j) {
            padded[2 * m * e + j] = complexes[m * e + j];
          }
        }
        values_.emplace(name, value_entry{{}, std::move(padded),
                                          std::nullopt, std::move(dims)});
      }
    } else {
      Rcpp::stop("Variable '" + name
                 + "' must be an integer, numeric, or complex atomic "
                   "vector or array. A list value here means R-side tuple "
                   "flattening was bypassed, or '" + name
                 + "' is not declared as a tuple.");
    }
  }
}

bool r_data_context::contains_r(const std::string& name) const {
  return values_.find(name) != values_.end();
}

std::vector<double> r_data_context::vals_r(const std::string& name) const {
  const auto it = values_.find(name);
  if (it == values_.end()) return std::vector<double>();
  if (it->second.ints) {
    const auto& ints = *it->second.ints;
    return std::vector<double>(ints.begin(), ints.end());
  }
  return it->second.reals;
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
