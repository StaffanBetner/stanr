#include "include/r_data_context.hpp"
#include "include/tuple_declarations.hpp"

#include <numeric>

namespace stanr {

namespace {

bool has_nonempty_names(SEXP x) {
  SEXP names = Rf_getAttrib(x, R_NamesSymbol);
  if (Rf_isNull(names)) return false;
  for (R_xlen_t i = 0; i < Rf_xlength(names); ++i) {
    SEXP name = STRING_ELT(names, i);
    if (name != NA_STRING && CHAR(name)[0] != '\0') return true;
  }
  return false;
}

// The "own shape" of one leaf payload: its `dim` attribute if present,
// otherwise its length if > 1, otherwise empty (a bare scalar).
std::vector<int> payload_shape(SEXP x) {
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_length(dim) > 0) return Rcpp::as<std::vector<int>>(dim);
  if (Rf_xlength(x) > 1) return {static_cast<int>(Rf_xlength(x))};
  return {};
}

std::string shape_string(const std::vector<int>& shape) {
  if (shape.empty()) return "scalar";
  std::string out;
  for (size_t i = 0; i < shape.size(); ++i) {
    if (i > 0) out += "x";
    out += std::to_string(shape[i]);
  }
  return out;
}

size_t shape_product(const std::vector<int>& shape) {
  return std::accumulate(
      shape.begin(), shape.end(), size_t{1},
      [](size_t acc, int d) { return acc * static_cast<size_t>(d); });
}

// Coerces each data.frame column to RTYPE and lays it out column-major.
template <int RTYPE>
SEXP columns_to_matrix(Rcpp::List columns, int n_rows, int n_columns) {
  Rcpp::Matrix<RTYPE> out(n_rows, n_columns);
  for (int c = 0; c < n_columns; ++c) {
    Rcpp::Vector<RTYPE> column(Rf_coerceVector(columns[c], RTYPE));
    for (int r = 0; r < n_rows; ++r) out(r, c) = column[r];
  }
  return out;
}

// Validate that `value` is an unnamed list nested `k` levels deep (one level
// per enclosing array dimension, outermost first) and return the array's
// size at each level. Errors on named/non-list values and on non-rectangular
// arrays.
std::vector<int> tuple_array_shape(const std::string& name, SEXP value,
                                   int k) {
  if (TYPEOF(value) != VECSXP || Rf_inherits(value, "data.frame")
      || has_nonempty_names(value)) {
    Rcpp::stop("`%s` must be an unnamed list (%d %s deep, matching its "
               "declared array dimensions); see the tuple data shape "
               "documentation.",
               name, k, k == 1 ? "level" : "levels");
  }
  const int d1 = Rf_length(value);
  if (k == 1) return {d1};
  std::vector<int> sizes;
  for (int i = 0; i < d1; ++i) {
    std::vector<int> shape_i = tuple_array_shape(
        name + "[[" + std::to_string(i + 1) + "]]", VECTOR_ELT(value, i),
        k - 1);
    if (i == 0) {
      sizes = shape_i;
    } else if (shape_i != sizes) {
      Rcpp::stop("`%s` is not a rectangular tuple array: element %d has "
                 "shape %s but a previous element has shape %s.",
                 name, i + 1, shape_string(shape_i), shape_string(sizes));
    }
  }
  sizes.insert(sizes.begin(), d1);
  return sizes;
}

// Flatten a nested list (already validated by tuple_array_shape(), with
// per-level sizes `sizes`) into a flat vector of elements enumerated
// column-major -- first (outermost) index fastest.
std::vector<SEXP> enumerate_elements(SEXP value,
                                     const std::vector<int>& sizes) {
  if (sizes.empty()) return {value};
  const int d1 = sizes[0];
  if (sizes.size() == 1) {
    std::vector<SEXP> out(d1);
    for (int i = 0; i < d1; ++i) out[i] = VECTOR_ELT(value, i);
    return out;
  }
  const std::vector<int> rest(sizes.begin() + 1, sizes.end());
  std::vector<std::vector<SEXP>> sub(d1);
  for (int i = 0; i < d1; ++i) {
    sub[i] = enumerate_elements(VECTOR_ELT(value, i), rest);
  }
  const size_t rest_n = shape_product(rest);
  std::vector<SEXP> out;
  out.reserve(d1 * rest_n);
  for (size_t r = 0; r < rest_n; ++r) {
    for (int i = 0; i < d1; ++i) out.push_back(sub[i][r]);
  }
  return out;
}

}  // namespace

r_data_context::r_data_context(Rcpp::List list, SEXP declarations) {
  if (list.size() == 0) return;
  if (Rf_isNull(list.names())) {
    Rcpp::stop("Stan data and initialization lists must be named.");
  }

  const auto varnames = Rcpp::as<std::vector<std::string>>(list.names());
  for (const std::string& name : varnames) {
    if (name.empty()) {
      Rcpp::stop(
          "Stan data and initialization lists cannot contain empty names.");
    }
    if (!reserved_names_.insert(name).second) {
      Rcpp::stop(
          "Stan data and initialization lists cannot contain duplicate names.");
    }
  }

  for (R_xlen_t i = 0; i < list.size(); ++i) {
    const std::string& name = varnames[i];
    SEXP value = list[i];

    if (Rf_inherits(value, "data.frame")) {
      // Accept data.frames as matrix-like (cmdstanr-style) when every
      // column survives as.matrix() numerically.
      Rcpp::List columns(value);
      const int n_columns = columns.size();
      const int n_rows = n_columns > 0 ? Rf_length(columns[0]) : 0;
      bool any_complex = false;
      bool all_integer = true;
      for (int c = 0; c < n_columns; ++c) {
        switch (TYPEOF(columns[c])) {
          case INTSXP:
            break;
          case REALSXP:
            all_integer = false;
            break;
          case CPLXSXP:
            any_complex = true;
            all_integer = false;
            break;
          default:
            Rcpp::stop("`%s` is a data.frame with a non-numeric, non-complex "
                       "column; data.frames are only accepted as data when "
                       "every column is numeric or complex -- supply a "
                       "matrix instead.",
                       name);
        }
      }
      const SEXP matrix = any_complex
          ? columns_to_matrix<CPLXSXP>(columns, n_rows, n_columns)
          : all_integer ? columns_to_matrix<INTSXP>(columns, n_rows, n_columns)
                        : columns_to_matrix<REALSXP>(columns, n_rows,
                                                      n_columns);
      add_value(name, matrix);
    } else if (TYPEOF(value) == VECSXP) {
      SEXP decl = R_NilValue;
      if (!Rf_isNull(declarations)) {
        Rcpp::List declaration_list(declarations);
        if (declaration_list.containsElementNamed(name.c_str())) {
          decl = declaration_list[name];
        }
      }
      SEXP decl_type =
          Rf_isNull(decl) ? R_NilValue : Rcpp::List(decl)["type"];
      if (Rf_isNull(decl) || !Rf_inherits(decl_type, "data.frame")) {
        Rcpp::stop("`%s` is not declared as a tuple; lists are only accepted "
                   "for tuple variables.",
                   name);
      }
      flatten_tuple(name, value, decl_type,
                    decl_int(Rcpp::List(decl)["dimensions"], 0));
    } else {
      add_value(name, value);
    }
  }
}

// Stores one atomic (non-list) value, including user-supplied dotted tuple
// leaves (the manual escape hatch).
void r_data_context::add_value(const std::string& name, SEXP value) {
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
    values_.emplace(name,
                    value_entry{{}, {}, std::move(ints), std::move(dims)});
  } else if (Rf_isNumeric(value)) {
    Rcpp::NumericVector input(value);
    store_numeric(name, std::vector<double>(input.begin(), input.end()),
                  std::move(dims));
  } else if (Rf_isComplex(value)) {
    Rcpp::ComplexVector input(value);
    std::vector<std::complex<double>> complexes;
    complexes.reserve(input.size());
    for (R_xlen_t j = 0; j < input.size(); ++j) {
      complexes.emplace_back(input[j].r, input[j].i);
    }
    // A dotted name is a manually-supplied tuple-slot leaf; without the
    // declared structure, every pre-trailing dim is treated as enclosing
    // (m = 1), the escape hatch's documented limitation.
    const size_t enclosing = name.find('.') != std::string::npos
        ? dims.size()
        : std::numeric_limits<size_t>::max();
    store_complex(name, std::move(complexes), std::move(dims), enclosing);
  } else {
    Rcpp::stop("Variable '" + name
               + "' must be an integer, numeric, or complex atomic "
                 "vector or array, or a tuple value as an unnamed list.");
  }
}

// Classifies an all-double payload as int (every value integer-valued and
// in range) or real, with the NA checks each kind requires.
void r_data_context::store_numeric(const std::string& name,
                                   std::vector<double> values,
                                   std::vector<size_t> dims) {
  bool is_integer_valued = true;
  std::vector<int> ints;
  ints.reserve(values.size());
  for (const double element : values) {
    if (!std::isfinite(element) || std::trunc(element) != element
        || element < static_cast<double>(std::numeric_limits<int>::min())
        || element > static_cast<double>(std::numeric_limits<int>::max())) {
      is_integer_valued = false;
      break;
    }
    ints.push_back(static_cast<int>(element));
  }
  if (is_integer_valued) {
    values_.emplace(name,
                    value_entry{{}, {}, std::move(ints), std::move(dims)});
    return;
  }
  for (const double element : values) {
    if (std::isnan(element)) {
      Rcpp::stop("Real variable '" + name + "' contains NA or NaN.");
    }
  }
  values_.emplace(name, value_entry{std::move(values), {}, std::nullopt,
                                    std::move(dims)});
}

// `dims` excludes the trailing complex storage dimension of size two, which
// is appended here. `enclosing_dims` is the count of enclosing array dims
// (SIZE_MAX for a top-level variable, stored dense): stanc 2.39's generated
// reader consumes vals_c() for a tuple-slot complex leaf in
// per-enclosing-array-element windows of size 2m that are only half used,
// so that windowed layout is built here.
void r_data_context::store_complex(const std::string& name,
                                   std::vector<std::complex<double>> values,
                                   std::vector<size_t> dims,
                                   size_t enclosing_dims) {
  for (const std::complex<double>& element : values) {
    if (std::isnan(element.real()) || std::isnan(element.imag())) {
      Rcpp::stop("Complex variable '" + name + "' contains NA or NaN.");
    }
  }
  const bool windowed = enclosing_dims != std::numeric_limits<size_t>::max();
  dims.push_back(2);
  if (!windowed) {
    values_.emplace(name, value_entry{{}, std::move(values), std::nullopt,
                                      std::move(dims)});
    return;
  }
  size_t enclosing_elements = 1;
  for (size_t d = 0; d < enclosing_dims; ++d) enclosing_elements *= dims[d];
  if (enclosing_elements == 0 || values.size() % enclosing_elements != 0) {
    Rcpp::stop("Variable '" + name
               + "' has complex values inconsistent with its enclosing "
                 "array dimensions.");
  }
  const size_t m = values.size() / enclosing_elements;
  std::vector<std::complex<double>> padded(2 * m * enclosing_elements);
  for (size_t e = 0; e < enclosing_elements; ++e) {
    for (size_t j = 0; j < m; ++j) {
      padded[2 * m * e + j] = values[m * e + j];
    }
  }
  values_.emplace(name, value_entry{{}, std::move(padded), std::nullopt,
                                    std::move(dims)});
}

void r_data_context::flatten_tuple(const std::string& name, SEXP value,
                                   Rcpp::List type_df, int n_array_dims) {
  std::vector<int> array_sizes;
  std::vector<SEXP> elements;
  if (n_array_dims == 0) {
    elements = {value};
  } else {
    array_sizes = tuple_array_shape(name, value, n_array_dims);
    elements = enumerate_elements(value, array_sizes);
  }
  flatten_recurse(name, elements, array_sizes, type_df);
}

void r_data_context::flatten_recurse(const std::string& name,
                                     const std::vector<SEXP>& elements,
                                     const std::vector<int>& array_sizes,
                                     Rcpp::List type_df) {
  const int n_slots = tuple_slot_count(type_df);
  for (SEXP element : elements) {
    if (TYPEOF(element) != VECSXP || Rf_inherits(element, "data.frame")
        || has_nonempty_names(element) || Rf_length(element) != n_slots) {
      Rcpp::stop("`%s` must be an unnamed list of length %d (one entry per "
                 "tuple slot); see the tuple data shape documentation.",
                 name, n_slots);
    }
  }
  for (int s = 0; s < n_slots; ++s) {
    const std::string slot_name = name + "." + std::to_string(s + 1);
    std::vector<SEXP> slot_values;
    slot_values.reserve(elements.size());
    for (SEXP element : elements) {
      slot_values.push_back(VECTOR_ELT(element, s));
    }
    const slot_decl slot = tuple_slot(type_df, s);
    if (slot.is_tuple) {
      if (slot.dims == 0) {
        flatten_recurse(slot_name, slot_values, array_sizes, slot.tuple_df);
        continue;
      }
      // This slot is itself an array of tuples: enumerate each enclosing
      // element's inner array (shapes must agree) and concatenate in outer
      // order, which preserves blocked storage.
      std::vector<int> inner_sizes;
      std::vector<SEXP> new_elements;
      for (size_t i = 0; i < slot_values.size(); ++i) {
        std::vector<int> shape_i = tuple_array_shape(
            slot_name + "[" + std::to_string(i + 1) + "]", slot_values[i],
            slot.dims);
        if (i == 0) {
          inner_sizes = shape_i;
        } else if (shape_i != inner_sizes) {
          Rcpp::stop("`%s` is not a rectangular tuple array across enclosing "
                     "elements: element %d has shape %s but a previous "
                     "element has shape %s.",
                     slot_name, static_cast<int>(i + 1),
                     shape_string(shape_i), shape_string(inner_sizes));
        }
        std::vector<SEXP> inner = enumerate_elements(slot_values[i],
                                                     inner_sizes);
        new_elements.insert(new_elements.end(), inner.begin(), inner.end());
      }
      std::vector<int> new_array_sizes = array_sizes;
      new_array_sizes.insert(new_array_sizes.end(), inner_sizes.begin(),
                             inner_sizes.end());
      flatten_recurse(slot_name, new_elements, new_array_sizes,
                      slot.tuple_df);
    } else {
      flatten_leaf(slot_name, slot_values, slot.kind, array_sizes);
    }
  }
}

void r_data_context::flatten_leaf(const std::string& name,
                                  const std::vector<SEXP>& slot_values,
                                  const std::string& kind,
                                  const std::vector<int>& array_sizes) {
  if (reserved_names_.count(name) || values_.count(name)) {
    const std::string base = name.substr(0, name.find('.'));
    Rcpp::stop("Flattening `%s` would create entries that already exist: "
               "`%s`. Remove the manually-supplied dotted entry or the "
               "list value.",
               base, name);
  }
  if (slot_values.empty()) {
    Rcpp::stop("`%s` has no enclosing array elements to flatten.", name);
  }

  const std::vector<int> ref_shape = payload_shape(slot_values[0]);
  for (size_t i = 1; i < slot_values.size(); ++i) {
    std::vector<int> shape_i = payload_shape(slot_values[i]);
    if (shape_i != ref_shape) {
      Rcpp::stop("`%s` has inconsistent value shapes across enclosing array "
                 "elements: element %d has shape %s but element 1 has "
                 "shape %s.",
                 name, static_cast<int>(i + 1), shape_string(shape_i),
                 shape_string(ref_shape));
    }
  }

  std::vector<size_t> dims;
  dims.reserve(array_sizes.size() + ref_shape.size());
  for (int d : array_sizes) dims.push_back(static_cast<size_t>(d));
  for (int d : ref_shape) dims.push_back(static_cast<size_t>(d));

  const size_t m = shape_product(ref_shape);
  if (kind == "complex") {
    std::vector<std::complex<double>> values;
    values.reserve(m * slot_values.size());
    for (SEXP payload : slot_values) {
      if (TYPEOF(payload) != INTSXP && TYPEOF(payload) != REALSXP
          && TYPEOF(payload) != CPLXSXP) {
        Rcpp::stop("`%s` must contain numeric or complex values.", name);
      }
      Rcpp::ComplexVector coerced(Rf_coerceVector(payload, CPLXSXP));
      for (R_xlen_t j = 0; j < coerced.size(); ++j) {
        values.emplace_back(coerced[j].r, coerced[j].i);
      }
    }
    store_complex(name, std::move(values), std::move(dims),
                  array_sizes.size());
  } else {
    std::vector<double> values;
    values.reserve(m * slot_values.size());
    for (SEXP payload : slot_values) {
      if (TYPEOF(payload) != INTSXP && TYPEOF(payload) != REALSXP) {
        Rcpp::stop("`%s` must contain numeric values.", name);
      }
      Rcpp::NumericVector coerced(Rf_coerceVector(payload, REALSXP));
      values.insert(values.end(), coerced.begin(), coerced.end());
    }
    store_numeric(name, std::move(values), std::move(dims));
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

void r_data_context::validate_dims(
    const std::string& stage, const std::string& name,
    const std::string& base_type,
    const std::vector<size_t>& dims_declared) const {
  stan::io::validate_dims(*this, stage, name, base_type, dims_declared);
}

}  // namespace stanr
