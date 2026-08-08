#include <stanr/model_methods.hpp>
#include <stanr/tuple_declarations.hpp>
#include <stanr/r_data_context.hpp>
#include <stan/math/rev/functor/gradient.hpp>
#include <stan/math/rev/functor/finite_diff_hessian_auto.hpp>
#include <stan/model/log_prob_grad.hpp>
#include <stan/model/log_prob_propto.hpp>
#include <stan/model/model_base.hpp>
#include <stan/services/util/create_rng.hpp>

#include <RcppEigen.h>

#include <cmath>
#include <cstddef>
#include <functional>
#include <map>
#include <numeric>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace stanr {

std::string model_method_error_prefix(
    const stan::model::model_base& model, const char* method) {
  return std::string(method) + " for model '" + model.model_name() + "': ";
}

Eigen::VectorXd checked_unconstrained_values(
    const stan::model::model_base& model, Rcpp::NumericVector values,
    const char* method) {
  const R_xlen_t expected = static_cast<R_xlen_t>(model.num_params_r());
  if (values.size() != expected) {
    Rcpp::stop(
        "%sexpected %d unconstrained parameter value(s), but received %d.",
        model_method_error_prefix(model, method), expected, values.size());
  }

  const Eigen::Map<const Eigen::VectorXd> mapped(values.begin(), expected);
  if (!mapped.allFinite()) {
    for (R_xlen_t i = 0; i < expected; ++i) {
      if (!std::isfinite(values[i])) {
        Rcpp::stop(
            "%sunconstrained parameter values must all be finite; value "
            "%d is not finite.",
            model_method_error_prefix(model, method), i + 1);
      }
    }
  }
  return mapped;
}

template <typename RObject>
void attach_messages(RObject& result, const std::ostringstream& stream) {
  const std::string messages = stream.str();
  if (!messages.empty()) result.attr("messages") = messages;
}

Rcpp::XPtr<stan::rng_t> make_base_rng(unsigned int seed) {
  // Independent stream; chain zero so these draws don't share an inference
  // chain's stream.
  return Rcpp::XPtr<stan::rng_t>(
      new stan::rng_t(stan::services::util::create_rng(seed, 0)));
}

int model_num_upars(const stan::model::model_base& model) {
  return static_cast<int>(model.num_params_r());
}

Rcpp::List model_param_metadata(
    const stan::model::model_base& model) {
  std::vector<std::string> parameter_names;
  std::vector<std::vector<size_t>> parameter_dims;
  std::vector<std::string> transformed_names;
  std::vector<std::vector<size_t>> transformed_dims;
  std::vector<std::string> all_names;
  std::vector<std::vector<size_t>> all_dims;

  model.get_param_names(parameter_names, false, false);
  model.get_dims(parameter_dims, false, false);
  model.get_param_names(transformed_names, true, false);
  model.get_dims(transformed_dims, true, false);
  model.get_param_names(all_names, true, true);
  model.get_dims(all_dims, true, true);

  Rcpp::CharacterVector names(all_names.size());
  Rcpp::List dimensions(all_names.size());
  Rcpp::CharacterVector stages(all_names.size());
  for (size_t i = 0; i < all_names.size(); ++i) {
    names[i] = all_names[i];
    Rcpp::IntegerVector dims(all_dims[i].size());
    for (size_t j = 0; j < all_dims[i].size(); ++j) {
      dims[j] = static_cast<int>(all_dims[i][j]);
    }
    dimensions[i] = dims;
    stages[i] = i < parameter_names.size()
                    ? "parameter"
                    : (i < transformed_names.size()
                           ? "transformed_parameter"
                           : "generated_quantity");
  }

  return Rcpp::List::create(Rcpp::Named("names") = names,
                            Rcpp::Named("dimensions") = dimensions,
                            Rcpp::Named("stages") = stages);
}

Rcpp::CharacterVector model_constrained_names(
    const stan::model::model_base& model, bool include_tparams,
    bool include_gqs) {
  std::vector<std::string> names;
  model.constrained_param_names(names, include_tparams, include_gqs);
  return Rcpp::wrap(names);
}

Rcpp::CharacterVector model_unconstrained_names(
    const stan::model::model_base& model) {
  std::vector<std::string> names;
  model.unconstrained_param_names(names, false, false);
  return Rcpp::wrap(names);
}

Rcpp::NumericVector model_log_prob(
    const stan::model::model_base& model, Rcpp::NumericVector values,
    bool jacobian) {
  Eigen::VectorXd upars
      = checked_unconstrained_values(model, values, "model_log_prob");
  std::ostringstream messages;
  double lp;
  try {
    lp = jacobian
             ? stan::model::log_prob_propto<true>(model, upars, &messages)
             : stan::model::log_prob_propto<false>(model, upars, &messages);
  } catch (const std::exception& error) {
    Rcpp::stop("%s%s", model_method_error_prefix(model, "model_log_prob"),
               error.what());
  }
  Rcpp::NumericVector result = Rcpp::NumericVector::create(lp);
  attach_messages(result, messages);
  return result;
}

Rcpp::NumericVector model_grad_log_prob(
    const stan::model::model_base& model, Rcpp::NumericVector values,
    bool jacobian) {
  Eigen::VectorXd upars
      = checked_unconstrained_values(model, values, "model_grad_log_prob");
  Eigen::VectorXd gradient;
  std::ostringstream messages;
  double lp;
  try {
    lp = jacobian
             ? stan::model::log_prob_grad<true, true>(model, upars, gradient,
                                                       &messages)
             : stan::model::log_prob_grad<true, false>(model, upars, gradient,
                                                        &messages);
  } catch (const std::exception& error) {
    Rcpp::stop("%s%s", model_method_error_prefix(model, "model_grad_log_prob"),
               error.what());
  }

  Rcpp::NumericVector result = Rcpp::wrap(gradient);
  result.attr("log_prob") = lp;
  attach_messages(result, messages);
  return result;
}

Rcpp::List model_hessian(const stan::model::model_base& model,
                                Rcpp::NumericVector values, bool jacobian) {
  Eigen::VectorXd upars
      = checked_unconstrained_values(model, values, "model_hessian");
  std::ostringstream messages;
  double lp;
  Eigen::VectorXd gradient;
  Eigen::MatrixXd hessian;

  try {
    if (upars.size() == 0) {
      lp = jacobian
               ? stan::model::log_prob_propto<true>(model, upars, &messages)
               : stan::model::log_prob_propto<false>(model, upars, &messages);
      gradient.resize(0);
      hessian.resize(0, 0);
    } else {
      auto log_density = [&](auto&& parameters) {
        if (jacobian) {
          return model.template log_prob<true, true>(parameters, &messages);
        }
        return model.template log_prob<true, false>(parameters, &messages);
      };
      stan::math::internal::finite_diff_hessian_auto(
          log_density, upars, lp, gradient, hessian);
    }
  } catch (const std::exception& error) {
    Rcpp::stop("%s%s", model_method_error_prefix(model, "model_hessian"),
               error.what());
  }

  Rcpp::List result = Rcpp::List::create(
      Rcpp::Named("log_prob") = lp,
      Rcpp::Named("grad_log_prob") = gradient,
      Rcpp::Named("hessian") = hessian);
  attach_messages(result, messages);
  return result;
}

Rcpp::NumericVector model_unconstrain(
    const stan::model::model_base& model, Rcpp::List variables,
    SEXP declarations) {
  r_data_context context(variables, declarations);
  Eigen::VectorXd upars;
  std::ostringstream messages;
  try {
    model.transform_inits(context, upars, &messages);
  } catch (const std::exception& error) {
    Rcpp::stop("%s%s", model_method_error_prefix(model, "model_unconstrain"),
               error.what());
  }
  if (upars.size() != static_cast<Eigen::Index>(model.num_params_r())) {
    Rcpp::stop(
        "%sthe generated transform returned an unexpected number of values.",
        model_method_error_prefix(model, "model_unconstrain"));
  }
  Rcpp::NumericVector result = Rcpp::wrap(upars);
  attach_messages(result, messages);
  return result;
}

Rcpp::NumericMatrix model_unconstrain_matrix(
    const stan::model::model_base& model, Rcpp::NumericMatrix values) {
  std::vector<std::string> constrained_names;
  model.constrained_param_names(constrained_names, false, false);
  if (values.ncol() != static_cast<int>(constrained_names.size())) {
    Rcpp::stop(
        "%sexpected %d constrained parameter column(s), but received %d.",
        model_method_error_prefix(model, "model_unconstrain_matrix"),
        constrained_names.size(), values.ncol());
  }

  const int output_columns = model_num_upars(model);
  Rcpp::NumericMatrix result(values.nrow(), output_columns);
  const Eigen::Map<const Eigen::MatrixXd> input(values.begin(), values.nrow(),
                                                 values.ncol());
  Eigen::Map<Eigen::MatrixXd> output(result.begin(), values.nrow(),
                                     output_columns);
  std::ostringstream messages;
  for (int row = 0; row < values.nrow(); ++row) {
    if (!input.row(row).allFinite()) {
      for (int column = 0; column < values.ncol(); ++column) {
        if (!std::isfinite(input(row, column))) {
          Rcpp::stop(
              "%sconstrained parameter values must all be finite; row %d, "
              "column %d is not finite.",
              model_method_error_prefix(model, "model_unconstrain_matrix"),
              row + 1, column + 1);
        }
      }
    }

    Eigen::VectorXd unconstrained;
    try {
      model.unconstrain_array(input.row(row), unconstrained, &messages);
    } catch (const std::exception& error) {
      Rcpp::stop(
          "%srow %d: %s",
          model_method_error_prefix(model, "model_unconstrain_matrix"),
          row + 1, error.what());
    }
    if (unconstrained.size() != output_columns) {
      Rcpp::stop(
          "%srow %d produced an unexpected number of values.",
          model_method_error_prefix(model, "model_unconstrain_matrix"),
          row + 1);
    }
    output.row(row) = unconstrained;
  }

  Rcpp::CharacterVector unconstrained_names
      = model_unconstrained_names(model);
  result.attr("dimnames")
      = Rcpp::List::create(R_NilValue, unconstrained_names);
  attach_messages(result, messages);
  return result;
}

namespace {

// One node of the "sized structure": the declared type tree of a variable
// (from `model$variables()`) merged with the generated model's per-leaf
// sizes. A leaf's `dims` are its own container dims, with enclosing
// tuple-array dims stripped and complex's trailing storage `2` dropped.
struct sized_node {
  bool is_tuple = false;
  bool is_complex = false;
  std::vector<int> dims;        // leaf: own container dims
  std::vector<int> array_dims;  // tuple: own enclosing-array sizes
  std::vector<sized_node> slots;
};

struct sized_variable {
  std::string name;
  int stage;  // 0 = parameter, 1 = transformed parameter, 2 = gq
  sized_node node;
};

using metadata_index = std::map<std::string, const std::vector<size_t>*>;

int dims_product(const std::vector<int>& dims) {
  return std::accumulate(dims.begin(), dims.end(), 1, std::multiplies<>());
}

// Dotted name of some leaf reachable from this tuple node via slot 1: any
// leaf beneath the subtree carries the same enclosing-array dims prefix.
std::string first_leaf_name(const std::string& dotted, Rcpp::List type_df) {
  slot_decl slot = tuple_slot(type_df, 0);
  const std::string name = dotted + ".1";
  return slot.is_tuple ? first_leaf_name(name, slot.tuple_df) : name;
}

const std::vector<size_t>& metadata_dims(const metadata_index& metadata,
                                         const std::string& name) {
  const auto it = metadata.find(name);
  if (it == metadata.end()) {
    Rcpp::stop(
        "stanr internal error: the model metadata is missing an entry for "
        "`%s`.",
        name);
  }
  return *it->second;
}

sized_node build_tuple_node(const std::string& dotted, Rcpp::List type_df,
                            int own_array_count,
                            const std::vector<int>& outer_dims,
                            const metadata_index& metadata) {
  sized_node node;
  node.is_tuple = true;
  const size_t n_outer = outer_dims.size();
  if (own_array_count > 0) {
    const std::vector<size_t>& probe
        = metadata_dims(metadata, first_leaf_name(dotted, type_df));
    for (int d = 0; d < own_array_count; ++d) {
      node.array_dims.push_back(static_cast<int>(probe[n_outer + d]));
    }
  }
  std::vector<int> new_outer = outer_dims;
  new_outer.insert(new_outer.end(), node.array_dims.begin(),
                   node.array_dims.end());

  const int n_slots = tuple_slot_count(type_df);
  node.slots.reserve(n_slots);
  for (int s = 0; s < n_slots; ++s) {
    const std::string slot_name = dotted + "." + std::to_string(s + 1);
    const slot_decl slot = tuple_slot(type_df, s);
    if (slot.is_tuple) {
      node.slots.push_back(build_tuple_node(slot_name, slot.tuple_df,
                                            slot.dims, new_outer, metadata));
      continue;
    }
    const std::vector<size_t>& dims = metadata_dims(metadata, slot_name);
    for (size_t d = 0; d < new_outer.size(); ++d) {
      if (d >= dims.size()
          || dims[d] != static_cast<size_t>(new_outer[d])) {
        Rcpp::stop(
            "stanr internal error: `%s`'s declared dimensions are "
            "inconsistent with the enclosing tuple-array sizes.",
            slot_name);
      }
    }
    sized_node leaf;
    leaf.is_complex = slot.kind == "complex";
    for (size_t d = new_outer.size(); d < dims.size(); ++d) {
      leaf.dims.push_back(static_cast<int>(dims[d]));
    }
    if (leaf.is_complex) leaf.dims.pop_back();
    node.slots.push_back(std::move(leaf));
  }
  return node;
}

std::vector<sized_variable> build_sized_structure(
    const stan::model::model_base& model, Rcpp::List declarations) {
  std::vector<std::string> names;
  model.get_param_names(names, false, false);
  const size_t n_parameter_rows = names.size();
  model.get_param_names(names, true, false);
  const size_t n_transformed_rows = names.size();
  model.get_param_names(names, true, true);
  std::vector<std::vector<size_t>> dims;
  model.get_dims(dims, true, true);

  metadata_index metadata;
  for (size_t i = 0; i < names.size(); ++i) {
    metadata.emplace(names[i], &dims[i]);
  }

  std::vector<sized_variable> result;
  std::set<std::string> seen;
  for (size_t i = 0; i < names.size(); ++i) {
    const std::string base = names[i].substr(0, names[i].find('.'));
    if (!seen.insert(base).second) continue;
    if (!declarations.containsElementNamed(base.c_str())) {
      Rcpp::stop(
          "stanr internal error: `%s` from the model metadata is not "
          "declared in `model$variables()`.",
          base);
    }
    Rcpp::List decl = declarations[base];
    SEXP decl_type = decl["type"];

    sized_variable variable;
    variable.name = base;
    variable.stage = i < n_parameter_rows ? 0
                     : (i < n_transformed_rows ? 1 : 2);
    if (Rf_inherits(decl_type, "data.frame")) {
      variable.node = build_tuple_node(base, decl_type,
                                       decl_int(decl["dimensions"], 0), {},
                                       metadata);
    } else {
      const std::vector<size_t>& leaf_dims = metadata_dims(metadata, base);
      variable.node.is_complex
          = Rcpp::as<std::string>(decl_type) == "complex";
      for (size_t d : leaf_dims) {
        variable.node.dims.push_back(static_cast<int>(d));
      }
      // get_dims() includes complex's trailing storage dim of size two.
      if (variable.node.is_complex) variable.node.dims.pop_back();
    }
    result.push_back(std::move(variable));
  }
  return result;
}

void apply_leaf_dims(SEXP values, const std::vector<int>& dims) {
  if (dims.size() >= 2) {
    Rf_setAttrib(values, R_DimSymbol,
                 Rcpp::IntegerVector(dims.begin(), dims.end()));
  }
}

// Rebuild the canonical nested-list shape from tuple-array elements
// enumerated column-major (first index fastest).
SEXP reshape_column_major(Rcpp::List elements, const std::vector<int>& dims) {
  if (dims.size() <= 1) return elements;
  const int d1 = dims[0];
  const std::vector<int> rest(dims.begin() + 1, dims.end());
  const int rest_n = dims_product(rest);
  Rcpp::List out(d1);
  for (int i = 0; i < d1; ++i) {
    Rcpp::List sub(rest_n);
    for (int r = 0; r < rest_n; ++r) sub[r] = elements[i + r * d1];
    out[i] = reshape_column_major(sub, rest);
  }
  return out;
}

// Consume one node's worth of scalars from `flat` in native constrained
// order: containers column-major; complex with adjacent (real, imag) pairs;
// tuples slot-by-slot; tuple arrays element-major.
SEXP consume_node(const sized_node& node, const Eigen::VectorXd& flat,
                  Eigen::Index& pos) {
  if (node.is_tuple) {
    const int n_elements = dims_product(node.array_dims);
    Rcpp::List elements(n_elements);
    for (int e = 0; e < n_elements; ++e) {
      Rcpp::List slots(node.slots.size());
      for (size_t s = 0; s < node.slots.size(); ++s) {
        slots[s] = consume_node(node.slots[s], flat, pos);
      }
      elements[e] = slots;
    }
    if (node.array_dims.empty()) return elements[0];
    return reshape_column_major(elements, node.array_dims);
  }
  const int n = dims_product(node.dims);
  if (node.is_complex) {
    if (pos + 2 * n > flat.size()) {
      Rcpp::stop(
          "stanr internal error: the constrained output has fewer values "
          "than the variable structure expects.");
    }
    Rcpp::ComplexVector values(n);
    for (int j = 0; j < n; ++j) {
      values[j].r = flat[pos + 2 * j];
      values[j].i = flat[pos + 2 * j + 1];
    }
    pos += 2 * n;
    apply_leaf_dims(values, node.dims);
    return values;
  }
  if (pos + n > flat.size()) {
    Rcpp::stop(
        "stanr internal error: the constrained output has fewer values "
        "than the variable structure expects.");
  }
  Rcpp::NumericVector values(n);
  for (int j = 0; j < n; ++j) values[j] = flat[pos + j];
  pos += n;
  apply_leaf_dims(values, node.dims);
  return values;
}

SEXP skeleton_node(const sized_node& node) {
  if (node.is_tuple) {
    const int n_elements = dims_product(node.array_dims);
    Rcpp::List elements(n_elements);
    for (int e = 0; e < n_elements; ++e) {
      Rcpp::List slots(node.slots.size());
      for (size_t s = 0; s < node.slots.size(); ++s) {
        slots[s] = skeleton_node(node.slots[s]);
      }
      elements[e] = slots;
    }
    if (node.array_dims.empty()) return elements[0];
    return reshape_column_major(elements, node.array_dims);
  }
  const int n = dims_product(node.dims);
  SEXP values;
  if (node.is_complex) {
    Rcpp::ComplexVector out(n);
    std::fill(COMPLEX(out), COMPLEX(out) + n,
              Rcomplex{NA_REAL, NA_REAL});
    values = out;
  } else {
    values = Rcpp::NumericVector(n, NA_REAL);
  }
  apply_leaf_dims(values, node.dims);
  return values;
}

bool stage_kept(int stage, bool include_tparams, bool include_gqs) {
  return stage == 0 || (include_tparams && stage == 1)
         || (include_gqs && stage == 2);
}

}  // namespace

Rcpp::List model_constrain_variables(
    const stan::model::model_base& model, stan::rng_t& rng,
    Rcpp::NumericVector values, bool include_tparams, bool include_gqs,
    Rcpp::List declarations) {
  Eigen::VectorXd upars
      = checked_unconstrained_values(model, values,
                                     "model_constrain_variables");
  Eigen::VectorXd constrained;
  std::ostringstream messages;
  try {
    model.write_array(rng, upars, constrained, include_tparams, include_gqs,
                      &messages);
  } catch (const std::exception& error) {
    Rcpp::stop("%s%s",
               model_method_error_prefix(model, "model_constrain_variables"),
               error.what());
  }

  const std::vector<sized_variable> sized
      = build_sized_structure(model, declarations);
  std::vector<const sized_variable*> kept;
  for (const sized_variable& variable : sized) {
    if (stage_kept(variable.stage, include_tparams, include_gqs)) {
      kept.push_back(&variable);
    }
  }
  Rcpp::List result(kept.size());
  Rcpp::CharacterVector names(kept.size());
  Eigen::Index pos = 0;
  for (size_t k = 0; k < kept.size(); ++k) {
    names[k] = kept[k]->name;
    result[k] = consume_node(kept[k]->node, constrained, pos);
  }
  if (pos != constrained.size()) {
    Rcpp::stop(
        "%sthe constrained output length (%d) did not match the expected "
        "variable structure (consumed %d).",
        model_method_error_prefix(model, "model_constrain_variables"),
        constrained.size(), pos);
  }
  result.names() = names;
  attach_messages(result, messages);
  return result;
}

Rcpp::List model_variable_skeleton(
    const stan::model::model_base& model, bool include_tparams,
    bool include_gqs, Rcpp::List declarations) {
  const std::vector<sized_variable> sized
      = build_sized_structure(model, declarations);
  std::vector<const sized_variable*> kept;
  for (const sized_variable& variable : sized) {
    if (stage_kept(variable.stage, include_tparams, include_gqs)) {
      kept.push_back(&variable);
    }
  }
  Rcpp::List result(kept.size());
  Rcpp::CharacterVector names(kept.size());
  for (size_t k = 0; k < kept.size(); ++k) {
    names[k] = kept[k]->name;
    result[k] = skeleton_node(kept[k]->node);
  }
  result.names() = names;
  return result;
}

Rcpp::NumericMatrix model_constrain_matrix(
    const stan::model::model_base& model, stan::rng_t& rng,
    Rcpp::NumericMatrix values, bool include_tparams, bool include_gqs) {
  const int input_columns = model_num_upars(model);
  if (values.ncol() != input_columns) {
    Rcpp::stop(
        "%sexpected %d unconstrained parameter column(s), but received %d.",
        model_method_error_prefix(model, "model_constrain_matrix"),
        input_columns, values.ncol());
  }

  Rcpp::CharacterVector names
      = model_constrained_names(model, include_tparams, include_gqs);
  Rcpp::NumericMatrix result(values.nrow(), names.size());
  const Eigen::Map<const Eigen::MatrixXd> input(values.begin(), values.nrow(),
                                                 values.ncol());
  Eigen::Map<Eigen::MatrixXd> output(result.begin(), values.nrow(),
                                     names.size());
  std::ostringstream messages;
  Eigen::VectorXd upars(input_columns);
  Eigen::VectorXd constrained;
  for (int row = 0; row < values.nrow(); ++row) {
    if (!input.row(row).allFinite()) {
      for (int column = 0; column < input_columns; ++column) {
        if (!std::isfinite(input(row, column))) {
          Rcpp::stop(
              "%sunconstrained parameter values must all be finite; row %d, "
              "column %d is not finite.",
              model_method_error_prefix(model, "model_constrain_matrix"),
              row + 1, column + 1);
        }
      }
    }
    upars = input.row(row);

    try {
      model.write_array(rng, upars, constrained, include_tparams, include_gqs,
                        &messages);
    } catch (const std::exception& error) {
      Rcpp::stop("%srow %d: %s",
                 model_method_error_prefix(model, "model_constrain_matrix"),
                 row + 1, error.what());
    }
    if (constrained.size() != static_cast<Eigen::Index>(names.size())) {
      Rcpp::stop(
          "%srow %d produced an unexpected number of values.",
          model_method_error_prefix(model, "model_constrain_matrix"),
          row + 1);
    }
    output.row(row) = constrained;
  }

  result.attr("dimnames") = Rcpp::List::create(R_NilValue, names);
  attach_messages(result, messages);
  return result;
}

Rcpp::NumericVector model_constrain(
    const stan::model::model_base& model, stan::rng_t& rng,
    Rcpp::NumericVector values, bool include_tparams, bool include_gqs) {
  Eigen::VectorXd upars
      = checked_unconstrained_values(model, values, "model_constrain");
  Eigen::VectorXd constrained;
  std::ostringstream messages;
  try {
    model.write_array(rng, upars, constrained, include_tparams, include_gqs,
                      &messages);
  } catch (const std::exception& error) {
    Rcpp::stop("%s%s", model_method_error_prefix(model, "model_constrain"),
               error.what());
  }

  Rcpp::CharacterVector names
      = model_constrained_names(model, include_tparams, include_gqs);
  if (constrained.size() != names.size()) {
    Rcpp::stop(
        "%sthe generated transform returned an unexpected number of values.",
        model_method_error_prefix(model, "model_constrain"));
  }
  Rcpp::NumericVector result = Rcpp::wrap(constrained);
  result.names() = names;
  attach_messages(result, messages);
  return result;
}

}  // namespace stanr
