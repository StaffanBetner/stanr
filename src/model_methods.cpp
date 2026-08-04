#include "include/model_methods.hpp"
#include <stanr/r_data_context.hpp>
#include <stan/math/rev/functor/gradient.hpp>
#include <stan/math/rev/functor/finite_diff_hessian_auto.hpp>
#include <stan/model/log_prob_grad.hpp>
#include <stan/model/log_prob_propto.hpp>
#include <stan/model/model_base.hpp>
#include <stan/services/util/create_rng.hpp>

#include <Rcpp.h>

#include <cmath>
#include <cstddef>
#include <limits>
#include <sstream>
#include <stdexcept>
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
    throw std::invalid_argument(
        model_method_error_prefix(model, method)
        + "expected " + std::to_string(expected)
        + " unconstrained parameter value(s), but received "
        + std::to_string(values.size()) + ".");
  }

  Eigen::VectorXd result(expected);
  for (R_xlen_t i = 0; i < expected; ++i) {
    const double value = values[i];
    if (!std::isfinite(value)) {
      throw std::invalid_argument(
          model_method_error_prefix(model, method)
          + "unconstrained parameter values must all be finite; value "
          + std::to_string(i + 1) + " is not finite.");
    }
    result[static_cast<Eigen::Index>(i)] = value;
  }
  return result;
}

Rcpp::NumericVector eigen_to_numeric(const Eigen::VectorXd& values) {
  Rcpp::NumericVector result(values.size());
  Eigen::Map<Eigen::VectorXd>(result.begin(), values.size()) = values;
  return result;
}

Rcpp::NumericMatrix eigen_to_numeric(const Eigen::MatrixXd& values) {
  Rcpp::NumericMatrix result(values.rows(), values.cols());
  Eigen::Map<Eigen::MatrixXd>(result.begin(), values.rows(), values.cols()) =
      values;
  return result;
}

template <typename RObject>
void attach_messages(RObject& result, const std::ostringstream& stream) {
  const std::string messages = stream.str();
  if (!messages.empty()) result.attr("messages") = messages;
}

Rcpp::XPtr<stan::rng_t> make_base_rng(unsigned int seed) {
  // A model-method RNG is an independent stream. Chain zero is intentional:
  // these draws do not share a stream with an inference chain.
  return Rcpp::XPtr<stan::rng_t>(
      new stan::rng_t(stan::services::util::create_rng(seed, 0)));
}

int model_num_upars(const stan::model::model_base& model) {
  const size_t count = model.num_params_r();
  if (count > static_cast<size_t>(std::numeric_limits<int>::max())) {
    throw std::overflow_error("The model has too many parameters for R.");
  }
  return static_cast<int>(count);
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

  if (parameter_names.size() != parameter_dims.size()
      || transformed_names.size() != transformed_dims.size()
      || all_names.size() != all_dims.size()
      || parameter_names.size() > transformed_names.size()
      || transformed_names.size() > all_names.size()) {
    throw std::runtime_error(
        model_method_error_prefix(model, "model_param_metadata")
        + "generated parameter names and dimensions are inconsistent.");
  }

  Rcpp::CharacterVector names(all_names.size());
  Rcpp::List dimensions(all_names.size());
  Rcpp::CharacterVector stages(all_names.size());
  for (size_t i = 0; i < all_names.size(); ++i) {
    names[i] = all_names[i];
    Rcpp::IntegerVector dims(all_dims[i].size());
    for (size_t j = 0; j < all_dims[i].size(); ++j) {
      if (all_dims[i][j]
          > static_cast<size_t>(std::numeric_limits<int>::max())) {
        throw std::overflow_error(
            model_method_error_prefix(model, "model_param_metadata")
            + "a declared dimension is too large for R.");
      }
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
    throw std::runtime_error(model_method_error_prefix(model, "model_log_prob")
                             + error.what());
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
    throw std::runtime_error(
        model_method_error_prefix(model, "model_grad_log_prob")
        + error.what());
  }

  Rcpp::NumericVector result = eigen_to_numeric(gradient);
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
    throw std::runtime_error(model_method_error_prefix(model, "model_hessian")
                             + error.what());
  }

  Rcpp::List result = Rcpp::List::create(
      Rcpp::Named("log_prob") = lp,
      Rcpp::Named("grad_log_prob") = eigen_to_numeric(gradient),
      Rcpp::Named("hessian") = eigen_to_numeric(hessian));
  attach_messages(result, messages);
  return result;
}

Rcpp::NumericVector model_unconstrain(
    const stan::model::model_base& model, Rcpp::List variables) {
  r_data_context context(variables);
  Eigen::VectorXd upars;
  std::ostringstream messages;
  try {
    model.transform_inits(context, upars, &messages);
  } catch (const std::exception& error) {
    throw std::runtime_error(
        model_method_error_prefix(model, "model_unconstrain")
        + error.what());
  }
  if (upars.size() != static_cast<Eigen::Index>(model.num_params_r())) {
    throw std::runtime_error(
        model_method_error_prefix(model, "model_unconstrain")
        + "the generated transform returned an unexpected number of values.");
  }
  Rcpp::NumericVector result = eigen_to_numeric(upars);
  attach_messages(result, messages);
  return result;
}

Rcpp::NumericMatrix model_unconstrain_matrix(
    const stan::model::model_base& model, Rcpp::NumericMatrix values) {
  std::vector<std::string> constrained_names;
  model.constrained_param_names(constrained_names, false, false);
  if (values.ncol() != static_cast<int>(constrained_names.size())) {
    throw std::invalid_argument(
        model_method_error_prefix(model, "model_unconstrain_matrix")
        + "expected " + std::to_string(constrained_names.size())
        + " constrained parameter column(s), but received "
        + std::to_string(values.ncol()) + ".");
  }

  const int output_columns = model_num_upars(model);
  Rcpp::NumericMatrix result(values.nrow(), output_columns);
  std::ostringstream messages;
  for (int row = 0; row < values.nrow(); ++row) {
    Eigen::VectorXd constrained(values.ncol());
    for (int column = 0; column < values.ncol(); ++column) {
      const double value = values(row, column);
      if (!std::isfinite(value)) {
        throw std::invalid_argument(
            model_method_error_prefix(model, "model_unconstrain_matrix")
            + "constrained parameter values must all be finite; row "
            + std::to_string(row + 1) + ", column "
            + std::to_string(column + 1) + " is not finite.");
      }
      constrained[column] = value;
    }

    Eigen::VectorXd unconstrained;
    try {
      model.unconstrain_array(constrained, unconstrained, &messages);
    } catch (const std::exception& error) {
      throw std::runtime_error(
          model_method_error_prefix(model, "model_unconstrain_matrix")
          + "row " + std::to_string(row + 1) + ": " + error.what());
    }
    if (unconstrained.size() != output_columns) {
      throw std::runtime_error(
          model_method_error_prefix(model, "model_unconstrain_matrix")
          + "row " + std::to_string(row + 1)
          + " produced an unexpected number of values.");
    }
    for (int column = 0; column < output_columns; ++column) {
      result(row, column) = unconstrained[column];
    }
  }

  Rcpp::CharacterVector unconstrained_names
      = model_unconstrained_names(model);
  result.attr("dimnames")
      = Rcpp::List::create(R_NilValue, unconstrained_names);
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
    throw std::runtime_error(model_method_error_prefix(model, "model_constrain")
                             + error.what());
  }

  Rcpp::CharacterVector names
      = model_constrained_names(model, include_tparams, include_gqs);
  if (constrained.size() != names.size()) {
    throw std::runtime_error(
        model_method_error_prefix(model, "model_constrain")
        + "the generated transform returned an unexpected number of values.");
  }
  Rcpp::NumericVector result = eigen_to_numeric(constrained);
  result.names() = names;
  attach_messages(result, messages);
  return result;
}

}  // namespace stanr
