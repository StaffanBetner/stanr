#include <stan/math/prim/fun/Eigen.hpp>
#include <tbb/global_control.h>
#include <Rcpp.h>
#include <RcppEigen.h>

#include <stan/services/error_codes.hpp>
#include <stan/model/model_base.hpp>

#include "include/run_advi.hpp"
#include "include/run_diagnose.hpp"
#include "include/run_optimizing.hpp"
#include "include/run_pathfinder.hpp"
#include "include/run_laplace.hpp"
#include "include/run_sampling.hpp"
#include "include/run_standalone_gqs.hpp"

namespace newstan {

Rcpp::List run_model(stan::model::model_base& model, Rcpp::List args) {
  int num_threads = newstan::get_int(args, "num_threads", 0);
  stan::math::init_threadpool_tbb(num_threads);

  // Extract method from args
  std::string method = newstan::get_string(args, "method", "sample");

  int return_code = stan::services::error_codes::CONFIG;

  if (method == "sample") {
    return newstan::run_sampling(model, args);
  } else if (method == "optimize") {
    return newstan::run_optimizing(model, args);
  } else if (method == "diagnose") {
    return newstan::run_diagnose(model, args);
  } else if (method == "variational") {
    return newstan::run_advi(model, args);
  } else if (method == "generate_quantities") {
    return newstan::run_standalone_gqs(model, args);
  } else if (method == "pathfinder") {
    return newstan::run_pathfinder(model, args);
  } else if (method == "laplace") {
    return newstan::run_laplace(model, args);
  } else {
    std::ostringstream msg;
    msg << "Unknown method: " << method;
    Rcpp::Rcout << msg.str() << std::endl;
    return_code = stan::services::error_codes::CONFIG;
  }

  // Fallback: build minimal result list
  return Rcpp::List::create(
    Rcpp::_["return_code"] = return_code,
    Rcpp::_["method"] = method
  );
}

Rcpp::CharacterVector constrained_param_names(
    const stan::model::model_base& model) {
  std::vector<std::string> param_names;
  model.constrained_param_names(param_names, false, false);
  if (param_names.size() < 1) {
    std::stringstream msg;
    msg << "Model " << model.model_name()
        << " has no parameters, nothing to estimate." << std::endl;
    Rcpp::stop(msg.str());
  }
  return Rcpp::wrap(param_names);
}

}  // namespace newstan
