#include <stan/math/prim/fun/Eigen.hpp>
#include <tbb/global_control.h>
#include <Rcpp.h>
#include <RcppEigen.h>

#include <stan/services/error_codes.hpp>
#include <stan/model/model_base.hpp>

#include "Rcpp/XPtr.h"
#include "Rcpp/exceptions.h"
#include "Rcpp/internal/wrap.h"
#include "include/model_bridge.hpp"
#include "include/run_advi.hpp"
#include "include/run_diagnose.hpp"
#include "include/run_optimizing.hpp"
#include "include/run_pathfinder.hpp"
#include "include/run_laplace.hpp"
#include "include/run_sampling.hpp"
#include "include/run_standalone_gqs.hpp"

extern "C" SEXP newstan_run(SEXP model_ptr, SEXP args) {
  BEGIN_RCPP
  int num_threads = newstan::get_int(args, "num_threads", 0);
  stan::math::init_threadpool_tbb(num_threads);

  auto model = Rcpp::XPtr<stan::model::model_base>(model_ptr);
  if (!Rcpp::List(args).containsElementNamed("bridge")) {
    Rcpp::stop("Model bridge is missing. Recompile the Stan model with stan_model().");
  }
  SEXP bridge_ptr = Rcpp::List(args)["bridge"];
  auto bridge = Rcpp::XPtr<newstan::model_bridge>(bridge_ptr);
  newstan::model_bridge_model bridged_model(*model, *bridge);

  // Extract method from args
  std::string method = newstan::get_string(args, "method", "sample");

  int return_code = stan::services::error_codes::CONFIG;

  if (method == "sample") {
    return Rcpp::wrap(newstan::run_sampling(bridged_model, args));
  } else if (method == "optimize") {
    return Rcpp::wrap(newstan::run_optimizing(bridged_model, args));
  } else if (method == "diagnose") {
    return Rcpp::wrap(newstan::run_diagnose(bridged_model, args));
  } else if (method == "variational") {
    return Rcpp::wrap(newstan::run_advi(bridged_model, args));
  } else if (method == "generate_quantities") {
    return Rcpp::wrap(newstan::run_standalone_gqs(bridged_model, args));
  } else if (method == "pathfinder") {
    return Rcpp::wrap(newstan::run_pathfinder(bridged_model, args));
  } else if (method == "laplace") {
    return Rcpp::wrap(newstan::run_laplace(bridged_model, args));
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
  END_RCPP
}

extern "C" SEXP r_data_context(SEXP data_list) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::io::var_context> m(new newstan::r_data_context(Rcpp::List(data_list)));
  return Rcpp::wrap(m);
  END_RCPP
}

extern "C" SEXP constrained_par_names(SEXP model_ptr) {
  BEGIN_RCPP
  auto model = Rcpp::XPtr<stan::model::model_base>(model_ptr);
  std::vector<std::string> param_names;
  model->constrained_param_names(param_names, false, false);
  if (param_names.size() < 1) {
    std::stringstream msg;
    msg << "Model " << model->model_name()
        << " has no parameters, nothing to estimate." << std::endl;
    Rcpp::stop(msg.str());
  }
  return Rcpp::wrap(param_names);
  END_RCPP
}
