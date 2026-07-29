#include <stan/math/prim/fun/Eigen.hpp>
#include <tbb/global_control.h>
#include <Rcpp.h>
#include <RcppEigen.h>

#include <stan/services/error_codes.hpp>
#include <stan/model/model_base.hpp>

#include "Rcpp/XPtr.h"
#include "Rcpp/exceptions.h"
#include "Rcpp/internal/wrap.h"
#include "include/run_advi.hpp"
#include "include/run_diagnose.hpp"
#include "include/run_optimizing.hpp"
#include "include/run_pathfinder.hpp"
#include "include/run_sampling.hpp"
#include "include/run_standalone_gqs.hpp"

extern "C" SEXP newstan_run(SEXP model_ptr, SEXP args) {
  BEGIN_RCPP
  stan::math::init_threadpool_tbb();

  auto model = Rcpp::XPtr<stan::model::model_base>(model_ptr);

  // Extract method from args
  std::string method = newstan::get_string(args, "method", "sampling");

  int return_code = stan::services::error_codes::CONFIG;

  if (method == "sampling") {
    return Rcpp::wrap(newstan::run_sampling(*model, args));
  } else if (method == "optimizing") {
    return Rcpp::wrap(newstan::run_optimizing(*model, args));
  } else if (method == "diagnose") {
    return Rcpp::wrap(newstan::run_diagnose(*model, args));
  } else if (method == "advi") {
    return Rcpp::wrap(newstan::run_advi(*model, args));
  } else if (method == "standalone_gqs") {
    return Rcpp::wrap(newstan::run_standalone_gqs(*model, args));
  } else if (method == "pathfinder") {
    return Rcpp::wrap(newstan::run_pathfinder(*model, args));
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
