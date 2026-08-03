#include <stan/math/prim/fun/Eigen.hpp>
#include <tbb/global_control.h>
#include <Rcpp.h>
#include <RcppEigen.h>

#include <stan/model/model_base.hpp>

#include "include/run_advi.hpp"
#include "include/run_diagnose.hpp"
#include "include/run_optimizing.hpp"
#include "include/run_pathfinder.hpp"
#include "include/run_laplace.hpp"
#include "include/run_sampling.hpp"
#include "include/run_standalone_gqs.hpp"
#include "newstan/r_data_context.hpp"

namespace newstan {

Rcpp::List run_model(stan::model::model_base& model, Rcpp::List args) {
  int num_threads = Rcpp::as<int>(args["num_threads"]);
  if (num_threads < 1) {
    Rcpp::stop("num_threads must be a positive integer.");
  }
  // First call fixes the static TBB ceiling for the whole session, so set
  // it to hardware concurrency (-1), not this run's request.
  stan::math::init_threadpool_tbb(-1);
  // TBB's effective limit is the minimum over live global_control objects,
  // so this scopes the current run down to num_threads and is released on
  // return. It stays alive for the whole service call: even the
  // worker-thread services block here until the coordinator joins.
  tbb::global_control run_limit(
      tbb::global_control::max_allowed_parallelism, num_threads);

  std::string method = Rcpp::as<std::string>(args["method"]);

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
    Rcpp::stop("Unknown method: " + method);
  }
}
}  // namespace newstan
