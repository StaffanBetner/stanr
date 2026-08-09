#include <stan/math/prim/fun/Eigen.hpp>
#include <tbb/global_control.h>
#include <Rcpp.h>
#include <stanr/rcpp_eigen_interop.hpp>

#include <stan/model/model_base.hpp>

#include <stanr/run_advi.hpp>
#include <stanr/run_diagnose.hpp>
#include <stanr/run_optimizing.hpp>
#include <stanr/run_pathfinder.hpp>
#include <stanr/run_laplace.hpp>
#include <stanr/run_sampling.hpp>
#include <stanr/run_standalone_gqs.hpp>
#include <stanr/r_data_context.hpp>

namespace stanr {

Rcpp::List run_model(stan::model::model_base& model, Rcpp::List args) {
  int num_threads = Rcpp::as<int>(args["num_threads"]);
  if (num_threads < 1) {
    Rcpp::stop("num_threads must be a positive integer.");
  }
  // First call fixes the static TBB ceiling for the session; -1 = hardware
  // concurrency. The scoped global_control limits this run to num_threads.
  stan::math::init_threadpool_tbb(-1);
  tbb::global_control run_limit(
      tbb::global_control::max_allowed_parallelism, num_threads);

  std::string method = Rcpp::as<std::string>(args["method"]);

  if (method == "sample") {
    return stanr::run_sampling(model, args);
  } else if (method == "optimize") {
    return stanr::run_optimizing(model, args);
  } else if (method == "diagnose") {
    return stanr::run_diagnose(model, args);
  } else if (method == "variational") {
    return stanr::run_advi(model, args);
  } else if (method == "generate_quantities") {
    return stanr::run_standalone_gqs(model, args);
  } else if (method == "pathfinder") {
    return stanr::run_pathfinder(model, args);
  } else if (method == "laplace") {
    return stanr::run_laplace(model, args);
  } else {
    Rcpp::stop("Unknown method: " + method);
  }
}
}  // namespace stanr
