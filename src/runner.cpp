#include <stan/math/prim/fun/Eigen.hpp>
#include <tbb/global_control.h>
#include <cpp11.hpp>
#include <stanr/cpp11_tuple_interop.hpp>

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

cpp11::writable::list run_model(stan::model::model_base& model, cpp11::list args) {
  int num_threads = stanr::as_cpp<int>(args["num_threads"]);
  if (num_threads < 1) {
    cpp11::stop("num_threads must be a positive integer.");
  }
  // First call fixes the static TBB ceiling for the session; -1 = hardware
  // concurrency. The scoped global_control limits this run to num_threads.
  stan::math::init_threadpool_tbb(-1);
  tbb::global_control run_limit(
      tbb::global_control::max_allowed_parallelism, num_threads);

  std::string method = stanr::as_cpp<std::string>(args["method"]);

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
    cpp11::stop("Unknown method: %s", method.c_str());
  }
}
}  // namespace stanr
