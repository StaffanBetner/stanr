#ifndef STANR_RUN_LAPLACE_HPP
#define STANR_RUN_LAPLACE_HPP

#include <cpp11.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
#include <stan/services/optimize/laplace_sample.hpp>
#include <stan/callbacks/structured_writer.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace stanr {
  template <class Model>
  cpp11::writable::list run_laplace(Model& model, cpp11::list args) {
    const Eigen::VectorXd mode = stanr::as_cpp<Eigen::VectorXd>(args["mode"]);
    const int draws = stanr::as_cpp<int>(args["num_draws"]);
    const bool jacobian = stanr::as_cpp<bool>(args["jacobian"]);
    const bool calculate_lp = stanr::as_cpp<bool>(args["calculate_lp"]);
    const unsigned int seed = stanr::as_cpp<unsigned int>(args["seed"]);
    const int refresh = stanr::as_cpp<int>(args["refresh"]);
    const bool verbose = stanr::as_cpp<bool>(args["verbose"]);
    const bool show_exceptions = stanr::as_cpp<bool>(args["show_exceptions"]);

    stanr::r_sample_writer sample_writer(draws);
    // No-op: the mode's Hessian is not exposed to R.
    stan::callbacks::structured_writer hessian_writer;
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    int return_code;
    if (jacobian) {
      return_code = stan::services::laplace_sample<true>(
          model, mode, draws, calculate_lp, seed, refresh, interrupt, logger,
          sample_writer, hessian_writer);
    } else {
      return_code = stan::services::laplace_sample<false>(
          model, mode, draws, calculate_lp, seed, refresh, interrupt, logger,
          sample_writer, hessian_writer);
    }
    logger.flush();
    cpp11::writable::strings output = cpp11::as_sexp(logger.history());
    return cpp11::writable::list({
      cpp11::named_arg("draws") = sample_writer.to_r_matrix(),
      cpp11::named_arg("return_code") = return_code,
      cpp11::named_arg("output") = output
    });
  }
}

#endif
