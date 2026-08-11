#ifndef STANR_RUN_ADVI_HPP
#define STANR_RUN_ADVI_HPP

#include <cpp11.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
#include <stan/services/experimental/advi/fullrank.hpp>
#include <stan/services/experimental/advi/meanfield.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include <stanr/r_data_context.hpp>
#include "r_logger.hpp"

namespace stanr {
  template <class Model>
  cpp11::writable::list run_advi(Model& model, cpp11::list args) {
    const std::string algorithm = stanr::as_cpp<std::string>(args["algorithm"]);

    const unsigned int seed = stanr::as_cpp<unsigned int>(args["seed"]);
    const unsigned int chain_id = stanr::as_cpp<unsigned int>(args["id"]);
    const double init_radius = stanr::as_cpp<double>(args["init_radius"]);
    const int iter = stanr::as_cpp<int>(args["iter"]);
    const int grad_samples = stanr::as_cpp<int>(args["grad_samples"]);
    const int elbo_samples = stanr::as_cpp<int>(args["elbo_samples"]);
    const double tol_rel_obj = stanr::as_cpp<double>(args["tol_rel_obj"]);
    const double eta = stanr::as_cpp<double>(args["eta"]);
    const bool adapt_engaged = stanr::as_cpp<bool>(args["adapt_engaged"]);
    const int adapt_iter = stanr::as_cpp<int>(args["adapt_iter"]);
    const int eval_elbo = stanr::as_cpp<int>(args["eval_elbo"]);
    const int output_samples = stanr::as_cpp<int>(args["output_samples"]);
    const bool verbose = stanr::as_cpp<bool>(args["verbose"]);
    const bool show_exceptions = stanr::as_cpp<bool>(args["show_exceptions"]);

    cpp11::list init_list = args["init"];

    stanr::r_data_context init_ctx(init_list, args["init_declarations"]);
    stan::callbacks::writer init_writer;
    // Stan writes the posterior mean followed by output_samples draws.
    stanr::r_sample_writer sample_writer(output_samples + 1);
    stan::callbacks::writer diagnostic_writer;
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    int return_code = stan::services::error_codes::CONFIG;

    if (algorithm == "fullrank") {
      return_code = stan::services::experimental::advi::fullrank(
          model, init_ctx, seed, chain_id, init_radius,
          grad_samples, elbo_samples, iter, tol_rel_obj, eta,
          adapt_engaged, adapt_iter, eval_elbo, output_samples,
          interrupt, logger,
          init_writer, sample_writer,
          diagnostic_writer);
    } else if (algorithm == "meanfield") {
      return_code = stan::services::experimental::advi::meanfield(
          model, init_ctx, seed, chain_id, init_radius,
          grad_samples, elbo_samples, iter, tol_rel_obj, eta,
          adapt_engaged, adapt_iter, eval_elbo, output_samples,
          interrupt, logger,
          init_writer, sample_writer,
          diagnostic_writer);
    } else {
      logger.error("Unknown variational algorithm: " + algorithm);
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
