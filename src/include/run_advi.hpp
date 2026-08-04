#ifndef STANR_RUN_ADVI_HPP
#define STANR_RUN_ADVI_HPP

#include <Rcpp.h>
#include <stan/services/experimental/advi/fullrank.hpp>
#include <stan/services/experimental/advi/meanfield.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include <stanr/r_data_context.hpp>
#include "r_logger.hpp"

namespace stanr {
  template <class Model>
  Rcpp::List run_advi(Model& model, Rcpp::List args) {
    const std::string algorithm = Rcpp::as<std::string>(args["algorithm"]);

    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const unsigned int chain_id = Rcpp::as<unsigned int>(args["id"]);
    const double init_radius = Rcpp::as<double>(args["init_radius"]);
    const int iter = Rcpp::as<int>(args["iter"]);
    const int grad_samples = Rcpp::as<int>(args["grad_samples"]);
    const int elbo_samples = Rcpp::as<int>(args["elbo_samples"]);
    const double tol_rel_obj = Rcpp::as<double>(args["tol_rel_obj"]);
    const double eta = Rcpp::as<double>(args["eta"]);
    const bool adapt_engaged = Rcpp::as<bool>(args["adapt_engaged"]);
    const int adapt_iter = Rcpp::as<int>(args["adapt_iter"]);
    const int eval_elbo = Rcpp::as<int>(args["eval_elbo"]);
    const int output_samples = Rcpp::as<int>(args["output_samples"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);
    const bool show_exceptions = Rcpp::as<bool>(args["show_exceptions"]);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    stanr::r_data_context init_ctx(init_list);
    stanr::r_discard_writer init_writer;
    // Stan writes the posterior mean followed by output_samples draws.
    stanr::r_sample_writer sample_writer(output_samples + 1);
    stanr::r_discard_writer diagnostic_writer;
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
    Rcpp::CharacterVector output(logger.history().begin(), logger.history().end());
    return Rcpp::List::create(
      Rcpp::_["draws"] = sample_writer.to_r_matrix(),
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["output"] = output
    );
  }
}

#endif
