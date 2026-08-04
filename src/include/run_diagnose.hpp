#ifndef STANR_RUN_DIAGNOSE_HPP
#define STANR_RUN_DIAGNOSE_HPP

#include <Rcpp.h>
#include <stan/services/diagnose/diagnose.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include <stanr/r_data_context.hpp>

namespace stanr {
  template <class Model>
  Rcpp::List run_diagnose(Model& model, Rcpp::List args) {
    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const unsigned int chain_id = Rcpp::as<unsigned int>(args["id"]);
    const double init_radius = Rcpp::as<double>(args["init_radius"]);
    const double epsilon = Rcpp::as<double>(args["epsilon"]);
    const double error_thresh = Rcpp::as<double>(args["error"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);
    const bool show_exceptions = Rcpp::as<bool>(args["show_exceptions"]);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    stanr::r_data_context init_ctx(init_list);
    stanr::r_discard_writer init_writer;
    stanr::r_sample_writer param_writer;
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    // diagnose returns number of failed parameters (not error code)
    int n_failed = stan::services::diagnose::diagnose(
        model, init_ctx, seed, chain_id, init_radius,
        epsilon, error_thresh,
        interrupt, logger,
        init_writer, param_writer);

    logger.flush();

    Rcpp::CharacterVector output(
        logger.history().begin(),
        logger.history().end()
    );
    Rcpp::CharacterVector gradient_lines(
        param_writer.messages().begin(),
        param_writer.messages().end()
    );

    return Rcpp::List::create(
      Rcpp::_["num_failed"] = n_failed,
      Rcpp::_["return_code"] = n_failed == 0 ? 0 : 1,
      Rcpp::_["output"] = output,
      Rcpp::_["gradient_lines"] = gradient_lines
    );
  }
}

#endif
