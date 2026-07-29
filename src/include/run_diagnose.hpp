#ifndef NEWSTAN_RUN_DIAGNOSE_HPP
#define NEWSTAN_RUN_DIAGNOSE_HPP

#include <Rcpp.h>
#include <stan/services/diagnose/diagnose.hpp>
#include "get_arg.hpp"
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include "r_data_context.hpp"

namespace newstan {
  template <class Model>
  Rcpp::List run_diagnose(Model& model, Rcpp::List args) {
    unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
    unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
    double init_radius    = Rcpp::as<double>(args["init_radius"]);
    double epsilon        = get_double(args, "epsilon", 1e-6);
    double error_thresh   = get_double(args, "error", 1e-6);
    bool verbose          = Rcpp::as<bool>(args["verbose"]);

    Rcpp::List init_list = args.containsElementNamed("init")
                        ? Rcpp::as<Rcpp::List>(args["init"])
                        : Rcpp::as<Rcpp::List>(args["data"]);

    newstan::r_data_context init_ctx(init_list);
    newstan::r_sample_writer sample_writer;
    newstan::r_logger logger(verbose);
    newstan::r_interrupt interrupt;

    // diagnose returns number of failed parameters (not error code)
    int n_failed = stan::services::diagnose::diagnose(
        model, init_ctx, seed, chain_id, init_radius,
        epsilon, error_thresh,
        interrupt, logger,
        /*init_writer=*/sample_writer, sample_writer);

    logger.flush();
    return Rcpp::List::create(
      Rcpp::_["num_failed"] = n_failed,
      Rcpp::_["return_code"] = n_failed == 0 ? 0 : 1,
      Rcpp::_["method"] = "diagnose"
    );
  }
}

#endif
