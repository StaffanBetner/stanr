#ifndef NEWSTAN_RUN_ADVI_HPP
#define NEWSTAN_RUN_ADVI_HPP

#include <Rcpp.h>
#include <stan/services/experimental/advi/fullrank.hpp>
#include <stan/services/experimental/advi/meanfield.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include <newstan/r_data_context.hpp>
#include "r_logger.hpp"

namespace newstan {
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

    Rcpp::List data_list = Rcpp::as<Rcpp::List>(args["data"]);
    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    newstan::r_data_context init_ctx(init_list);
    // Stan writes the posterior mean followed by output_samples draws.
    newstan::r_sample_writer sample_writer(output_samples + 1);
    newstan::r_discard_writer diagnostic_writer;
    newstan::r_logger logger(verbose);
    newstan::r_interrupt interrupt;

    int return_code = stan::services::error_codes::CONFIG;

    if (algorithm == "fullrank") {
      return_code = stan::services::experimental::advi::fullrank(
          model, init_ctx, seed, chain_id, init_radius,
          grad_samples, elbo_samples, iter, tol_rel_obj, eta,
          adapt_engaged, adapt_iter, eval_elbo, output_samples,
          interrupt, logger,
          sample_writer, sample_writer,
          diagnostic_writer);
    } else if (algorithm == "meanfield") {
      return_code = stan::services::experimental::advi::meanfield(
          model, init_ctx, seed, chain_id, init_radius,
          grad_samples, elbo_samples, iter, tol_rel_obj, eta,
          adapt_engaged, adapt_iter, eval_elbo, output_samples,
          interrupt, logger,
          sample_writer, sample_writer,
          diagnostic_writer);
    }

    logger.flush();
    return Rcpp::List::create(
      Rcpp::_["draws"] = sample_writer.to_r_matrix(),
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "variational",
      Rcpp::_["algorithm"] = algorithm
    );
  }
}

#endif
