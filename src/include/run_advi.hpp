#ifndef NEWSTAN_RUN_ADVI_HPP
#define NEWSTAN_RUN_ADVI_HPP

#include <Rcpp.h>
#include <stan/callbacks/stream_logger.hpp>
#include <stan/services/experimental/advi/fullrank.hpp>
#include <stan/services/experimental/advi/meanfield.hpp>
#include "get_arg.hpp"
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_data_context.hpp"

namespace newstan {
  template <class Model>
  Rcpp::List run_advi(Model& model, Rcpp::List args) {
    std::string algorithm = get_string(args, "algorithm", "fullrank");

    unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
    unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
    double init_radius    = Rcpp::as<double>(args["init_radius"]);
    int iter              = get_int(args, "iter", 10000);
    int grad_samples      = get_int(args, "grad_samples", 1);
    int elbo_samples      = get_int(args, "elbo_samples", 100);
    double tol_rel_obj    = get_double(args, "tol_rel_obj", 0.01);
    double eta            = get_double(args, "eta", 1.0);
    bool adapt_engaged    = Rcpp::as<bool>(args["adapt_engaged"]);
    int adapt_iter        = get_int(args, "adapt_iter", 50);
    int eval_elbo         = get_int(args, "eval_elbo", 100);
    int output_samples    = get_int(args, "output_samples", 1000);
    bool verbose          = Rcpp::as<bool>(args["verbose"]);

    Rcpp::List data_list  = Rcpp::as<Rcpp::List>(args["data"]);
    Rcpp::List init_list  = args.containsElementNamed("init")
                        ? Rcpp::as<Rcpp::List>(args["init"])
                        : data_list;

    newstan::r_data_context init_ctx(init_list);
    newstan::r_sample_writer sample_writer;
    stan::callbacks::stream_logger logger(Rcpp::Rcout, Rcpp::Rcout, Rcpp::Rcout,
                                          Rcpp::Rcerr, Rcpp::Rcerr);
    newstan::r_interrupt interrupt;

    int return_code = stan::services::error_codes::CONFIG;

    if (algorithm == "fullrank") {
      return_code = stan::services::experimental::advi::fullrank(
          model, init_ctx, seed, chain_id, init_radius,
          grad_samples, elbo_samples, iter, tol_rel_obj, eta,
          adapt_engaged, adapt_iter, eval_elbo, output_samples,
          interrupt, logger,
          /*init_writer=*/sample_writer, sample_writer,
          /*diagnostic_writer=*/sample_writer);
    } else if (algorithm == "meanfield") {
      return_code = stan::services::experimental::advi::meanfield(
          model, init_ctx, seed, chain_id, init_radius,
          grad_samples, elbo_samples, iter, tol_rel_obj, eta,
          adapt_engaged, adapt_iter, eval_elbo, output_samples,
          interrupt, logger,
          /*init_writer=*/sample_writer, sample_writer,
          /*diagnostic_writer=*/sample_writer);
    }

    return Rcpp::List::create(
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "advi",
      Rcpp::_["algorithm"] = algorithm
    );
  }
}

#endif
