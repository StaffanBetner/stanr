#ifndef NEWSTAN_RUN_PATHFINDER
#define NEWSTAN_RUN_PATHFINDER

#include <Rcpp.h>
#include <stan/callbacks/stream_logger.hpp>
//#include <stan/services/pathfinder/multi.hpp>
#include <stan/services/pathfinder/single.hpp>
#include "get_arg.hpp"
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_data_context.hpp"

namespace newstan {
  template <class Model>
  Rcpp::List run_pathfinder(Model& model, Rcpp::List args) {
    unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
    unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
    double init_radius    = Rcpp::as<double>(args["init_radius"]);
    int iter              = get_int(args, "iter", 500);
    int history_size      = get_int(args, "history_size", 5);
    int num_elbo_draws    = get_int(args, "num_elbo_draws", 64);
    int num_draws         = get_int(args, "num_draws", 300);
    bool verbose          = Rcpp::as<bool>(args["verbose"]);

    Rcpp::List data_list  = Rcpp::as<Rcpp::List>(args["data"]);
    Rcpp::List init_list  = args.containsElementNamed("init")
                        ? Rcpp::as<Rcpp::List>(args["init"])
                        : data_list;

    newstan::r_data_context init_ctx(init_list);
    newstan::r_sample_writer sample_writer;
    newstan::r_metric_writer metric_writer;
    stan::callbacks::stream_logger logger(Rcpp::Rcout, Rcpp::Rcout, Rcpp::Rcout,
                                          Rcpp::Rcerr, Rcpp::Rcerr);
    newstan::r_interrupt interrupt;

    int return_code = stan::services::pathfinder::pathfinder_lbfgs_single<false, false>(
        model, init_ctx, seed, chain_id, init_radius,
        history_size, 0.001,  // init_alpha, default
        1e-12, 10000.0, 1e-8, 1e7, 1e-8,  // tolerances
        iter, num_elbo_draws, num_draws, false,
        get_int(args, "refresh", 100),
        interrupt, logger,
        sample_writer, sample_writer,
        metric_writer,
        true);  // calculate_lp

    return Rcpp::List::create(
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "pathfinder"
    );
  }
}

#endif
