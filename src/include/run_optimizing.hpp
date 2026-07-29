#ifndef NEWSTAN_RUN_OPTIMIZING_HPP
#define NEWSTAN_RUN_OPTIMIZING_HPP

#include <Rcpp.h>
#include <stan/callbacks/stream_logger.hpp>
#include <stan/services/optimize/bfgs.hpp>
#include <stan/services/optimize/lbfgs.hpp>
#include <stan/services/optimize/newton.hpp>
#include "get_arg.hpp"
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_data_context.hpp"

namespace newstan {
  template <class Model>
  Rcpp::List run_optimizing(Model& model, Rcpp::List args) {
    std::string algorithm = get_string(args, "algorithm", "bfgs");

    unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
    unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
    double init_radius    = Rcpp::as<double>(args["init_radius"]);
    int iter              = get_int(args, "iter", 2000);
    bool save_iterations  = Rcpp::as<bool>(args["save_iterations"]);
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

    if (algorithm == "newton") {
      return_code = stan::services::optimize::newton(
          model, init_ctx, seed, chain_id, init_radius,
          iter, save_iterations,
          interrupt, logger,
          /*init_writer=*/sample_writer, sample_writer);
    } else if (algorithm == "bfgs") {
      double init_alpha      = get_double(args, "init_alpha", 0.001);
      double tol_obj         = get_double(args, "tol_obj", 1e-12);
      double tol_rel_obj     = get_double(args, "tol_rel_obj", 10000.0);
      double tol_grad        = get_double(args, "tol_grad", 1e-8);
      double tol_rel_grad    = get_double(args, "tol_rel_grad", 1e7);
      double tol_param       = get_double(args, "tol_param", 1e-8);
      int refresh            = get_int(args, "refresh", 100);

      return_code = stan::services::optimize::bfgs(
          model, init_ctx, seed, chain_id, init_radius,
          init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
          iter, save_iterations, refresh,
          interrupt, logger,
          /*init_writer=*/sample_writer, sample_writer);
    } else if (algorithm == "lbfgs") {
      double init_alpha      = get_double(args, "init_alpha", 0.001);
      double tol_obj         = get_double(args, "tol_obj", 1e-12);
      double tol_rel_obj     = get_double(args, "tol_rel_obj", 10000.0);
      double tol_grad        = get_double(args, "tol_grad", 1e-8);
      double tol_rel_grad    = get_double(args, "tol_rel_grad", 1e7);
      double tol_param       = get_double(args, "tol_param", 1e-8);
      int history_size       = get_int(args, "history_size", 5);
      int refresh            = get_int(args, "refresh", 100);

      return_code = stan::services::optimize::lbfgs(
          model, init_ctx, seed, chain_id, init_radius,
          history_size, init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
          iter, save_iterations, refresh,
          interrupt, logger,
          /*init_writer=*/sample_writer, sample_writer);
    } else {
      std::ostringstream msg;
      msg << "Unknown optimization algorithm: " << algorithm;
      logger.error(msg.str());
      return_code = stan::services::error_codes::CONFIG;
    }

    // Extract results from sample_writer
    Rcpp::DataFrame df = sample_writer.to_dataframe();
    Rcpp::NumericVector par_vec;
    double lp_val = NA_REAL;

    if (df.nrow() > 0 && df.ncol() >= 1) {
      // Last row contains the solution; first column is lp__
      int last_row = df.nrow() - 1;
      Rcpp::CharacterVector colnames = df.names();
      for (int j = 0; j < df.ncol(); ++j) {
        if (colnames[j] == "lp__") {
          Rcpp::NumericVector col = df[j];
          lp_val = col[last_row];
          break;
        }
      }
    }

    return Rcpp::List::create(
      Rcpp::_["par"] = df,
      Rcpp::_["value"] = lp_val,
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "optimizing",
      Rcpp::_["algorithm"] = algorithm
    );
  }
}

#endif
