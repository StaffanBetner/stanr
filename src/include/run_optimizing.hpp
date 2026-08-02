#ifndef NEWSTAN_RUN_OPTIMIZING_HPP
#define NEWSTAN_RUN_OPTIMIZING_HPP

#include <Rcpp.h>
#include <stan/services/optimize/bfgs.hpp>
#include <stan/services/optimize/lbfgs.hpp>
#include <stan/services/optimize/newton.hpp>
#include "r_output.hpp"
#include "r_logger.hpp"
#include "r_interrupt.hpp"
#include <newstan/r_data_context.hpp>

namespace newstan {
  template <class Model>
  Rcpp::List run_optimizing(Model& model, Rcpp::List args) {
    const std::string algorithm = Rcpp::as<std::string>(args["algorithm"]);

    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const unsigned int chain_id = Rcpp::as<unsigned int>(args["id"]);
    const double init_radius = Rcpp::as<double>(args["init_radius"]);
    const int iter = Rcpp::as<int>(args["iter"]);
    const bool save_iterations = Rcpp::as<bool>(args["save_iterations"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    newstan::r_data_context init_ctx(init_list);
    // stan::services::util::initialize() writes the unconstrained initial
    // values to init_writer as a raw numeric row with no preceding
    // column-name header, before the algorithm ever touches parameter_writer;
    // this package doesn't use them.
    newstan::r_discard_writer init_writer;
    // With saved iterations Stan writes the initial point plus at most iter
    // updates; otherwise it writes only the final point.
    newstan::r_sample_writer sample_writer(save_iterations ? iter + 1 : 1);
    newstan::r_logger logger(verbose);
    newstan::r_interrupt interrupt;

    int return_code = stan::services::error_codes::CONFIG;

    if (algorithm == "newton") {
      return_code = stan::services::optimize::newton(
          model, init_ctx, seed, chain_id, init_radius,
          iter, save_iterations,
          interrupt, logger,
          init_writer, sample_writer);
    } else if (algorithm == "bfgs") {
      const double init_alpha = Rcpp::as<double>(args["init_alpha"]);
      const double tol_obj = Rcpp::as<double>(args["tol_obj"]);
      const double tol_rel_obj = Rcpp::as<double>(args["tol_rel_obj"]);
      const double tol_grad = Rcpp::as<double>(args["tol_grad"]);
      const double tol_rel_grad = Rcpp::as<double>(args["tol_rel_grad"]);
      const double tol_param = Rcpp::as<double>(args["tol_param"]);
      const int refresh = Rcpp::as<int>(args["refresh"]);

      return_code = stan::services::optimize::bfgs(
          model, init_ctx, seed, chain_id, init_radius,
          init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
          iter, save_iterations, refresh,
          interrupt, logger,
          init_writer, sample_writer);
    } else if (algorithm == "lbfgs") {
      const double init_alpha = Rcpp::as<double>(args["init_alpha"]);
      const double tol_obj = Rcpp::as<double>(args["tol_obj"]);
      const double tol_rel_obj = Rcpp::as<double>(args["tol_rel_obj"]);
      const double tol_grad = Rcpp::as<double>(args["tol_grad"]);
      const double tol_rel_grad = Rcpp::as<double>(args["tol_rel_grad"]);
      const double tol_param = Rcpp::as<double>(args["tol_param"]);
      const int history_size = Rcpp::as<int>(args["history_size"]);
      const int refresh = Rcpp::as<int>(args["refresh"]);

      return_code = stan::services::optimize::lbfgs(
          model, init_ctx, seed, chain_id, init_radius,
          history_size, init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
          iter, save_iterations, refresh,
          interrupt, logger,
          init_writer, sample_writer);
    } else {
      std::ostringstream msg;
      msg << "Unknown optimization algorithm: " << algorithm;
      logger.error(msg.str());
      return_code = stan::services::error_codes::CONFIG;
    }

    // Extract results from sample_writer
    Rcpp::NumericMatrix mat = sample_writer.to_r_matrix();
    double lp_val = NA_REAL;

    if (mat.nrow() > 0 && mat.ncol() >= 1) {
      // Last row contains the solution; find lp__ column
      Rcpp::List dimnames = mat.attr("dimnames");
      Rcpp::CharacterVector colnames = Rcpp::as<Rcpp::CharacterVector>(
        Rcpp::wrap(dimnames[1]));
      for (int j = 0; j < mat.ncol(); ++j) {
        if (colnames[j] == "lp__") {
          lp_val = mat(mat.nrow() - 1, j);
          break;
        }
      }
    }

    logger.flush();
    Rcpp::CharacterVector output(logger.history().begin(), logger.history().end());
    return Rcpp::List::create(
      Rcpp::_["par"] = mat,
      Rcpp::_["value"] = lp_val,
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "optimize",
      Rcpp::_["algorithm"] = algorithm,
      Rcpp::_["output"] = output
    );
  }
}

#endif
