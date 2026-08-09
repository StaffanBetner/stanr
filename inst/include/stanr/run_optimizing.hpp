#ifndef STANR_RUN_OPTIMIZING_HPP
#define STANR_RUN_OPTIMIZING_HPP

#include <Rcpp.h>
#include <stan/services/optimize/bfgs.hpp>
#include <stan/services/optimize/lbfgs.hpp>
#include <stan/services/optimize/newton.hpp>
#include "r_output.hpp"
#include "r_logger.hpp"
#include "r_interrupt.hpp"
#include <stanr/r_data_context.hpp>

namespace stanr {
  template <bool Jacobian, class Model, class InitContext, class InitWriter,
            class SampleWriter, class Logger, class Interrupt>
  int run_optimizing_algorithm(Model& model, const std::string& algorithm,
                                Rcpp::List args, InitContext& init_ctx,
                                unsigned int seed, unsigned int chain_id,
                                double init_radius, int iter,
                                bool save_iterations, Interrupt& interrupt,
                                Logger& logger, InitWriter& init_writer,
                                SampleWriter& sample_writer) {
    if (algorithm == "newton") {
      return stan::services::optimize::newton<Model, Jacobian>(
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

      return stan::services::optimize::bfgs<Model, Jacobian>(
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

      return stan::services::optimize::lbfgs<Model, Jacobian>(
          model, init_ctx, seed, chain_id, init_radius,
          history_size, init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
          iter, save_iterations, refresh,
          interrupt, logger,
          init_writer, sample_writer);
    } else {
      std::ostringstream msg;
      msg << "Unknown optimization algorithm: " << algorithm;
      logger.error(msg.str());
      return stan::services::error_codes::CONFIG;
    }
  }

  template <class Model>
  Rcpp::List run_optimizing(Model& model, Rcpp::List args) {
    const std::string algorithm = Rcpp::as<std::string>(args["algorithm"]);

    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const unsigned int chain_id = Rcpp::as<unsigned int>(args["id"]);
    const double init_radius = Rcpp::as<double>(args["init_radius"]);
    const int iter = Rcpp::as<int>(args["iter"]);
    const bool save_iterations = Rcpp::as<bool>(args["save_iterations"]);
    const bool jacobian = Rcpp::as<bool>(args["jacobian"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);
    const bool show_exceptions = Rcpp::as<bool>(args["show_exceptions"]);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    stanr::r_data_context init_ctx(init_list, args["init_declarations"]);
    stan::callbacks::writer init_writer;
    // With saved iterations Stan writes the initial point plus at most iter
    // updates; otherwise it writes only the final point.
    stanr::r_sample_writer sample_writer(save_iterations ? iter + 1 : 1);
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    int return_code = jacobian
        ? run_optimizing_algorithm<true>(
              model, algorithm, args, init_ctx, seed, chain_id, init_radius,
              iter, save_iterations, interrupt, logger, init_writer,
              sample_writer)
        : run_optimizing_algorithm<false>(
              model, algorithm, args, init_ctx, seed, chain_id, init_radius,
              iter, save_iterations, interrupt, logger, init_writer,
              sample_writer);

    Rcpp::NumericMatrix mat = sample_writer.to_r_matrix();
    double lp_val = NA_REAL;

    if (mat.nrow() > 0 && mat.ncol() >= 1) {
      // Last row contains the solution; find lp__ column
      Rcpp::List dimnames = mat.attr("dimnames");
      Rcpp::CharacterVector colnames = dimnames[1];
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
      Rcpp::_["output"] = output
    );
  }
}

#endif
