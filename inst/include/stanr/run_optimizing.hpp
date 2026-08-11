#ifndef STANR_RUN_OPTIMIZING_HPP
#define STANR_RUN_OPTIMIZING_HPP

#include <cpp11.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
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
                                cpp11::list args, InitContext& init_ctx,
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
      const double init_alpha = stanr::as_cpp<double>(args["init_alpha"]);
      const double tol_obj = stanr::as_cpp<double>(args["tol_obj"]);
      const double tol_rel_obj = stanr::as_cpp<double>(args["tol_rel_obj"]);
      const double tol_grad = stanr::as_cpp<double>(args["tol_grad"]);
      const double tol_rel_grad = stanr::as_cpp<double>(args["tol_rel_grad"]);
      const double tol_param = stanr::as_cpp<double>(args["tol_param"]);
      const int refresh = stanr::as_cpp<int>(args["refresh"]);

      return stan::services::optimize::bfgs<Model, Jacobian>(
          model, init_ctx, seed, chain_id, init_radius,
          init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
          iter, save_iterations, refresh,
          interrupt, logger,
          init_writer, sample_writer);
    } else if (algorithm == "lbfgs") {
      const double init_alpha = stanr::as_cpp<double>(args["init_alpha"]);
      const double tol_obj = stanr::as_cpp<double>(args["tol_obj"]);
      const double tol_rel_obj = stanr::as_cpp<double>(args["tol_rel_obj"]);
      const double tol_grad = stanr::as_cpp<double>(args["tol_grad"]);
      const double tol_rel_grad = stanr::as_cpp<double>(args["tol_rel_grad"]);
      const double tol_param = stanr::as_cpp<double>(args["tol_param"]);
      const int history_size = stanr::as_cpp<int>(args["history_size"]);
      const int refresh = stanr::as_cpp<int>(args["refresh"]);

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
  cpp11::writable::list run_optimizing(Model& model, cpp11::list args) {
    const std::string algorithm = stanr::as_cpp<std::string>(args["algorithm"]);

    const unsigned int seed = stanr::as_cpp<unsigned int>(args["seed"]);
    const unsigned int chain_id = stanr::as_cpp<unsigned int>(args["id"]);
    const double init_radius = stanr::as_cpp<double>(args["init_radius"]);
    const int iter = stanr::as_cpp<int>(args["iter"]);
    const bool save_iterations = stanr::as_cpp<bool>(args["save_iterations"]);
    const bool jacobian = stanr::as_cpp<bool>(args["jacobian"]);
    const bool verbose = stanr::as_cpp<bool>(args["verbose"]);
    const bool show_exceptions = stanr::as_cpp<bool>(args["show_exceptions"]);

    cpp11::list init_list = args["init"];

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

    cpp11::writable::doubles_matrix<> mat = sample_writer.to_r_matrix();
    double lp_val = NA_REAL;

    if (mat.nrow() > 0 && mat.ncol() >= 1) {
      // Last row contains the solution; find lp__ column
      cpp11::list dimnames = SEXP(mat.attr("dimnames"));
      cpp11::strings colnames = dimnames[1];
      for (int j = 0; j < mat.ncol(); ++j) {
        if (colnames[j] == "lp__") {
          lp_val = mat(mat.nrow() - 1, j);
          break;
        }
      }
    }

    logger.flush();
    cpp11::writable::strings output = cpp11::as_sexp(logger.history());
    return cpp11::writable::list({
      cpp11::named_arg("par") = mat,
      cpp11::named_arg("value") = lp_val,
      cpp11::named_arg("return_code") = return_code,
      cpp11::named_arg("output") = output
    });
  }
}

#endif
