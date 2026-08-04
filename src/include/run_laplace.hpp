#ifndef STANR_RUN_LAPLACE_HPP
#define STANR_RUN_LAPLACE_HPP

#include <Rcpp.h>
#include <stan/services/optimize/laplace_sample.hpp>
#include <stan/callbacks/json_writer.hpp>
#include <sstream>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace stanr {
  template <class Model>
  Rcpp::List run_laplace(Model& model, Rcpp::List args) {
    const Eigen::VectorXd mode = Rcpp::as<Eigen::VectorXd>(args["mode"]);
    const int draws = Rcpp::as<int>(args["num_draws"]);
    const bool jacobian = Rcpp::as<bool>(args["jacobian"]);
    const bool calculate_lp = Rcpp::as<bool>(args["calculate_lp"]);
    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const int refresh = Rcpp::as<int>(args["refresh"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);
    const bool show_exceptions = Rcpp::as<bool>(args["show_exceptions"]);

    stanr::r_sample_writer sample_writer(draws);
    stan::callbacks::json_writer<std::ostringstream> hessian_writer(
        std::make_unique<std::ostringstream>());
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    int return_code;
    if (jacobian) {
      return_code = stan::services::laplace_sample<true>(
          model, mode, draws, calculate_lp, seed, refresh, interrupt, logger,
          sample_writer, hessian_writer);
    } else {
      return_code = stan::services::laplace_sample<false>(
          model, mode, draws, calculate_lp, seed, refresh, interrupt, logger,
          sample_writer, hessian_writer);
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
