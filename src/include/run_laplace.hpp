#ifndef NEWSTAN_RUN_LAPLACE_HPP
#define NEWSTAN_RUN_LAPLACE_HPP

#include <Rcpp.h>
#include <stan/services/optimize/laplace_sample.hpp>
#include <stan/callbacks/json_writer.hpp>
#include <sstream>
#include "get_arg.hpp"
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace newstan {
  template <class Model>
  Rcpp::List run_laplace(Model& model, Rcpp::List args) {
    Eigen::VectorXd mode = Rcpp::as<Eigen::VectorXd>(args["mode"]);
    int draws = get_int(args, "draws", 1000);
    bool jacobian = get_bool(args, "jacobian", true);
    bool calculate_lp = get_bool(args, "calculate_lp", true);
    unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    int refresh = get_int(args, "refresh", 100);
    bool verbose = get_bool(args, "verbose", true);

    newstan::r_sample_writer sample_writer(draws);
    stan::callbacks::json_writer<std::ostringstream> hessian_writer(
        std::make_unique<std::ostringstream>());
    newstan::r_logger logger(verbose);
    newstan::r_interrupt interrupt;

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
    return Rcpp::List::create(
      Rcpp::_["draws"] = sample_writer.to_r_matrix(),
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "laplace"
    );
  }
}

#endif
