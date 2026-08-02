#ifndef NEWSTAN_RUN_STANDALONE_GQS_HPP
#define NEWSTAN_RUN_STANDALONE_GQS_HPP

#include <Rcpp.h>
#include <stan/services/sample/standalone_gqs.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace newstan {
  template <class Model>
  Rcpp::List run_standalone_gqs(Model& model, Rcpp::List args) {
    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);

    // draws: Eigen::MatrixXd (rows=samples, columns=parameters)
    const Eigen::Map<Eigen::MatrixXd> draws =
        Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(args["draws"]);

    newstan::r_sample_writer sample_writer(static_cast<int>(draws.rows()));
    newstan::r_logger logger(verbose);
    newstan::r_interrupt interrupt;

    const int return_code = stan::services::standalone_generate(
        model, draws, seed,
        interrupt, logger, sample_writer);

    logger.flush();
    Rcpp::CharacterVector output(logger.history().begin(), logger.history().end());
    return Rcpp::List::create(
      Rcpp::_["samples"] = sample_writer.to_r_matrix(),
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "generate_quantities",
      Rcpp::_["output"] = output
    );
  }
}

#endif
