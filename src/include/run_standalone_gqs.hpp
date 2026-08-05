#ifndef STANR_RUN_STANDALONE_GQS_HPP
#define STANR_RUN_STANDALONE_GQS_HPP

#include <Rcpp.h>
#include <RcppEigen.h>
#include <stan/services/sample/standalone_gqs.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace stanr {
  template <class Model>
  Rcpp::List run_standalone_gqs(Model& model, Rcpp::List args) {
    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);
    const bool show_exceptions = Rcpp::as<bool>(args["show_exceptions"]);
    const int nchains = Rcpp::as<int>(args["nchains"]);

    // draws: Eigen::MatrixXd (rows=samples, columns=parameters)
    const Eigen::Map<Eigen::MatrixXd> draws =
        Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(args["draws"]);

    stanr::r_sample_writer sample_writer(static_cast<int>(draws.rows()));
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    const int return_code = stan::services::standalone_generate(
        model, draws, seed,
        interrupt, logger, sample_writer);

    logger.flush();
    Rcpp::CharacterVector output(logger.history().begin(), logger.history().end());

    // Reshape output into iterations x chains x variables array
    const int n_rows = sample_writer.n_rows();
    const int n_cols = sample_writer.n_cols();
    const int n_iters = n_rows / nchains;
    const std::vector<std::string> colnames = sample_writer.colnames();

    Rcpp::NumericVector samples(
        static_cast<R_xlen_t>(n_iters) * nchains * n_cols);
    samples.attr("dim") =
        Rcpp::IntegerVector::create(n_iters, nchains, n_cols);
    samples.attr("dimnames") = Rcpp::List::create(
        R_NilValue, R_NilValue,
        Rcpp::CharacterVector(colnames.begin(), colnames.end()));
    samples.attr("class") = Rcpp::CharacterVector::create(
        "draws_array", "draws", "array");

    // Copy data in column-major order: for each variable, interleave chains
    // Source layout (column-major): chain 1 iters, chain 2 iters, ...
    // Dest layout (column-major): iter 1 chain 1..N, iter 2 chain 1..N, ...
    if (n_rows > 0) {
      for (int v = 0; v < n_cols; ++v) {
        const double* src = sample_writer.column_ptr(v);
        double* dst = REAL(samples) + static_cast<size_t>(v) * n_iters * nchains;
        for (int c = 0; c < nchains; ++c) {
          for (int i = 0; i < n_iters; ++i) {
            dst[i * nchains + c] = src[c * n_iters + i];
          }
        }
      }
    }

    return Rcpp::List::create(
      Rcpp::_["samples"] = samples,
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["output"] = output
    );
  }
}

#endif
