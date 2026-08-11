#ifndef STANR_RUN_STANDALONE_GQS_HPP
#define STANR_RUN_STANDALONE_GQS_HPP

#include <cpp11.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
#include <stan/services/sample/standalone_gqs.hpp>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace stanr {
  template <class Model>
  cpp11::writable::list run_standalone_gqs(Model& model, cpp11::list args) {
    const unsigned int seed = stanr::as_cpp<unsigned int>(args["seed"]);
    const bool verbose = stanr::as_cpp<bool>(args["verbose"]);
    const bool show_exceptions = stanr::as_cpp<bool>(args["show_exceptions"]);
    const int nchains = stanr::as_cpp<int>(args["nchains"]);

    // draws: Eigen::MatrixXd (rows=samples, columns=parameters)
    const Eigen::Map<Eigen::MatrixXd> draws =
        stanr::as_cpp<Eigen::Map<Eigen::MatrixXd>>(args["draws"]);

    stanr::r_sample_writer sample_writer(static_cast<int>(draws.rows()));
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    const int return_code = stan::services::standalone_generate(
        model, draws, seed,
        interrupt, logger, sample_writer);

    logger.flush();
    cpp11::writable::strings output = cpp11::as_sexp(logger.history());

    // Reshape output into iterations x chains x variables array
    const int n_rows = sample_writer.n_rows();
    const int n_cols = sample_writer.n_cols();
    const int n_iters = n_rows / nchains;
    const std::vector<std::string> colnames = sample_writer.colnames();

    cpp11::writable::doubles samples(
        static_cast<R_xlen_t>(n_iters) * nchains * n_cols);
    samples.attr("dim") =
        cpp11::writable::integers({n_iters, nchains, n_cols});
    samples.attr("dimnames") = cpp11::writable::list(
        {R_NilValue, R_NilValue, cpp11::as_sexp(colnames)});
    samples.attr("class") = cpp11::writable::strings(
        {"draws_array", "draws", "array"});

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

    return cpp11::writable::list({
      cpp11::named_arg("samples") = samples,
      cpp11::named_arg("return_code") = return_code,
      cpp11::named_arg("output") = output
    });
  }
}

#endif
