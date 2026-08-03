#ifndef NEWSTAN_STACK_WRITER_CHAINS_HPP
#define NEWSTAN_STACK_WRITER_CHAINS_HPP

#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Dense>
#include <cstring>
#include <stdexcept>
#include <string>

namespace newstan {

  // Stack per-chain writers into a single iterations x chains x variables
  // R array (Fortran/column-major order, matching posterior::draws_array).
  // Avoids building an intermediate combined Eigen matrix or data.frame.
  template <typename Writer>
  inline Rcpp::NumericVector writer_chains_to_array(
    const std::vector<Writer>& writers) {

    const int num_chains = static_cast<int>(writers.size());

    const int n_iterations = num_chains > 0 ? writers[0].n_rows() : 0;
    const int n_cols = num_chains > 0 ? writers[0].n_cols() : 0;

    for (int i = 0; i < num_chains; ++i) {
      if (writers[i].n_rows() != n_iterations || writers[i].n_cols() != n_cols) {
        throw std::runtime_error(
            "writer_chains_to_array: chain " + std::to_string(i) +
            " has dimensions (" + std::to_string(writers[i].n_rows()) + ", " +
            std::to_string(writers[i].n_cols()) +
            ") which do not match chain 0's dimensions (" +
            std::to_string(n_iterations) + ", " + std::to_string(n_cols) + ").");
      }
    }

    Rcpp::CharacterVector colnames;
    if (num_chains > 0 && !writers[0].colnames().empty()) {
      colnames = Rcpp::CharacterVector(
          writers[0].colnames().begin(), writers[0].colnames().end());
    } else {
      colnames = Rcpp::CharacterVector(n_cols);
    }

    Rcpp::NumericVector result(
        static_cast<R_xlen_t>(n_iterations) * num_chains * n_cols);

    if (n_iterations > 0) {
      for (int i = 0; i < num_chains; ++i) {
        writers[i].copy_to_r_array_chain(REAL(result), n_iterations, num_chains, i);
      }
    }

    result.attr("dim") = Rcpp::IntegerVector::create(n_iterations, num_chains, n_cols);
    result.attr("dimnames") = Rcpp::List::create(R_NilValue, R_NilValue, colnames);

    return result;
  }
}

#endif
