#ifndef NEWSTAN_STACK_WRITER_CHAINS_HPP
#define NEWSTAN_STACK_WRITER_CHAINS_HPP

#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Dense>
#include <algorithm>
#include <cstring>
#include <stdexcept>
#include <string>
#include <vector>
#include "r_output.hpp"

namespace newstan {

  // Stacks per-chain r_sample_writers into iterations x chains x variables R
  // arrays (Fortran/column-major order, matching posterior::draws_array).
  // Columns are routed into a parameter array and a diagnostics array by
  // name membership in diagnostic_names, preserving each group's original
  // relative column order. Each chain's writer buffer is released right
  // after its columns are copied, so peak native memory during this step is
  // one destination copy plus one remaining source chain buffer, rather
  // than two full combined-array copies.
  inline Rcpp::List writer_chains_to_arrays(
      std::vector<r_sample_writer>& writers,
      const std::vector<std::string>& diagnostic_names) {

    const int num_chains = static_cast<int>(writers.size());
    const int n_iterations = num_chains > 0 ? writers[0].n_rows() : 0;
    const int n_cols = num_chains > 0 ? writers[0].n_cols() : 0;

    for (int i = 0; i < num_chains; ++i) {
      if (writers[i].n_rows() != n_iterations || writers[i].n_cols() != n_cols) {
        throw std::runtime_error(
            "writer_chains_to_arrays: chain " + std::to_string(i) +
            " has dimensions (" + std::to_string(writers[i].n_rows()) + ", " +
            std::to_string(writers[i].n_cols()) +
            ") which do not match chain 0's dimensions (" +
            std::to_string(n_iterations) + ", " + std::to_string(n_cols) + ").");
      }
    }

    const std::vector<std::string> colnames =
        num_chains > 0 ? writers[0].colnames() : std::vector<std::string>();

    std::vector<int> param_cols;
    std::vector<int> diag_cols;
    std::vector<std::string> param_names;
    std::vector<std::string> diag_names;
    for (int v = 0; v < n_cols; ++v) {
      const bool is_diag = std::find(diagnostic_names.begin(),
                                      diagnostic_names.end(),
                                      colnames[v]) != diagnostic_names.end();
      if (is_diag) {
        diag_cols.push_back(v);
        diag_names.push_back(colnames[v]);
      } else {
        param_cols.push_back(v);
        param_names.push_back(colnames[v]);
      }
    }

    const int n_params = static_cast<int>(param_cols.size());
    const int n_diagnostics = static_cast<int>(diag_cols.size());

    Rcpp::NumericVector samples(
        static_cast<R_xlen_t>(n_iterations) * num_chains * n_params);
    Rcpp::NumericVector diagnostics(
        static_cast<R_xlen_t>(n_iterations) * num_chains * n_diagnostics);

    if (n_iterations > 0) {
      for (int i = 0; i < num_chains; ++i) {
        for (int g = 0; g < n_params; ++g) {
          std::memcpy(
              REAL(samples) +
                  (static_cast<size_t>(g) * num_chains + i) * n_iterations,
              writers[i].column_ptr(param_cols[g]),
              static_cast<size_t>(n_iterations) * sizeof(double));
        }
        for (int g = 0; g < n_diagnostics; ++g) {
          std::memcpy(
              REAL(diagnostics) +
                  (static_cast<size_t>(g) * num_chains + i) * n_iterations,
              writers[i].column_ptr(diag_cols[g]),
              static_cast<size_t>(n_iterations) * sizeof(double));
        }
        writers[i].release();
      }
    }

    samples.attr("dim") =
        Rcpp::IntegerVector::create(n_iterations, num_chains, n_params);
    samples.attr("dimnames") = Rcpp::List::create(
        R_NilValue, R_NilValue,
        Rcpp::CharacterVector(param_names.begin(), param_names.end()));

    diagnostics.attr("dim") =
        Rcpp::IntegerVector::create(n_iterations, num_chains, n_diagnostics);
    diagnostics.attr("dimnames") = Rcpp::List::create(
        R_NilValue, R_NilValue,
        Rcpp::CharacterVector(diag_names.begin(), diag_names.end()));

    return Rcpp::List::create(Rcpp::_["samples"] = samples,
                               Rcpp::_["diagnostics"] = diagnostics);
  }
}

#endif
