#ifndef STANR_STACK_WRITER_CHAINS_HPP
#define STANR_STACK_WRITER_CHAINS_HPP

#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Dense>
#include <algorithm>
#include <cstring>
#include <stdexcept>
#include <string>
#include <vector>
#include "r_output.hpp"

namespace stanr {

  // Stacks per-chain r_sample_writers into iterations x chains x variables R
  // arrays (Fortran/column-major order, matching posterior::draws_array).
  // Columns are routed into a parameter array and a diagnostics array by
  // name membership in diagnostic_names, preserving each group's original
  // relative column order, and the first `num_warmup_rows` iterations of
  // each chain are split into separate warmup arrays (NULL when zero).
  // Each chain's writer buffer is released right after its columns are
  // copied, so peak native memory during this step is one destination copy
  // plus one remaining source chain buffer.
  inline Rcpp::List writer_chains_to_arrays(
      std::vector<r_sample_writer>& writers,
      const std::vector<std::string>& diagnostic_names,
      int num_warmup_rows) {

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

    const int n_warmup = std::min(num_warmup_rows, n_iterations);
    const int n_post = n_iterations - n_warmup;

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

    const auto make_array = [&](int iterations, int variables,
                                const std::vector<std::string>& names) {
      Rcpp::NumericVector array(
          static_cast<R_xlen_t>(iterations) * num_chains * variables);
      array.attr("dim") =
          Rcpp::IntegerVector::create(iterations, num_chains, variables);
      array.attr("dimnames") = Rcpp::List::create(
          R_NilValue, R_NilValue,
          Rcpp::CharacterVector(names.begin(), names.end()));
      array.attr("class") = Rcpp::CharacterVector::create(
          "draws_array", "draws", "array");
      return array;
    };

    Rcpp::NumericVector samples = make_array(n_post, n_params, param_names);
    Rcpp::NumericVector diagnostics =
        make_array(n_post, n_diagnostics, diag_names);
    Rcpp::NumericVector warmup_samples;
    Rcpp::NumericVector warmup_diagnostics;
    if (n_warmup > 0) {
      warmup_samples = make_array(n_warmup, n_params, param_names);
      warmup_diagnostics = make_array(n_warmup, n_diagnostics, diag_names);
    }

    // Column layout is iteration-major within a chain buffer, so the warmup
    // rows are a contiguous prefix of each source column.
    const auto copy_column = [&](Rcpp::NumericVector& post,
                                 Rcpp::NumericVector& warmup,
                                 const double* source, int chain, int group) {
      if (n_warmup > 0) {
        std::memcpy(
            REAL(warmup) +
                (static_cast<size_t>(group) * num_chains + chain) * n_warmup,
            source, static_cast<size_t>(n_warmup) * sizeof(double));
      }
      std::memcpy(
          REAL(post) +
              (static_cast<size_t>(group) * num_chains + chain) * n_post,
          source + n_warmup, static_cast<size_t>(n_post) * sizeof(double));
    };

    if (n_iterations > 0) {
      for (int i = 0; i < num_chains; ++i) {
        for (int g = 0; g < n_params; ++g) {
          copy_column(samples, warmup_samples,
                      writers[i].column_ptr(param_cols[g]), i, g);
        }
        for (int g = 0; g < n_diagnostics; ++g) {
          copy_column(diagnostics, warmup_diagnostics,
                      writers[i].column_ptr(diag_cols[g]), i, g);
        }
        writers[i].release();
      }
    }

    return Rcpp::List::create(
        Rcpp::_["samples"] = samples,
        Rcpp::_["diagnostics"] = diagnostics,
        Rcpp::_["warmup_samples"] =
            n_warmup > 0 ? SEXP(warmup_samples) : R_NilValue,
        Rcpp::_["warmup_diagnostics"] =
            n_warmup > 0 ? SEXP(warmup_diagnostics) : R_NilValue);
  }
}

#endif
