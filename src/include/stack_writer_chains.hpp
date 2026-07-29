#ifndef NEWSTAN_STACK_WRITER_CHAINS_HPP
#define NEWSTAN_STACK_WRITER_CHAINS_HPP

#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Dense>
#include <cstring>

namespace newstan {

  // Helper: stack per-chain Eigen matrices and add a .chain column.
  // Uses Eigen vertical concatenation for fast, vectorized stacking.
  template <typename Writer>
  inline Rcpp::DataFrame stack_writer_chains(
    const std::vector<Writer>& writers, int num_chains) {

    int total_rows = 0;
    int n_cols = 0;
    for (int i = 0; i < num_chains; ++i) {
      total_rows += writers[i].n_rows();
      if (i == 0) n_cols = writers[i].n_cols();
    }

    if (total_rows == 0) {
      return Rcpp::DataFrame::create();
    }

    // Build the final R columns directly.  Avoiding an intermediate combined
    // Eigen matrix saves a full-draw-set allocation and copy.
    Rcpp::List df_list(n_cols + 1);
    for (int j = 0; j < n_cols; ++j) {
      df_list[j] = Rcpp::NumericVector(total_rows);
    }

    int offset = 0;
    for (int i = 0; i < num_chains; ++i) {
      writers[i].copy_to_r_columns(df_list, offset);
      offset += writers[i].n_rows();
    }

    // Append chain ID as the last column
    Rcpp::IntegerVector chain_col(total_rows);
    offset = 0;
    for (int i = 0; i < num_chains; ++i) {
      int n = writers[i].n_rows();
      std::fill(chain_col.begin() + offset, chain_col.begin() + offset + n, i + 1);
      offset += n;
    }
    df_list[n_cols] = chain_col;

    Rcpp::DataFrame df = Rcpp::DataFrame(df_list);

    // Set column names from first chain's writer + ".chain"
    Rcpp::CharacterVector names(n_cols + 1);
    if (num_chains > 0 && !writers[0].colnames().empty()) {
      for (int j = 0; j < n_cols; ++j) {
        names[j] = writers[0].colnames()[j];
      }
    }
    names[n_cols] = ".chain";
    df.names() = names;

    return df;
  }
}

#endif
