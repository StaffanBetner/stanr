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

    // Pre-allocate combined matrix with Eigen
    Eigen::MatrixXd combined(total_rows, n_cols);

    // Vertical concatenation using Eigen block assignment (vectorized memcpy)
    int offset = 0;
    for (int i = 0; i < num_chains; ++i) {
      Eigen::MatrixXd const& mat = writers[i].to_matrix();
      int n = writers[i].n_rows();
      if (n > 0) {
        combined.block(offset, 0, n, n_cols) = mat;
        offset += n;
      }
    }

    // Build R data.frame from combined matrix columns
    Rcpp::List df_list(n_cols + 1);
    for (int j = 0; j < n_cols; ++j) {
      Rcpp::NumericVector col(total_rows);
      std::memcpy(col.begin(), combined.col(j).data(),
                  static_cast<size_t>(total_rows) * sizeof(double));
      df_list[j] = col;
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
