#ifndef NEWSTAN_STACK_WRITER_CHAINS_HPP
#define NEWSTAN_STACK_WRITER_CHAINS_HPP


#include <Rcpp.h>

namespace newstan {

  // Helper: stack per-chain dataframes and add a .chain column
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

    // Build column vectors from stacked per-chain data
    Rcpp::List df_list(n_cols);
    for (int j = 0; j < n_cols; ++j) {
      Rcpp::NumericVector col(total_rows);
      int offset = 0;
      for (int i = 0; i < num_chains; ++i) {
        auto df_i = writers[i].to_dataframe();
        Rcpp::NumericVector col_i = Rcpp::as<Rcpp::NumericVector>(df_i[j]);
        for (int k = 0; k < col_i.size(); ++k) {
          col[offset + k] = col_i[k];
        }
        offset += col_i.size();
      }
      df_list[j] = col;
    }

    // Append chain ID as the last column
    Rcpp::IntegerVector chain_col(total_rows);
    int offset = 0;
    for (int i = 0; i < num_chains; ++i) {
      int n = writers[i].n_rows();
      for (int j = 0; j < n; ++j) chain_col[offset + j] = i + 1;
      offset += n;
    }
    df_list.push_back(chain_col);

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
