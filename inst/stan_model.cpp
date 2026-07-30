#include <Rcpp.h>
#include <newstan/r_data_context.hpp>

namespace newstan {
  Rcpp::List run_model(stan::model::model_base& model, Rcpp::List args);

  Rcpp::CharacterVector constrained_param_names(const stan::model::model_base& model);
}  // namespace newstan

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppParallel)]]

// [[Rcpp::export]]
Rcpp::XPtr<stan_model> new_model(Rcpp::List data, unsigned int seed) {
  newstan::r_data_context data_context(data);
  Rcpp::XPtr<stan_model> m(new stan_model(data_context, seed, &Rcpp::Rcout));
  return m;
}

// [[Rcpp::export]]
Rcpp::List run_model(Rcpp::XPtr<stan_model> model, Rcpp::List args) {
  return newstan::run_model(*model, args);
}

// [[Rcpp::export]]
Rcpp::CharacterVector constrained_param_names(
  Rcpp::XPtr<stan_model> model) {
  return newstan::constrained_param_names(*model);
}
