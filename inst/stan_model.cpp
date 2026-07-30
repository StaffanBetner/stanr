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
  // Sampling services may execute this model on a native worker thread.
  // A generated model keeps this stream pointer, so it cannot point at an R
  // stream even though construction itself occurs on the R thread.
  Rcpp::XPtr<stan_model> m(
      new stan_model(data_context, seed, &newstan::worker_safe_stream()));
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
