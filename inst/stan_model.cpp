#include <Rcpp.h>
#include <newstan/model_methods.hpp>
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

// [[Rcpp::export]]
Rcpp::XPtr<stan::rng_t> new_base_rng(unsigned int seed) {
  return newstan::make_base_rng(seed);
}

// [[Rcpp::export]]
int model_num_upars(Rcpp::XPtr<stan_model> model) {
  return newstan::model_num_upars(*model);
}

// [[Rcpp::export]]
Rcpp::List model_param_metadata(Rcpp::XPtr<stan_model> model) {
  return newstan::model_param_metadata(*model);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_constrained_names(
    Rcpp::XPtr<stan_model> model, bool include_tparams = true,
    bool include_gqs = true) {
  return newstan::model_constrained_names(*model, include_tparams, include_gqs);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_unconstrained_names(
    Rcpp::XPtr<stan_model> model) {
  return newstan::model_unconstrained_names(*model);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_log_prob(Rcpp::XPtr<stan_model> model,
                                   Rcpp::NumericVector upars,
                                   bool jacobian = true) {
  return newstan::model_log_prob(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_grad_log_prob(Rcpp::XPtr<stan_model> model,
                                        Rcpp::NumericVector upars,
                                        bool jacobian = true) {
  return newstan::model_grad_log_prob(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::List model_hessian(Rcpp::XPtr<stan_model> model,
                         Rcpp::NumericVector upars,
                         bool jacobian = true) {
  return newstan::model_hessian(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_unconstrain(Rcpp::XPtr<stan_model> model,
                                      Rcpp::List variables) {
  return newstan::model_unconstrain(*model, variables);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix model_unconstrain_matrix(
    Rcpp::XPtr<stan_model> model, Rcpp::NumericMatrix values) {
  return newstan::model_unconstrain_matrix(*model, values);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_constrain(Rcpp::XPtr<stan_model> model,
                                    Rcpp::XPtr<stan::rng_t> rng,
                                    Rcpp::NumericVector upars,
                                    bool include_tparams = true,
                                    bool include_gqs = true) {
  return newstan::model_constrain(*model, *rng, upars, include_tparams,
                                  include_gqs);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_compile_info(Rcpp::XPtr<stan_model> model) {
  return newstan::model_compile_info(*model);
}
