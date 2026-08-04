#include <Rcpp.h>
#include <stan/model/model_base.hpp>
#include <stanr/r_data_context.hpp>
#include <stanr/model_methods.hpp>

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppParallel)]]

// [[Rcpp::export]]
Rcpp::XPtr<stan::model::model_base> new_model(Rcpp::List data, unsigned int seed) {
  stanr::r_data_context data_context(data);
  // Sampling services may execute this model on a native worker thread.
  // A generated model keeps this stream pointer, so it cannot point at an R
  // stream even though construction itself occurs on the R thread.
  Rcpp::XPtr<stan::model::model_base> m(
      new stan_model(data_context, seed, &stanr::worker_safe_stream()));
  return m;
}

// [[Rcpp::export]]
Rcpp::List run_model(Rcpp::XPtr<stan::model::model_base> model, Rcpp::List args) {
  return stanr::run_model(*model, args);
}

// [[Rcpp::export]]
Rcpp::CharacterVector constrained_param_names(
  Rcpp::XPtr<stan::model::model_base> model) {
  return stanr::model_constrained_names(*model, false, false);
}

// [[Rcpp::export]]
Rcpp::XPtr<stan::rng_t> new_base_rng(unsigned int seed) {
  return stanr::make_base_rng(seed);
}

// [[Rcpp::export]]
int model_num_upars(Rcpp::XPtr<stan::model::model_base> model) {
  return stanr::model_num_upars(*model);
}

// [[Rcpp::export]]
Rcpp::List model_param_metadata(Rcpp::XPtr<stan::model::model_base> model) {
  return stanr::model_param_metadata(*model);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_constrained_names(
    Rcpp::XPtr<stan::model::model_base> model, bool include_tparams = true,
    bool include_gqs = true) {
  return stanr::model_constrained_names(*model, include_tparams, include_gqs);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_unconstrained_names(
    Rcpp::XPtr<stan::model::model_base> model) {
  return stanr::model_unconstrained_names(*model);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_log_prob(Rcpp::XPtr<stan::model::model_base> model,
                                   Rcpp::NumericVector upars,
                                   bool jacobian = true) {
  return stanr::model_log_prob(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_grad_log_prob(Rcpp::XPtr<stan::model::model_base> model,
                                        Rcpp::NumericVector upars,
                                        bool jacobian = true) {
  return stanr::model_grad_log_prob(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::List model_hessian(Rcpp::XPtr<stan::model::model_base> model,
                         Rcpp::NumericVector upars,
                         bool jacobian = true) {
  return stanr::model_hessian(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_unconstrain(Rcpp::XPtr<stan::model::model_base> model,
                                      Rcpp::List variables) {
  return stanr::model_unconstrain(*model, variables);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix model_unconstrain_matrix(
    Rcpp::XPtr<stan::model::model_base> model, Rcpp::NumericMatrix values) {
  return stanr::model_unconstrain_matrix(*model, values);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_constrain(Rcpp::XPtr<stan::model::model_base> model,
                                    Rcpp::XPtr<stan::rng_t> rng,
                                    Rcpp::NumericVector upars,
                                    bool include_tparams = true,
                                    bool include_gqs = true) {
  return stanr::model_constrain(*model, *rng, upars, include_tparams,
                                  include_gqs);
}

// [[Rcpp::export]]
void select_opencl_device(int platform_id, int device_id) {
#ifdef STAN_OPENCL
  stan::math::opencl_context.select_device(platform_id, device_id);
#else
  Rcpp::stop("This model was not compiled with OpenCL support; "
             "create it with stan_model(use_opencl = TRUE).");
#endif
}
