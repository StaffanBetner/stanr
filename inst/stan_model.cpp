#include <stan/model/model_base.hpp>
#include <stanr/r_data_context.hpp>
#include <stanr/model_methods.hpp>
#include <Rcpp.h>
#include <RcppEigen.h>

// Compiled directly by `R CMD SHLIB` (no Rcpp attribute processing), so
// every export is an `extern "C"` routine taking/returning SEXP. The R side
// binds these by name via getNativeSymbolInfo() and calls them as .Call
// routines. Each body is wrapped in BEGIN_RCPP/END_RCPP (as Rcpp's generated
// RcppExports.cpp would) so Rcpp exceptions surface as R errors rather than
// crashing R. Kept in sync by hand with `.stanr_model_support_exports`
// (R/stan_model.R).

extern "C" {

SEXP new_model(SEXP data, SEXP seed, SEXP declarations) {
  BEGIN_RCPP
  stanr::r_data_context data_context(Rcpp::as<Rcpp::List>(data), declarations);
  // Sampling services may execute this model on a native worker thread.
  // A generated model keeps this stream pointer, so it cannot point at an R
  // stream even though construction itself occurs on the R thread.
  Rcpp::XPtr<stan::model::model_base> m(
      new stan_model(data_context, Rcpp::as<unsigned int>(seed),
                     &stanr::worker_safe_stream()));
  return Rcpp::wrap(m);
  END_RCPP
}

SEXP run_model(SEXP model, SEXP args) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::run_model(*m, Rcpp::as<Rcpp::List>(args)));
  END_RCPP
}

SEXP constrained_param_names(SEXP model) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_constrained_names(*m, false, false));
  END_RCPP
}

SEXP new_base_rng(SEXP seed) {
  BEGIN_RCPP
  return Rcpp::wrap(stanr::make_base_rng(Rcpp::as<unsigned int>(seed)));
  END_RCPP
}

SEXP model_num_upars(SEXP model) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_num_upars(*m));
  END_RCPP
}

SEXP model_param_metadata(SEXP model) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_param_metadata(*m));
  END_RCPP
}

SEXP model_constrained_names(SEXP model, SEXP include_tparams,
                             SEXP include_gqs) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_constrained_names(
      *m, Rcpp::as<bool>(include_tparams), Rcpp::as<bool>(include_gqs)));
  END_RCPP
}

SEXP model_unconstrained_names(SEXP model) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_unconstrained_names(*m));
  END_RCPP
}

SEXP model_log_prob(SEXP model, SEXP upars, SEXP jacobian) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_log_prob(
      *m, Rcpp::as<Rcpp::NumericVector>(upars), Rcpp::as<bool>(jacobian)));
  END_RCPP
}

SEXP model_grad_log_prob(SEXP model, SEXP upars, SEXP jacobian) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_grad_log_prob(
      *m, Rcpp::as<Rcpp::NumericVector>(upars), Rcpp::as<bool>(jacobian)));
  END_RCPP
}

SEXP model_hessian(SEXP model, SEXP upars, SEXP jacobian) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_hessian(
      *m, Rcpp::as<Rcpp::NumericVector>(upars), Rcpp::as<bool>(jacobian)));
  END_RCPP
}

SEXP model_unconstrain(SEXP model, SEXP variables, SEXP declarations) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_unconstrain(
      *m, Rcpp::as<Rcpp::List>(variables), declarations));
  END_RCPP
}

SEXP model_unconstrain_matrix(SEXP model, SEXP values) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_unconstrain_matrix(
      *m, Rcpp::as<Rcpp::NumericMatrix>(values)));
  END_RCPP
}

SEXP model_constrain(SEXP model, SEXP rng, SEXP upars, SEXP include_tparams,
                     SEXP include_gqs) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  Rcpp::XPtr<stan::rng_t> r(rng);
  return Rcpp::wrap(stanr::model_constrain(
      *m, *r, Rcpp::as<Rcpp::NumericVector>(upars),
      Rcpp::as<bool>(include_tparams), Rcpp::as<bool>(include_gqs)));
  END_RCPP
}

SEXP model_constrain_matrix(SEXP model, SEXP rng, SEXP upars,
                            SEXP include_tparams, SEXP include_gqs) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  Rcpp::XPtr<stan::rng_t> r(rng);
  return Rcpp::wrap(stanr::model_constrain_matrix(
      *m, *r, Rcpp::as<Rcpp::NumericMatrix>(upars),
      Rcpp::as<bool>(include_tparams), Rcpp::as<bool>(include_gqs)));
  END_RCPP
}

SEXP model_constrain_variables(SEXP model, SEXP rng, SEXP upars,
                               SEXP include_tparams, SEXP include_gqs,
                               SEXP declarations) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  Rcpp::XPtr<stan::rng_t> r(rng);
  return Rcpp::wrap(stanr::model_constrain_variables(
      *m, *r, Rcpp::as<Rcpp::NumericVector>(upars),
      Rcpp::as<bool>(include_tparams), Rcpp::as<bool>(include_gqs),
      Rcpp::as<Rcpp::List>(declarations)));
  END_RCPP
}

SEXP model_variable_skeleton(SEXP model, SEXP include_tparams,
                             SEXP include_gqs, SEXP declarations) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> m(model);
  return Rcpp::wrap(stanr::model_variable_skeleton(
      *m, Rcpp::as<bool>(include_tparams), Rcpp::as<bool>(include_gqs),
      Rcpp::as<Rcpp::List>(declarations)));
  END_RCPP
}

SEXP select_opencl_device(SEXP platform_id, SEXP device_id) {
  BEGIN_RCPP
#ifdef STAN_OPENCL
  stan::math::opencl_context.select_device(Rcpp::as<int>(platform_id),
                                           Rcpp::as<int>(device_id));
#else
  Rcpp::stop("This model was not compiled with OpenCL support; "
             "create it with stan_model(use_opencl = TRUE).");
#endif
  return R_NilValue;
  END_RCPP
}

}  // extern "C"
