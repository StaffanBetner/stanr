#include <stan/model/model_base.hpp>
#include <stanr/r_data_context.hpp>
#include <stanr/model_methods.hpp>
#include <cpp11.hpp>
#include <cpp11/declarations.hpp>
#include <stanr/cpp11_tuple_interop.hpp>

// Compiled directly by `R CMD SHLIB` (no attribute processing): every
// export is an `extern "C"` SEXP routine bound by name via
// getNativeSymbolInfo(). Bodies are wrapped in BEGIN_CPP11/END_CPP11 so
// exceptions surface as R errors. Kept in sync with
// `.stanr_model_support_exports` (R/stan_model.R).

extern "C" {

SEXP new_model(SEXP data, SEXP seed, SEXP declarations) {
  BEGIN_CPP11
  stanr::r_data_context data_context(cpp11::list(data), declarations);
  // Model runs on a native worker thread, so the stream can't be an R stream.
  cpp11::external_pointer<stan::model::model_base> m(
      new stan_model(data_context, stanr::as_cpp<unsigned int>(seed),
                     &stanr::worker_safe_stream()));
  return m;
  END_CPP11
}

SEXP run_model(SEXP model, SEXP args) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::run_model(*m, cpp11::list(args));
  END_CPP11
}

SEXP constrained_param_names(SEXP model) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_constrained_names(*m, false, false);
  END_CPP11
}

SEXP new_base_rng(SEXP seed) {
  BEGIN_CPP11
  return stanr::make_base_rng(stanr::as_cpp<unsigned int>(seed));
  END_CPP11
}

SEXP model_num_upars(SEXP model) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return cpp11::as_sexp(stanr::model_num_upars(*m));
  END_CPP11
}

SEXP model_param_metadata(SEXP model) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_param_metadata(*m);
  END_CPP11
}

SEXP model_constrained_names(SEXP model, SEXP include_tparams,
                             SEXP include_gqs) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_constrained_names(
      *m, stanr::as_cpp<bool>(include_tparams), stanr::as_cpp<bool>(include_gqs));
  END_CPP11
}

SEXP model_unconstrained_names(SEXP model) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_unconstrained_names(*m);
  END_CPP11
}

SEXP model_log_prob(SEXP model, SEXP upars, SEXP jacobian) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_log_prob(
      *m, cpp11::doubles(upars), stanr::as_cpp<bool>(jacobian));
  END_CPP11
}

SEXP model_grad_log_prob(SEXP model, SEXP upars, SEXP jacobian) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_grad_log_prob(
      *m, cpp11::doubles(upars), stanr::as_cpp<bool>(jacobian));
  END_CPP11
}

SEXP model_hessian(SEXP model, SEXP upars, SEXP jacobian) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_hessian(
      *m, cpp11::doubles(upars), stanr::as_cpp<bool>(jacobian));
  END_CPP11
}

SEXP model_unconstrain(SEXP model, SEXP variables, SEXP declarations) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_unconstrain(
      *m, cpp11::list(variables), declarations);
  END_CPP11
}

SEXP model_unconstrain_matrix(SEXP model, SEXP values) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_unconstrain_matrix(
      *m, cpp11::doubles_matrix<>(values));
  END_CPP11
}

SEXP model_constrain(SEXP model, SEXP rng, SEXP upars, SEXP include_tparams,
                     SEXP include_gqs) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  cpp11::external_pointer<stan::rng_t> r(rng);
  return stanr::model_constrain(
      *m, *r, cpp11::doubles(upars),
      stanr::as_cpp<bool>(include_tparams), stanr::as_cpp<bool>(include_gqs));
  END_CPP11
}

SEXP model_constrain_matrix(SEXP model, SEXP rng, SEXP upars,
                            SEXP include_tparams, SEXP include_gqs) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  cpp11::external_pointer<stan::rng_t> r(rng);
  return stanr::model_constrain_matrix(
      *m, *r, cpp11::doubles_matrix<>(upars),
      stanr::as_cpp<bool>(include_tparams), stanr::as_cpp<bool>(include_gqs));
  END_CPP11
}

SEXP model_constrain_variables(SEXP model, SEXP rng, SEXP upars,
                               SEXP include_tparams, SEXP include_gqs,
                               SEXP declarations) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  cpp11::external_pointer<stan::rng_t> r(rng);
  return stanr::model_constrain_variables(
      *m, *r, cpp11::doubles(upars),
      stanr::as_cpp<bool>(include_tparams), stanr::as_cpp<bool>(include_gqs),
      cpp11::list(declarations));
  END_CPP11
}

SEXP model_variable_skeleton(SEXP model, SEXP include_tparams,
                             SEXP include_gqs, SEXP declarations) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_variable_skeleton(
      *m, stanr::as_cpp<bool>(include_tparams), stanr::as_cpp<bool>(include_gqs),
      cpp11::list(declarations));
  END_CPP11
}

SEXP select_opencl_device(SEXP platform_id, SEXP device_id) {
  BEGIN_CPP11
#ifdef STAN_OPENCL
  stan::math::opencl_context.select_device(stanr::as_cpp<int>(platform_id),
                                           stanr::as_cpp<int>(device_id));
#else
  cpp11::stop("This model was not compiled with OpenCL support; "
             "create it with stan_model(use_opencl = TRUE).");
#endif
  return R_NilValue;
  END_CPP11
}

}  // extern "C"
