#include <Rcpp.h>
#include <stan/model/model_base.hpp>
#include <newstan/r_data_context.hpp>

namespace newstan {
  Rcpp::List run_model(stan::model::model_base& model, Rcpp::List args);

  Rcpp::XPtr<stan::rng_t> make_base_rng(unsigned int seed);
  int model_num_upars(const stan::model::model_base& model);
  Rcpp::List model_param_metadata(const stan::model::model_base& model);
  Rcpp::CharacterVector model_constrained_names(const stan::model::model_base& model, bool include_tparams, bool include_gqs);
  Rcpp::CharacterVector model_unconstrained_names(const stan::model::model_base& model);
  Rcpp::NumericVector model_log_prob(const stan::model::model_base& model, Rcpp::NumericVector values, bool jacobian);
  Rcpp::NumericVector model_grad_log_prob(const stan::model::model_base& model, Rcpp::NumericVector values, bool jacobian);
  Rcpp::List model_hessian(const stan::model::model_base& model, Rcpp::NumericVector values, bool jacobian);
  Rcpp::NumericVector model_unconstrain(const stan::model::model_base& model, Rcpp::List variables);
  Rcpp::NumericMatrix model_unconstrain_matrix(const stan::model::model_base& model, Rcpp::NumericMatrix values);
  Rcpp::NumericVector model_constrain(const stan::model::model_base& model, stan::rng_t& rng, Rcpp::NumericVector values, bool include_tparams, bool include_gqs);
  Rcpp::CharacterVector model_compile_info(const stan::model::model_base& model);

  // Generated Stan models retain their constructor's ostream pointer and can
  // write through it while evaluating on a native/TBB worker.  This sink has
  // static lifetime and deliberately bypasses R's console API.
  class null_streambuf : public std::streambuf {
  protected:
    int_type overflow(int_type ch) override { return traits_type::not_eof(ch); }
    std::streamsize xsputn(const char*, std::streamsize n) override { return n; }
  };

  inline std::ostream& worker_safe_stream() {
    static null_streambuf buffer;
    static std::ostream stream(&buffer);
    return stream;
  }
}  // namespace newstan

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppParallel)]]

// [[Rcpp::export]]
Rcpp::XPtr<stan::model::model_base> new_model(Rcpp::List data, unsigned int seed) {
  newstan::r_data_context data_context(data);
  // Sampling services may execute this model on a native worker thread.
  // A generated model keeps this stream pointer, so it cannot point at an R
  // stream even though construction itself occurs on the R thread.
  Rcpp::XPtr<stan::model::model_base> m(
      new stan_model(data_context, seed, &newstan::worker_safe_stream()));
  return m;
}

// [[Rcpp::export]]
Rcpp::List run_model(Rcpp::XPtr<stan::model::model_base> model, Rcpp::List args) {
  return newstan::run_model(*model, args);
}

// [[Rcpp::export]]
Rcpp::CharacterVector constrained_param_names(
  Rcpp::XPtr<stan::model::model_base> model) {
  return newstan::model_constrained_names(*model, false, false);
}

// [[Rcpp::export]]
Rcpp::XPtr<stan::rng_t> new_base_rng(unsigned int seed) {
  return newstan::make_base_rng(seed);
}

// [[Rcpp::export]]
int model_num_upars(Rcpp::XPtr<stan::model::model_base> model) {
  return newstan::model_num_upars(*model);
}

// [[Rcpp::export]]
Rcpp::List model_param_metadata(Rcpp::XPtr<stan::model::model_base> model) {
  return newstan::model_param_metadata(*model);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_constrained_names(
    Rcpp::XPtr<stan::model::model_base> model, bool include_tparams = true,
    bool include_gqs = true) {
  return newstan::model_constrained_names(*model, include_tparams, include_gqs);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_unconstrained_names(
    Rcpp::XPtr<stan::model::model_base> model) {
  return newstan::model_unconstrained_names(*model);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_log_prob(Rcpp::XPtr<stan::model::model_base> model,
                                   Rcpp::NumericVector upars,
                                   bool jacobian = true) {
  return newstan::model_log_prob(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_grad_log_prob(Rcpp::XPtr<stan::model::model_base> model,
                                        Rcpp::NumericVector upars,
                                        bool jacobian = true) {
  return newstan::model_grad_log_prob(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::List model_hessian(Rcpp::XPtr<stan::model::model_base> model,
                         Rcpp::NumericVector upars,
                         bool jacobian = true) {
  return newstan::model_hessian(*model, upars, jacobian);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_unconstrain(Rcpp::XPtr<stan::model::model_base> model,
                                      Rcpp::List variables) {
  return newstan::model_unconstrain(*model, variables);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix model_unconstrain_matrix(
    Rcpp::XPtr<stan::model::model_base> model, Rcpp::NumericMatrix values) {
  return newstan::model_unconstrain_matrix(*model, values);
}

// [[Rcpp::export]]
Rcpp::NumericVector model_constrain(Rcpp::XPtr<stan::model::model_base> model,
                                    Rcpp::XPtr<stan::rng_t> rng,
                                    Rcpp::NumericVector upars,
                                    bool include_tparams = true,
                                    bool include_gqs = true) {
  return newstan::model_constrain(*model, *rng, upars, include_tparams,
                                  include_gqs);
}

// [[Rcpp::export]]
Rcpp::CharacterVector model_compile_info(Rcpp::XPtr<stan::model::model_base> model) {
  return newstan::model_compile_info(*model);
}
