#ifndef STANR_MODEL_METHODS_HPP
#define STANR_MODEL_METHODS_HPP

#include <Rcpp.h>
#include <stan/model/model_base.hpp>
#include <stan/services/util/create_rng.hpp>

namespace stanr {
  Rcpp::List run_model(stan::model::model_base& model, Rcpp::List args);

  Rcpp::XPtr<stan::rng_t> make_base_rng(unsigned int seed);
  int model_num_upars(const stan::model::model_base& model);
  Rcpp::List model_param_metadata(const stan::model::model_base& model);
  Rcpp::CharacterVector model_constrained_names(const stan::model::model_base& model, bool include_tparams, bool include_gqs);
  Rcpp::CharacterVector model_unconstrained_names(const stan::model::model_base& model);
  Rcpp::NumericVector model_log_prob(const stan::model::model_base& model, Rcpp::NumericVector values, bool jacobian);
  Rcpp::NumericVector model_grad_log_prob(const stan::model::model_base& model, Rcpp::NumericVector values, bool jacobian);
  Rcpp::List model_hessian(const stan::model::model_base& model, Rcpp::NumericVector values, bool jacobian);
  Rcpp::NumericVector model_unconstrain(const stan::model::model_base& model, Rcpp::List variables, SEXP declarations);
  Rcpp::NumericMatrix model_unconstrain_matrix(const stan::model::model_base& model, Rcpp::NumericMatrix values);
  Rcpp::NumericVector model_constrain(const stan::model::model_base& model, stan::rng_t& rng, Rcpp::NumericVector values, bool include_tparams, bool include_gqs);
  Rcpp::NumericMatrix model_constrain_matrix(const stan::model::model_base& model, stan::rng_t& rng, Rcpp::NumericMatrix values, bool include_tparams, bool include_gqs);
  Rcpp::List model_constrain_variables(const stan::model::model_base& model, stan::rng_t& rng, Rcpp::NumericVector values, bool include_tparams, bool include_gqs, Rcpp::List declarations);
  Rcpp::List model_variable_skeleton(const stan::model::model_base& model, bool include_tparams, bool include_gqs, Rcpp::List declarations);

  // Generated models keep their constructor's ostream pointer and can write
  // through it on a native/TBB worker; this sink bypasses R's console API.
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
}  // namespace stanr

#endif  // STANR_MODEL_METHODS_HPP
