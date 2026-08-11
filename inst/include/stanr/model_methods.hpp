#ifndef STANR_MODEL_METHODS_HPP
#define STANR_MODEL_METHODS_HPP

#include <R_ext/Print.h>
#include <cpp11.hpp>
#include <stan/model/model_base.hpp>
#include <stan/services/util/create_rng.hpp>

namespace stanr {
  cpp11::writable::list run_model(stan::model::model_base& model, cpp11::list args);

  cpp11::external_pointer<stan::rng_t> make_base_rng(unsigned int seed);
  int model_num_upars(const stan::model::model_base& model);
  cpp11::writable::list model_param_metadata(const stan::model::model_base& model);
  cpp11::writable::strings model_constrained_names(const stan::model::model_base& model, bool include_tparams, bool include_gqs);
  cpp11::writable::strings model_unconstrained_names(const stan::model::model_base& model);
  cpp11::writable::doubles model_log_prob(const stan::model::model_base& model, cpp11::doubles values, bool jacobian);
  cpp11::writable::doubles model_grad_log_prob(const stan::model::model_base& model, cpp11::doubles values, bool jacobian);
  cpp11::writable::list model_hessian(const stan::model::model_base& model, cpp11::doubles values, bool jacobian);
  cpp11::writable::doubles model_unconstrain(const stan::model::model_base& model, cpp11::list variables, SEXP declarations);
  cpp11::writable::doubles_matrix<> model_unconstrain_matrix(const stan::model::model_base& model, cpp11::doubles_matrix<> values);
  cpp11::writable::doubles model_constrain(const stan::model::model_base& model, stan::rng_t& rng, cpp11::doubles values, bool include_tparams, bool include_gqs);
  cpp11::writable::doubles_matrix<> model_constrain_matrix(const stan::model::model_base& model, stan::rng_t& rng, cpp11::doubles_matrix<> values, bool include_tparams, bool include_gqs);
  cpp11::writable::list model_constrain_variables(const stan::model::model_base& model, stan::rng_t& rng, cpp11::doubles values, bool include_tparams, bool include_gqs, cpp11::list declarations);
  cpp11::writable::list model_variable_skeleton(const stan::model::model_base& model, bool include_tparams, bool include_gqs, cpp11::list declarations);

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

  // Exposed-function wrappers run synchronously on the main R thread, so
  // (unlike worker_safe_stream()) Stan print/reject statements should
  // actually reach the R console.
  class r_console_streambuf : public std::streambuf {
  protected:
    int_type overflow(int_type ch) override {
      if (ch != traits_type::eof()) {
        char c = static_cast<char>(ch);
        Rprintf("%.*s", 1, &c);
      }
      return ch;
    }
    std::streamsize xsputn(const char* s, std::streamsize n) override {
      Rprintf("%.*s", static_cast<int>(n), s);
      return n;
    }
  };

  inline std::ostream& r_console_stream() {
    static r_console_streambuf buffer;
    static std::ostream stream(&buffer);
    return stream;
  }
}  // namespace stanr

#endif  // STANR_MODEL_METHODS_HPP
