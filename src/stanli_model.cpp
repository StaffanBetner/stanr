#include <stan/model/model_base.hpp>
#include <stan/math/rev/core.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
#include <stanr/model_methods.hpp>
#include <stanli/compile.hpp>
#include <stanli/executor_pool.hpp>
#include <stanli/wa_interp.hpp>

#include <cpp11.hpp>
#include <cpp11/declarations.hpp>

#include <R.h>
#include <Rinternals.h>

#include <Eigen/Dense>

#include <cmath>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace stanr {
namespace {

// R's REAL()/INTEGER() buffers are already column-major for up to 3 dims,
// the same layout stanli::DataMap::Entry expects -- so array data copies
// straight across with no reshaping (unlike JSON, which nests row-major and
// needs stanli's parser to transpose it back).
std::vector<int64_t> value_dims(SEXP value) {
  SEXP dim = Rf_getAttrib(value, R_DimSymbol);
  const int ndims = Rf_length(dim);
  if (ndims == 0) return {};
  if (ndims > 3)
    throw std::runtime_error(
        "stanli data arrays with more than 3 dimensions are not supported");
  std::vector<int64_t> dims(ndims);
  for (int i = 0; i < ndims; ++i) {
    const int d = INTEGER_ELT(dim, i);
    if (d < 0) throw std::runtime_error("invalid stanli data dimensions");
    dims[i] = d;
  }
  return dims;
}

void check_supported(const std::string& name, SEXP value) {
  if (Rf_isNull(value))
    throw std::runtime_error("stanli data value cannot be NULL: " + name);
  if (Rf_inherits(value, "data.frame"))
    throw std::runtime_error("stanli data does not support data.frames yet");
  if (TYPEOF(value) == CPLXSXP)
    throw std::runtime_error("stanli data does not support complex values yet");
  if (TYPEOF(value) != INTSXP && TYPEOF(value) != LGLSXP &&
      TYPEOF(value) != REALSXP) {
    throw std::runtime_error(
        "stanli data must be numeric, logical, or an array (tuple-typed "
        "data is not yet supported): " + name);
  }
}

bool whole_number(double x) {
  return x == std::trunc(x) &&
         x >= static_cast<double>(std::numeric_limits<int>::min()) &&
         x <= static_cast<double>(std::numeric_limits<int>::max());
}

void set_data_entry(stanli::DataMap& out, const std::string& name,
                    SEXP value) {
  check_supported(name, value);
  const std::vector<int64_t> dims = value_dims(value);
  const R_xlen_t n = XLENGTH(value);
  const bool is_real = TYPEOF(value) == REALSXP;

  if (dims.empty() && n == 1) {
    if (is_real) {
      const double x = REAL_ELT(value, 0);
      if (!std::isfinite(x))
        throw std::runtime_error("stanli data cannot contain NA, NaN, or Inf: " + name);
      // Matches r_data_context.cpp's store_numeric(): a whole-number real
      // (the common `y = c(1, 0, 1, ...)` idiom, rather than `1L`) must
      // also be readable wherever int-declared data is read.
      if (whole_number(x)) {
        out.set_int(name, static_cast<int>(x));
      } else {
        out.set_real(name, x);
      }
    } else {
      const int x =
          TYPEOF(value) == LGLSXP ? LOGICAL_ELT(value, 0) : INTEGER_ELT(value, 0);
      if (x == NA_INTEGER)
        throw std::runtime_error("stanli data cannot contain NA: " + name);
      out.set_int(name, x);
    }
    return;
  }

  if (is_real) {
    std::vector<double> values(n);
    bool all_whole = true;
    for (R_xlen_t i = 0; i < n; ++i) {
      values[i] = REAL_ELT(value, i);
      if (!std::isfinite(values[i]))
        throw std::runtime_error("stanli data cannot contain NA, NaN, or Inf: " + name);
      all_whole = all_whole && whole_number(values[i]);
    }
    // Matches r_data_context.cpp's store_numeric(): a whole-number real
    // array is also usable wherever int-declared data is read.
    if (all_whole) {
      std::vector<int> ints(values.size());
      for (size_t i = 0; i < values.size(); ++i) {
        ints[i] = static_cast<int>(values[i]);
      }
      out.set_int_array(name, std::move(ints), dims);
      return;
    }
    out.set_real_array(name, std::move(values), dims);
    return;
  }

  std::vector<int> values(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    const int x =
        TYPEOF(value) == LGLSXP ? LOGICAL_ELT(value, i) : INTEGER_ELT(value, i);
    if (x == NA_INTEGER)
      throw std::runtime_error("stanli data cannot contain NA: " + name);
    values[i] = x;
  }
  out.set_int_array(name, std::move(values), dims);
}

// SEXP -> stanli::DataMap, via DataMap's typed setters. No JSON round trip:
// set_int_array() takes a dims argument (a stanr patch to data.hpp; see
// tools/upgrade_stanli.sh), so a multi-dimensional integer array -- the one
// shape that used to force a fallback through QuickJSR::to_json() plus
// DataMap::from_json() -- now builds directly like everything else.
stanli::DataMap sexp_to_data_map(SEXP data) {
  stanli::DataMap out;
  if (Rf_isNull(data) || XLENGTH(data) == 0) return out;
  SEXP names = Rf_getAttrib(data, R_NamesSymbol);
  if (TYPEOF(data) != VECSXP || Rf_isNull(names))
    throw std::runtime_error("stanli data must be a named list");
  for (R_xlen_t i = 0; i < XLENGTH(data); ++i) {
    const char* name = CHAR(STRING_ELT(names, i));
    if (name[0] == '\0')
      throw std::runtime_error("stanli data list names must be non-empty");
    set_data_entry(out, name, VECTOR_ELT(data, i));
  }
  return out;
}

void append_unc_names(const stanli::CompiledModel::UncParam& p,
                      std::vector<std::string>& out) {
  int64_t product = 1;
  bool sane = true;
  for (const int64_t d : p.dims) {
    if (d < 0) sane = false;
    product *= d;
  }
  if (sane && product == p.len && !p.dims.empty()) {
    std::vector<int64_t> index(p.dims.size(), 0);
    for (int64_t k = 0; k < p.len; ++k) {
      std::string name = p.name;
      for (const int64_t i : index) name += "." + std::to_string(i + 1);
      out.push_back(std::move(name));
      for (size_t d = 0; d < index.size(); ++d) {
        if (++index[d] < p.dims[d]) break;
        index[d] = 0;
      }
    }
    return;
  }
  if (p.dims.empty() && p.len == 1) {
    out.push_back(p.name);
    return;
  }
  for (int64_t i = 0; i < p.len; ++i)
    out.push_back(p.name + "." + std::to_string(i + 1));
}

size_t scalar_count(const std::vector<stanli::CompiledModel::ParamView>& views,
                    size_t first, size_t last) {
  size_t n = 0;
  for (size_t i = first; i < last; ++i) n += static_cast<size_t>(views[i].len);
  return n;
}

}  // namespace

class stanli_model_base final : public stan::model::model_base {
 public:
  stanli_model_base(const std::string& mir, const stanli::DataMap& data,
                    std::string model_name, unsigned int seed)
      : stan::model::model_base(0), name_(std::move(model_name)), seed_(seed) {
    cm_ = stanli::compile_model(mir, data);
    proto_ = std::make_unique<stanli::Executor>(std::move(cm_.graph));
    cm_.bind(*proto_);
    num_params_r__ = static_cast<size_t>(proto_->n_params());
    pool_ = std::make_unique<stanli::ExecutorPool>(*proto_);
    prepare_write_array();
  }

  std::string model_name() const override { return name_; }

  std::vector<std::string> model_compile_info() const override {
    return {"stanli graph interpreter", "model construction without C++ compilation"};
  }

  void get_param_names(std::vector<std::string>& names,
                       bool include_tparams = true,
                       bool include_gqs = true) const override {
    names.clear();
    if (has_write_array_) {
      for (size_t i = 0; i < selected_column_end(include_tparams, include_gqs); ++i)
        names.push_back(wa_columns_[i].name);
    } else {
      for (const auto& v : cm_.views) names.push_back(v.name);
    }
  }

  void get_dims(std::vector<std::vector<size_t>>& dims,
                bool include_tparams = true,
                bool include_gqs = true) const override {
    dims.clear();
    if (has_write_array_) {
      for (size_t i = 0; i < selected_column_end(include_tparams, include_gqs); ++i)
        dims.push_back(column_dims(wa_columns_[i]));
    } else {
      for (const auto& v : cm_.views) dims.push_back(view_dims(v));
    }
  }

  void constrained_param_names(std::vector<std::string>& names,
                               bool include_tparams = true,
                               bool include_gqs = true) const override {
    if (has_write_array_) {
      append_columns(names, 0, selected_column_end(include_tparams, include_gqs));
    } else {
      for (const auto& v : cm_.views) v.append_names(names);
    }
  }

  void unconstrained_param_names(std::vector<std::string>& names,
                                 bool = true, bool = true) const override {
    for (const auto& p : cm_.unc_params) append_unc_names(p, names);
  }

  template <bool propto, bool jacobian>
  double log_prob_impl(Eigen::VectorXd& q, std::ostream*) const {
    (void)propto;
    (void)jacobian;
    auto lease = pool_->acquire();
    check_size(q.size());
    for (Eigen::Index i = 0; i < q.size(); ++i) lease->params_data()[i] = q(i);
    try {
      return lease->forward();
    } catch (const std::exception&) {
      return -std::numeric_limits<double>::infinity();
    }
  }

  template <bool propto, bool jacobian>
  stan::math::var log_prob_impl(Eigen::Matrix<stan::math::var, -1, 1>& q,
                                std::ostream*) const {
    (void)propto;
    (void)jacobian;
    auto lease = pool_->acquire();
    check_size(q.size());
    std::vector<double> gradient(static_cast<size_t>(q.size()));
    for (Eigen::Index i = 0; i < q.size(); ++i)
      lease->params_data()[i] = q(i).val();
    double value;
    try {
      value = lease->gradient(gradient.data());
    } catch (const std::exception&) {
      return stan::math::var(-std::numeric_limits<double>::infinity());
    }
    std::vector<stan::math::var> operands(q.data(), q.data() + q.size());
    return stan::math::precomputed_gradients(value, operands, gradient);
  }

#define STANLI_EIGEN_LOG_PROB(NAME, PRO, JAC) \
  double NAME(Eigen::VectorXd& q, std::ostream* msgs) const override { \
    return log_prob_impl<PRO, JAC>(q, msgs); \
  } \
  stan::math::var NAME( \
      Eigen::Matrix<stan::math::var, -1, 1>& q, std::ostream* msgs) const override { \
    return log_prob_impl<PRO, JAC>(q, msgs); \
  }

  STANLI_EIGEN_LOG_PROB(log_prob, false, false)
  STANLI_EIGEN_LOG_PROB(log_prob_jacobian, false, true)
  STANLI_EIGEN_LOG_PROB(log_prob_propto, true, false)
  STANLI_EIGEN_LOG_PROB(log_prob_propto_jacobian, true, true)

#undef STANLI_EIGEN_LOG_PROB

  template <bool propto, bool jacobian, typename T>
  T vector_log_prob(std::vector<T>& q, std::vector<int>&, std::ostream* msgs) const {
    Eigen::Matrix<T, -1, 1> mapped = Eigen::Map<Eigen::Matrix<T, -1, 1>>(
        q.data(), static_cast<Eigen::Index>(q.size()));
    return log_prob_impl<propto, jacobian>(mapped, msgs);
  }

#define STANLI_VECTOR_LOG_PROB(NAME, PRO, JAC) \
  double NAME(std::vector<double>& q, std::vector<int>& i, std::ostream* msgs) const override { \
    return vector_log_prob<PRO, JAC>(q, i, msgs); \
  } \
  stan::math::var NAME(std::vector<stan::math::var>& q, std::vector<int>& i, std::ostream* msgs) const override { \
    return vector_log_prob<PRO, JAC>(q, i, msgs); \
  }

  STANLI_VECTOR_LOG_PROB(log_prob, false, false)
  STANLI_VECTOR_LOG_PROB(log_prob_jacobian, false, true)
  STANLI_VECTOR_LOG_PROB(log_prob_propto, true, false)
  STANLI_VECTOR_LOG_PROB(log_prob_propto_jacobian, true, true)

#undef STANLI_VECTOR_LOG_PROB

  void transform_inits(const stan::io::var_context&, Eigen::VectorXd&,
                       std::ostream*) const override {
    throw std::runtime_error(
        "stanli cannot take inits on the constrained scale: pass an unconstrained init instead");
  }

  void transform_inits(const stan::io::var_context&, std::vector<int>&,
                       std::vector<double>&, std::ostream*) const override {
    throw std::runtime_error(
        "stanli cannot take inits on the constrained scale: pass an unconstrained init instead");
  }

  void unconstrain_array(const Eigen::VectorXd&, Eigen::VectorXd&,
                         std::ostream*) const override {
    throw std::runtime_error(
        "stanli does not yet implement inverse parameter transforms");
  }

  void unconstrain_array(const std::vector<double>&, std::vector<double>&,
                         std::ostream*) const override {
    throw std::runtime_error(
        "stanli does not yet implement inverse parameter transforms");
  }

  void write_array(stan::rng_t& rng, Eigen::VectorXd& q, Eigen::VectorXd& out,
                   bool include_tparams = true, bool include_gqs = true,
                   std::ostream* msgs = nullptr) const override {
    std::vector<double> input(q.data(), q.data() + q.size());
    std::vector<double> values;
    write_array_impl(rng, input, values, include_tparams, include_gqs, msgs);
    out = Eigen::Map<Eigen::VectorXd>(values.data(), static_cast<Eigen::Index>(values.size()));
  }

  void write_array(stan::rng_t& rng, std::vector<double>& q,
                   std::vector<int>&, std::vector<double>& out,
                   bool include_tparams = true, bool include_gqs = true,
                   std::ostream* msgs = nullptr) const override {
    write_array_impl(rng, q, out, include_tparams, include_gqs, msgs);
  }

 private:
  static std::vector<size_t> view_dims(const stanli::CompiledModel::ParamView& v) {
    return std::vector<size_t>(v.dims.begin(), v.dims.end());
  }

  static std::vector<size_t> column_dims(const stanli::CompiledModel::ParamView& v) {
    if (v.naming == stanli::CompiledModel::ParamView::Naming::Matrix && v.rows > 0)
      return {static_cast<size_t>(v.rows), static_cast<size_t>(v.len / v.rows)};
    if (v.len == 1 && v.naming == stanli::CompiledModel::ParamView::Naming::Scalar)
      return {};
    return {static_cast<size_t>(v.len)};
  }

  size_t selected_column_end(bool include_tparams, bool include_gqs) const {
    if (!has_write_array_) return cm_.views.size();
    if (include_gqs) return wa_columns_.size();
    if (include_tparams) return wa_n_gq_start_;
    return wa_n_tp_start_;
  }

  void append_columns(std::vector<std::string>& names, size_t first,
                      size_t last) const {
    for (size_t i = first; i < last; ++i) wa_columns_[i].append_names(names);
  }

  void check_size(Eigen::Index n) const {
    if (n != static_cast<Eigen::Index>(num_params_r__))
      throw std::invalid_argument("stanli received the wrong number of unconstrained parameters");
  }

  void prepare_write_array() {
    if (!cm_.write_array) return;
    auto& wa = *cm_.write_array;
    if (wa.interp) {
      wa_interp_ = wa.interp;
      std::vector<double> q(static_cast<size_t>(proto_->n_params()));
      bool found = false;
      for (int variant = 0; variant < 3 && !found; ++variant) {
        for (size_t i = 0; i < q.size(); ++i)
          q[i] = stanli::wa_probe_point(static_cast<int64_t>(i), variant);
        try {
          stanli::WaRng probe(1);
          std::copy(q.begin(), q.end(), proto_->params_data());
          proto_->run_forward_only();
          (void)wa_interp_->eval(cm_.constrained_env(*proto_), probe);
          found = true;
        } catch (const std::exception&) {
        }
      }
      if (!found) throw std::runtime_error("stanli could not evaluate write_array during model construction");
      wa_columns_ = wa_interp_->columns();
      wa_n_tp_start_ = wa_interp_->n_tp_start();
      wa_n_gq_start_ = wa_interp_->n_gq_start();
    } else if (!wa.columns.empty()) {
      wa_proto_ = std::make_unique<stanli::Executor>(std::move(wa.graph));
      wa.bind(*wa_proto_);
      wa_columns_ = wa.columns;
      wa_n_tp_start_ = wa.n_tp_start;
      wa_n_gq_start_ = wa.n_gq_start;
      wa_pool_ = std::make_unique<stanli::ExecutorPool>(*wa_proto_);
    } else {
      return;
    }
    has_write_array_ = true;
  }

  void write_array_impl(stan::rng_t& rng, const std::vector<double>& q,
                        std::vector<double>& out, bool include_tparams,
                        bool include_gqs, std::ostream*) const {
    if (q.size() != num_params_r__)
      throw std::invalid_argument("stanli received the wrong number of unconstrained parameters");
    const size_t first = 0;
    const size_t last = selected_column_end(include_tparams, include_gqs);
    out.assign(has_write_array_ ? scalar_count(wa_columns_, first, last)
                  : scalar_count(cm_.views, first, last),
           0.0);
    if (!has_write_array_) {
      auto lease = pool_->acquire();
      std::copy(q.begin(), q.end(), lease->params_data());
      lease->run_forward_only();
      size_t offset = 0;
      for (const auto& v : cm_.views) {
        const double* p = lease->value_ptr(v.slot);
        std::copy(p, p + v.len, out.begin() + offset);
        offset += static_cast<size_t>(v.len);
      }
      return;
    }
    if (wa_interp_) {
      // The interpreter owns a different RNG type. A fresh seed from the
      // Stan RNG keeps this path thread-safe; graph write_array remains the
      // exact fast path. RNG generated quantities are intentionally not
      // advertised as bit-identical by this first backend.
      stanli::WaRng wa_rng(static_cast<unsigned>(rng()));
      auto lease = pool_->acquire();
      std::copy(q.begin(), q.end(), lease->params_data());
      lease->run_forward_only();
      const std::vector<double> row = wa_interp_->eval(
          cm_.constrained_env(*lease), wa_rng);
      std::copy(row.begin(), row.begin() + static_cast<std::ptrdiff_t>(out.size()), out.begin());
      return;
    }
    auto lease = wa_pool_->acquire();
    std::copy(q.begin(), q.end(), lease->params_data());
    lease->run_forward_only();
    size_t offset = 0;
    for (size_t i = first; i < last; ++i) {
      const auto& v = wa_columns_[i];
      const double* p = lease->value_ptr(v.slot);
      std::copy(p, p + v.len, out.begin() + offset);
      offset += static_cast<size_t>(v.len);
    }
  }

  std::string name_;
  // Recorded, not consumed: stanli's compiler rejects any `_rng` call in
  // transformed data outright, so there is nothing to seed yet. Kept for
  // parity with stanli's own bs_model_from_mir(), which does the same.
  unsigned int seed_;
  stanli::CompiledModel cm_;
  std::unique_ptr<stanli::Executor> proto_;
  mutable std::unique_ptr<stanli::ExecutorPool> pool_;
  std::unique_ptr<stanli::Executor> wa_proto_;
  mutable std::unique_ptr<stanli::ExecutorPool> wa_pool_;
  std::shared_ptr<stanli::WaInterp> wa_interp_;
  std::vector<stanli::CompiledModel::ParamView> wa_columns_;
  size_t wa_n_tp_start_ = 0;
  size_t wa_n_gq_start_ = 0;
  bool has_write_array_ = false;
};

#undef STANLI_EIGEN_LOG_PROB
#undef STANLI_VECTOR_LOG_PROB

extern "C" SEXP stanr_stanli_new_model(SEXP mir, SEXP data, SEXP model_name,
                                        SEXP seed) {
  BEGIN_CPP11
  if (TYPEOF(mir) != STRSXP || XLENGTH(mir) != 1)
    cpp11::stop("stanli MIR must be a single string");
  if (TYPEOF(model_name) != STRSXP || XLENGTH(model_name) != 1)
    cpp11::stop("stanli model name must be a single string");
  stanli::DataMap data_map = sexp_to_data_map(data);
  auto* model = new stanli_model_base(
      CHAR(STRING_ELT(mir, 0)), data_map, CHAR(STRING_ELT(model_name, 0)),
      stanr::as_cpp<unsigned int>(seed));
  return cpp11::external_pointer<stan::model::model_base>(model);
  END_CPP11
}

extern "C" SEXP stanr_stanli_run_model(SEXP model, SEXP args) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::run_model(*m, cpp11::list(args));
  END_CPP11
}

extern "C" SEXP stanr_stanli_constrained_param_names(SEXP model) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_constrained_names(*m, false, false);
  END_CPP11
}

extern "C" SEXP stanr_stanli_new_base_rng(SEXP seed) {
  BEGIN_CPP11
  return stanr::make_base_rng(stanr::as_cpp<unsigned int>(seed));
  END_CPP11
}

#define STANLI_MODEL_METHOD(name, signature, expression) \
  extern "C" SEXP stanr_stanli_##name signature { \
    BEGIN_CPP11 \
    cpp11::external_pointer<stan::model::model_base> m(model); \
    expression; \
    END_CPP11 \
  }

STANLI_MODEL_METHOD(model_num_upars, (SEXP model),
                    return cpp11::as_sexp(stanr::model_num_upars(*m)))
STANLI_MODEL_METHOD(model_param_metadata, (SEXP model),
                    return stanr::model_param_metadata(*m))
STANLI_MODEL_METHOD(model_constrained_names,
                    (SEXP model, SEXP tp, SEXP gq),
                    return stanr::model_constrained_names(
                        *m, stanr::as_cpp<bool>(tp), stanr::as_cpp<bool>(gq)))
STANLI_MODEL_METHOD(model_unconstrained_names, (SEXP model),
                    return stanr::model_unconstrained_names(*m))
STANLI_MODEL_METHOD(model_log_prob,
                    (SEXP model, SEXP upars, SEXP jacobian),
                    return stanr::model_log_prob(
                        *m, cpp11::doubles(upars), stanr::as_cpp<bool>(jacobian)))
STANLI_MODEL_METHOD(model_grad_log_prob,
                    (SEXP model, SEXP upars, SEXP jacobian),
                    return stanr::model_grad_log_prob(
                        *m, cpp11::doubles(upars), stanr::as_cpp<bool>(jacobian)))
STANLI_MODEL_METHOD(model_hessian,
                    (SEXP model, SEXP upars, SEXP jacobian),
                    return stanr::model_hessian(
                        *m, cpp11::doubles(upars), stanr::as_cpp<bool>(jacobian)))
STANLI_MODEL_METHOD(model_unconstrain,
                    (SEXP model, SEXP variables, SEXP declarations),
                    return stanr::model_unconstrain(
                        *m, cpp11::list(variables), declarations))
STANLI_MODEL_METHOD(model_unconstrain_matrix,
                    (SEXP model, SEXP values),
                    return stanr::model_unconstrain_matrix(
                        *m, cpp11::doubles_matrix<>(values)))
STANLI_MODEL_METHOD(model_constrain,
                    (SEXP model, SEXP rng, SEXP upars, SEXP tp, SEXP gq),
                    return stanr::model_constrain(
                        *m, *cpp11::external_pointer<stan::rng_t>(rng),
                        cpp11::doubles(upars), stanr::as_cpp<bool>(tp),
                        stanr::as_cpp<bool>(gq)))
STANLI_MODEL_METHOD(model_constrain_matrix,
                    (SEXP model, SEXP rng, SEXP upars, SEXP tp, SEXP gq),
                    return stanr::model_constrain_matrix(
                        *m, *cpp11::external_pointer<stan::rng_t>(rng),
                        cpp11::doubles_matrix<>(upars), stanr::as_cpp<bool>(tp),
                        stanr::as_cpp<bool>(gq)))
STANLI_MODEL_METHOD(model_constrain_variables,
                    (SEXP model, SEXP rng, SEXP upars, SEXP tp, SEXP gq,
                     SEXP declarations),
                    return stanr::model_constrain_variables(
                        *m, *cpp11::external_pointer<stan::rng_t>(rng),
                        cpp11::doubles(upars), stanr::as_cpp<bool>(tp),
                        stanr::as_cpp<bool>(gq), declarations))
STANLI_MODEL_METHOD(model_variable_skeleton,
                    (SEXP model, SEXP tp, SEXP gq, SEXP declarations),
                    return stanr::model_variable_skeleton(
                        *m, stanr::as_cpp<bool>(tp), stanr::as_cpp<bool>(gq),
                        cpp11::list(declarations)))
extern "C" SEXP stanr_stanli_select_opencl_device(SEXP, SEXP) {
  BEGIN_CPP11
  return R_NilValue;
  END_CPP11
}

}  // namespace stanr
