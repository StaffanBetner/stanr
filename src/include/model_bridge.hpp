#ifndef NEWSTAN_MODEL_BRIDGE_HPP
#define NEWSTAN_MODEL_BRIDGE_HPP

#include <stan/math/rev/core/precomputed_gradients.hpp>
#include <stan/model/model_base_crtp.hpp>

#include <stdexcept>
#include <type_traits>
#include <vector>

namespace newstan {

// This is deliberately a small C++ ABI shared with the sourceCpp-generated
// model.  The callback keeps construction of model autodiff variables, model
// evaluation, reverse propagation, and memory recovery in the dylib that owns
// the concrete generated model.
struct model_bridge {
  using log_prob_fn = double (*)(const void*, const double*, double*, bool,
                                 bool, std::ostream*);

  const void* context;
  log_prob_fn log_prob;
};

class model_bridge_model
    : public stan::model::model_base_crtp<model_bridge_model> {
 private:
  using base_type = stan::model::model_base_crtp<model_bridge_model>;

  const stan::model::model_base& model_;
  const model_bridge& bridge_;

  double evaluate(const double* params_r, double* gradient, bool propto,
                  bool jacobian, std::ostream* msgs) const {
    return bridge_.log_prob(bridge_.context, params_r, gradient, propto,
                            jacobian, msgs);
  }

  template <typename T>
  struct unsupported_scalar : std::false_type {};

 public:
  model_bridge_model(const stan::model::model_base& model,
                     const model_bridge& bridge)
      : base_type(model.num_params_r()), model_(model), bridge_(bridge) {
    if (bridge_.context == nullptr || bridge_.log_prob == nullptr) {
      throw std::invalid_argument("newstan model bridge is invalid");
    }
  }

  using base_type::log_prob;

  std::string model_name() const override { return model_.model_name(); }

  std::vector<std::string> model_compile_info() const override {
    return model_.model_compile_info();
  }

  void get_param_names(std::vector<std::string>& names, bool include_tparams,
                       bool include_gqs) const override {
    model_.get_param_names(names, include_tparams, include_gqs);
  }

  void get_dims(std::vector<std::vector<size_t>>& dimss, bool include_tparams,
                bool include_gqs) const override {
    model_.get_dims(dimss, include_tparams, include_gqs);
  }

  void constrained_param_names(std::vector<std::string>& names,
                               bool include_tparams,
                               bool include_gqs) const override {
    model_.constrained_param_names(names, include_tparams, include_gqs);
  }

  void unconstrained_param_names(std::vector<std::string>& names,
                                 bool include_tparams,
                                 bool include_gqs) const override {
    model_.unconstrained_param_names(names, include_tparams, include_gqs);
  }

  template <bool propto, bool jacobian, typename T>
  T log_prob(Eigen::Matrix<T, -1, 1>& params_r,
             std::ostream* msgs = nullptr) const {
    if constexpr (std::is_same_v<T, double>) {
      return evaluate(params_r.data(), nullptr, propto, jacobian, msgs);
    } else if constexpr (std::is_same_v<T, stan::math::var>) {
      Eigen::VectorXd values(params_r.size());
      Eigen::VectorXd gradient(params_r.size());
      for (Eigen::Index i = 0; i < params_r.size(); ++i) {
        values(i) = params_r(i).val();
      }
      double lp = evaluate(values.data(), gradient.data(), propto, jacobian,
                           msgs);
      return stan::math::precomputed_gradients(lp, params_r, gradient);
    } else {
      static_assert(unsupported_scalar<T>::value,
                    "newstan model bridge supports double and stan::math::var");
    }
  }

  template <bool propto, bool jacobian, typename T>
  T log_prob(std::vector<T>& params_r, std::vector<int>&,
             std::ostream* msgs = nullptr) const {
    if constexpr (std::is_same_v<T, double>) {
      return evaluate(params_r.data(), nullptr, propto, jacobian, msgs);
    } else if constexpr (std::is_same_v<T, stan::math::var>) {
      std::vector<double> values(params_r.size());
      std::vector<double> gradient(params_r.size());
      for (size_t i = 0; i < params_r.size(); ++i) {
        values[i] = params_r[i].val();
      }
      double lp = evaluate(values.data(), gradient.data(), propto, jacobian,
                           msgs);
      return stan::math::precomputed_gradients(lp, params_r, gradient);
    } else {
      static_assert(unsupported_scalar<T>::value,
                    "newstan model bridge supports double and stan::math::var");
    }
  }

  void transform_inits(const stan::io::var_context& context,
                       Eigen::VectorXd& params_r,
                       std::ostream* msgs = nullptr) const {
    model_.transform_inits(context, params_r, msgs);
  }

  void transform_inits(const stan::io::var_context& context,
                       std::vector<int>& params_i,
                       std::vector<double>& params_r,
                       std::ostream* msgs = nullptr) const override {
    model_.transform_inits(context, params_i, params_r, msgs);
  }

  void write_array(stan::rng_t& rng, Eigen::VectorXd& params_r,
                   Eigen::VectorXd& vars, bool include_tparams = true,
                   bool include_gqs = true,
                   std::ostream* msgs = nullptr) const {
    model_.write_array(rng, params_r, vars, include_tparams, include_gqs,
                       msgs);
  }

  void write_array(stan::rng_t& rng, std::vector<double>& params_r,
                   std::vector<int>& params_i, std::vector<double>& vars,
                   bool include_tparams = true, bool include_gqs = true,
                   std::ostream* msgs = nullptr) const {
    model_.write_array(rng, params_r, params_i, vars, include_tparams,
                       include_gqs, msgs);
  }

  void unconstrain_array(const Eigen::VectorXd& params_r_constrained,
                         Eigen::VectorXd& params_r,
                         std::ostream* msgs = nullptr) const {
    model_.unconstrain_array(params_r_constrained, params_r, msgs);
  }

  void unconstrain_array(const std::vector<double>& params_r_constrained,
                         std::vector<double>& params_r,
                         std::ostream* msgs = nullptr) const {
    model_.unconstrain_array(params_r_constrained, params_r, msgs);
  }
};

}  // namespace newstan

#endif
