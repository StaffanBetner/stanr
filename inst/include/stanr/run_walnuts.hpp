#ifndef STANR_RUN_WALNUTS_HPP
#define STANR_RUN_WALNUTS_HPP

#include <cpp11.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
#include <walnutpie/adaptive_walnuts.hpp>
#include <walnutpie/config.hpp>
#include <stan/model/model_base.hpp>
#include <stan/model/log_prob_grad.hpp>
#include <stan/services/util/create_rng.hpp>
#include <stan/services/util/initialize.hpp>
#include <tbb/blocked_range.h>
#include <tbb/parallel_for.h>
#include <limits>
#include <random>
#include <string>
#include <vector>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include "r_worker.hpp"
#include <stanr/r_data_context.hpp>
#include "stack_writer_chains.hpp"

namespace stanr {

// Adapts a Stan model to walnutpie's LogpGrad concept.
struct walnuts_logp_grad {
  const stan::model::model_base& model;

  void operator()(const Eigen::VectorXd& x, double& logp,
                  Eigen::VectorXd& grad) const {
    // log_prob_grad takes a mutable ref but never writes through it.
    logp = stan::model::log_prob_grad<true, true>(
        model, const_cast<Eigen::VectorXd&>(x), grad);
  }
};

// A walnutpie ChainHandler writing constrained draws into an r_sample_writer
// with columns [lp__, <constrained params>], the layout
// writer_chains_to_arrays() expects. Also records adapted step size/inv mass.
class r_walnuts_handler {
 public:
  r_walnuts_handler(const stan::model::model_base& model, stan::rng_t& rng,
                    r_sample_writer& writer, r_logger& logger,
                    std::size_t constrained_dim, bool save_warmup, int thin)
      : model_(model),
        rng_(rng),
        writer_(writer),
        logger_(logger),
        save_warmup_(save_warmup),
        thin_(thin),
        row_(1 + constrained_dim) {}

  // 0-indexed and pre-check: keeps the first iteration of each phase, then
  // every thin_-th after, matching saved_rows()'s ceiling division in R.
  void on_sample(const Eigen::VectorXd& position, double lp) {
    if (sample_iter_++ % thin_ == 0) write(position, lp);
  }

  void on_warmup(const Eigen::VectorXd& position, double lp, double,
                const Eigen::VectorXd&) {
    if (save_warmup_ && warmup_iter_++ % thin_ == 0) write(position, lp);
  }

  void on_warmup_complete(double step_size, const Eigen::VectorXd& inv_mass) {
    step_size_ = step_size;
    inv_mass_ = inv_mass;
  }

  // Same "Informational Message:"/"" block shape the Stan services emit, so
  // r_logger classifies it as exception chatter gated by show_exceptions.
  void on_logp_exception(const Eigen::VectorXd&,
                         const std::exception& e) const noexcept {
    try {
      logger_.error(std::string("Informational Message: ") + e.what());
      logger_.error("");
    } catch (...) {
    }
  }

  double step_size() const { return step_size_; }
  const Eigen::VectorXd& inv_mass() const { return inv_mass_; }

 private:
  void write(const Eigen::VectorXd& position, double lp) {
    try {
      // write_array takes mutable refs but leaves the position unchanged.
      model_.write_array(rng_, const_cast<Eigen::VectorXd&>(position),
                         constrained_, true, true);
      row_.tail(row_.size() - 1) = constrained_;
    } catch (const std::exception& e) {
      on_logp_exception(position, e);
      row_.tail(row_.size() - 1)
          .setConstant(std::numeric_limits<double>::quiet_NaN());
    }
    row_[0] = lp;
    writer_(row_);
  }

  const stan::model::model_base& model_;
  stan::rng_t& rng_;
  r_sample_writer& writer_;
  r_logger& logger_;
  const bool save_warmup_;
  const int thin_;
  int warmup_iter_ = 0;
  int sample_iter_ = 0;
  double step_size_ = 0;
  Eigen::VectorXd inv_mass_;
  Eigen::VectorXd row_;
  Eigen::VectorXd constrained_;
};

inline cpp11::writable::list run_walnuts(stan::model::model_base& model, cpp11::list args) {
  const bool verbose = stanr::as_cpp<bool>(args["verbose"]);
  const bool show_exceptions = stanr::as_cpp<bool>(args["show_exceptions"]);

  const unsigned int seed = stanr::as_cpp<unsigned int>(args["seed"]);
  const unsigned int chain_id = stanr::as_cpp<unsigned int>(args["id"]);
  const int num_chains = stanr::as_cpp<int>(args["num_chains"]);
  const double init_radius = stanr::as_cpp<double>(args["init_radius"]);
  const int num_warmup = stanr::as_cpp<int>(args["num_warmup"]);
  const int num_samples = stanr::as_cpp<int>(args["num_samples"]);
  const int num_thin = stanr::as_cpp<int>(args["thin"]);
  const bool save_warmup = stanr::as_cpp<bool>(args["save_warmup"]);
  const double step_size_init = stanr::as_cpp<double>(args["stepsize"]);

  const auto size_arg = [&](const char* name) {
    return static_cast<std::size_t>(stanr::as_cpp<int>(args[name]));
  };
  const std::size_t max_trajectory_doublings = size_arg("max_depth");
  const std::size_t max_step_halvings = size_arg("max_step_halvings");
  const std::size_t min_micro_steps = size_arg("min_micro_steps");

  const double step_accept_rate_target = stanr::as_cpp<double>(args["delta"]);
  const double max_hamiltonian_error =
      stanr::as_cpp<double>(args["max_hamiltonian_error"]);
  const double mass_init_count = stanr::as_cpp<double>(args["mass_init_count"]);
  const double mass_additive_smoothing =
      stanr::as_cpp<double>(args["mass_additive_smoothing"]);
  const double max_macro_steps_target =
      stanr::as_cpp<double>(args["max_macro_steps_target"]);
  const double step_learning_rate = stanr::as_cpp<double>(args["step_learning_rate"]);
  const double step_gradient_decay =
      stanr::as_cpp<double>(args["step_gradient_decay"]);
  const double step_sq_gradient_decay =
      stanr::as_cpp<double>(args["step_sq_gradient_decay"]);
  const double step_stabilization = stanr::as_cpp<double>(args["step_stabilization"]);
  const double step_learn_rate_decay =
      stanr::as_cpp<double>(args["step_learn_rate_decay"]);

  const std::vector<std::string> diagnostic_names =
      stanr::as_cpp<std::vector<std::string>>(args["diagnostic_names"]);

  stanr::r_logger logger(verbose, show_exceptions);

  const auto saved_rows = [num_thin](int iterations) {
    return iterations / num_thin + (iterations % num_thin != 0);
  };
  const int warmup_rows = save_warmup ? saved_rows(num_warmup) : 0;
  const int expected_rows = saved_rows(num_samples) + warmup_rows;

  std::vector<std::string> col_names{"lp__"};
  model.constrained_param_names(col_names, true, true);
  const std::size_t constrained_dim = col_names.size() - 1;

  std::vector<stanr::r_sample_writer> sample_writers;
  sample_writers.reserve(num_chains);
  for (int i = 0; i < num_chains; ++i) {
    sample_writers.emplace_back(expected_rows);
    sample_writers.back()(col_names);
  }

  // Per-chain unconstrained init and RNG for the model's own constraining
  // transform -- separate from walnutpie's own mt19937_64 stream.
  cpp11::list init_list = args["init"];
  stanr::r_data_context init_ctx(init_list, args["init_declarations"]);
  std::vector<stan::rng_t> model_rngs;
  std::vector<Eigen::VectorXd> positions;
  model_rngs.reserve(num_chains);
  positions.reserve(num_chains);
  for (int i = 0; i < num_chains; ++i) {
    model_rngs.push_back(
        stan::services::util::create_rng(seed, chain_id + i));
    stan::callbacks::writer init_writer;
    std::vector<double> unconstrained = stan::services::util::initialize<true>(
        model, init_ctx, model_rngs.back(), init_radius, false, logger,
        init_writer);
    positions.emplace_back(
        Eigen::Map<Eigen::VectorXd>(unconstrained.data(), unconstrained.size()));
  }

  walnuts_logp_grad logp{model};
  walnutpie::InitConfigBuilder init_builder{static_cast<std::size_t>(num_chains),
                                            model.num_params_r()};
  init_builder.step_sizes(step_size_init).positions(positions);
  if (!Rf_isNull(args["inv_metric"])) {
    cpp11::list inv_metric_list = args["inv_metric"];
    const bool per_chain =
        inv_metric_list.size() == static_cast<R_xlen_t>(num_chains);
    std::vector<Eigen::VectorXd> masses(num_chains);
    for (int i = 0; i < num_chains; ++i) {
      cpp11::doubles inv_metric(inv_metric_list[per_chain ? i : 0]);
      // walnutpie's masses() wants the mass matrix; inv_metric is its
      // inverse (Stan's usual "inverse metric" convention).
      masses[i] = Eigen::Map<Eigen::VectorXd>(REAL(inv_metric.data()),
                                              inv_metric.size())
                      .array()
                      .inverse()
                      .matrix();
    }
    init_builder.masses(masses);
  } else {
    init_builder.masses(logp, mass_additive_smoothing);
  }
  walnutpie::InitConfig init_cfg = init_builder.build();

  walnutpie::WarmupConfig warmup_cfg =
      walnutpie::WarmupConfigBuilder()
          .mass_init_count(mass_init_count)
          .mass_additive_smoothing(mass_additive_smoothing)
          .max_macro_steps_target(max_macro_steps_target)
          .step_accept_rate_target(step_accept_rate_target)
          .step_learning_rate(step_learning_rate)
          .step_gradient_decay(step_gradient_decay)
          .step_sq_gradient_decay(step_sq_gradient_decay)
          .step_stabilization(step_stabilization)
          .step_learn_rate_decay(step_learn_rate_decay)
          .build();

  walnutpie::SamplingConfig sampling_cfg =
      walnutpie::SamplingConfigBuilder()
          .max_trajectory_doublings(max_trajectory_doublings)
          .max_step_halvings(max_step_halvings)
          .max_hamiltonian_error(max_hamiltonian_error)
          .min_micro_steps(min_micro_steps)
          .build();

  std::vector<stanr::r_walnuts_handler> handlers;
  handlers.reserve(num_chains);
  for (int i = 0; i < num_chains; ++i) {
    handlers.emplace_back(model, model_rngs[i], sample_writers[i], logger,
                          constrained_dim, save_warmup, num_thin);
  }

  stanr::run_on_worker_thread(
      logger, "Sampling", [&](stanr::r_interrupt& interrupt) -> int {
        tbb::parallel_for(
            tbb::blocked_range<int>(0, num_chains),
            [&](const tbb::blocked_range<int>& range) {
              for (int i = range.begin(); i != range.end(); ++i) {
                std::seed_seq chain_seed{seed,
                                         chain_id + static_cast<unsigned int>(i)};
                std::mt19937_64 walnuts_rng(chain_seed);
                walnutpie::AdaptiveWalnuts walnuts(
                    walnuts_rng, handlers[i], logp, init_cfg.init_chain_config(i),
                    warmup_cfg, sampling_cfg);
                for (int w = 0; w < num_warmup; ++w) {
                  interrupt();
                  walnuts();
                }
                auto sampler = walnuts.sampler();
                for (int s = 0; s < num_samples; ++s) {
                  interrupt();
                  sampler();
                }
              }
            });
        return 0;
      });

  // Interrupts and worker exceptions surface as cpp11::stop inside
  // run_on_worker_thread, so reaching here means success.
  cpp11::writable::list chain_arrays = stanr::writer_chains_to_arrays(
      sample_writers, diagnostic_names, warmup_rows);
  cpp11::writable::strings output = cpp11::as_sexp(logger.history());

  cpp11::writable::list inv_metric(num_chains);
  cpp11::writable::doubles step_size(num_chains);
  for (int i = 0; i < num_chains; ++i) {
    step_size[i] = handlers[i].step_size();
    inv_metric[i] = stanr::as_sexp(handlers[i].inv_mass());
  }

  return cpp11::writable::list({
      cpp11::named_arg("samples") = chain_arrays["samples"],
      cpp11::named_arg("diagnostics") = chain_arrays["diagnostics"],
      cpp11::named_arg("warmup_samples") = chain_arrays["warmup_samples"],
      cpp11::named_arg("warmup_diagnostics") = chain_arrays["warmup_diagnostics"],
      cpp11::named_arg("return_code") = 0,
      cpp11::named_arg("inv_metric") = inv_metric,
      cpp11::named_arg("step_size") = step_size,
      cpp11::named_arg("output") = output});
}

}  // namespace stanr

#endif  // STANR_RUN_WALNUTS_HPP
