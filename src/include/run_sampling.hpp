#ifndef NEWSTAN_RUN_SAMPLING_HPP
#define NEWSTAN_RUN_SAMPLING_HPP

#include <Rcpp.h>
#include <memory>
#include <stan/io/var_context.hpp>
#include <stan/services/sample/fixed_param.hpp>
#include <stan/services/sample/hmc_nuts_dense_e.hpp>
#include <stan/services/sample/hmc_nuts_dense_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_diag_e.hpp>
#include <stan/services/sample/hmc_nuts_diag_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_unit_e.hpp>
#include <stan/services/sample/hmc_nuts_unit_e_adapt.hpp>
#include <stan/services/sample/hmc_static_unit_e.hpp>
#include <stan/services/sample/hmc_static_unit_e_adapt.hpp>
#include <stan/services/sample/hmc_static_diag_e.hpp>
#include <stan/services/sample/hmc_static_diag_e_adapt.hpp>
#include <stan/services/sample/hmc_static_dense_e.hpp>
#include <stan/services/sample/hmc_static_dense_e_adapt.hpp>
#include <stan/callbacks/json_writer.hpp>
#include <stan/math/rev/core/chainablestack.hpp>
#include <stan/math/rev/core/init_chainablestack.hpp>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <exception>
#include <mutex>
#include <sstream>
#include <thread>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include <newstan/r_data_context.hpp>
#include <newstan/r_metric_context.hpp>
#include "stack_writer_chains.hpp"

namespace newstan {

  template <class Model>
  std::vector<std::shared_ptr<stan::io::var_context>> make_metric_contexts(
      Model& model, const Rcpp::List& args, const std::string& metric,
      int num_chains) {
    const bool metric_supplied = args.containsElementNamed("inv_metric") &&
        !Rcpp::as<bool>(args["inv_metric_na"]);
    if (!metric_supplied || metric == "unit_e") return {};

    Rcpp::List inv_metric_list = Rcpp::as<Rcpp::List>(args["inv_metric"]);
    const size_t num_params = model.num_params_r();
    const bool per_chain =
        inv_metric_list.length() == static_cast<R_xlen_t>(num_chains);
    std::vector<std::shared_ptr<stan::io::var_context>> contexts;
    contexts.reserve(num_chains);

    for (int i = 0; i < num_chains; ++i) {
      SEXP metric_value = inv_metric_list[per_chain ? i : 0];
      std::vector<double> values;
      std::vector<size_t> dimensions;
      if (metric == "diag_e") {
        Rcpp::NumericVector vector = Rcpp::as<Rcpp::NumericVector>(metric_value);
        values.assign(vector.begin(), vector.end());
        dimensions = {num_params};
      } else {
        Rcpp::NumericMatrix matrix = Rcpp::as<Rcpp::NumericMatrix>(metric_value);
        values.assign(matrix.begin(), matrix.end());
        dimensions = {num_params, num_params};
      }
      contexts.emplace_back(
          std::make_shared<newstan::r_metric_context>(std::move(values),
                                                       std::move(dimensions)));
    }
    return contexts;
  }

  template <class Model>
  Rcpp::List run_sampling(Model& model, Rcpp::List args) {
    const std::string algorithm = Rcpp::as<std::string>(args["algorithm"]);
    const std::string engine = Rcpp::as<std::string>(args["engine"]);
    const std::string metric = Rcpp::as<std::string>(args["metric"]);
    const bool adapt_engaged = Rcpp::as<bool>(args["adapt_engaged"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);

    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const unsigned int chain_id = Rcpp::as<unsigned int>(args["id"]);
    const int num_chains = Rcpp::as<int>(args["num_chains"]);
    const double init_radius = Rcpp::as<double>(args["init_radius"]);
    const int num_warmup = Rcpp::as<int>(args["num_warmup"]);
    const int num_samples = Rcpp::as<int>(args["num_samples"]);
    const int num_thin = Rcpp::as<int>(args["thin"]);
    const bool save_warmup = Rcpp::as<bool>(args["save_warmup"]);
    const int refresh = Rcpp::as<int>(args["refresh"]);
    const double stepsize = Rcpp::as<double>(args["stepsize"]);
    const double stepsize_jitter = Rcpp::as<double>(args["stepsize_jitter"]);
    const int max_depth = Rcpp::as<int>(args["max_depth"]);
    const double int_time = Rcpp::as<double>(args["int_time"]);

    // Adaptation parameters
    const double delta = Rcpp::as<double>(args["delta"]);
    const double gamma = Rcpp::as<double>(args["gamma"]);
    const double kappa = Rcpp::as<double>(args["kappa"]);
    const double t0 = Rcpp::as<double>(args["t0"]);

    // Adaptation window parameters (for diag_e and dense_e metrics)
    const int init_buffer_arg = Rcpp::as<int>(args["init_buffer"]);
    const int term_buffer_arg = Rcpp::as<int>(args["term_buffer"]);
    const int window_arg = Rcpp::as<int>(args["window"]);

    newstan::r_logger logger(verbose);

    const auto saved_rows = [num_thin](int iterations) {
      return iterations / num_thin + (iterations % num_thin != 0);
    };
    const int expected_rows = saved_rows(num_samples)
        + (save_warmup ? saved_rows(num_warmup) : 0);
    const unsigned int init_buffer = static_cast<unsigned int>(init_buffer_arg);
    const unsigned int term_buffer = static_cast<unsigned int>(term_buffer_arg);
    const unsigned int window = static_cast<unsigned int>(window_arg);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    // ── Build per-chain contexts and writers ────────────────────────
    const auto init_ctx = std::make_shared<newstan::r_data_context>(init_list);
    std::vector<std::shared_ptr<newstan::r_data_context>> init_ctxs(
        num_chains, init_ctx);
    std::vector<newstan::r_sample_writer> init_writers;
    std::vector<newstan::r_sample_writer> sample_writers;
    std::vector<newstan::r_discard_writer> diag_writers;
    std::vector<stan::callbacks::json_writer<std::ostringstream>> metric_writers;

    init_writers.reserve(num_chains);
    sample_writers.reserve(num_chains);
    diag_writers.reserve(num_chains);
    metric_writers.reserve(num_chains);

    for (int i = 0; i < num_chains; ++i) {
      init_writers.emplace_back(1);
      sample_writers.emplace_back(expected_rows);
      diag_writers.emplace_back();
      metric_writers.emplace_back(std::make_unique<std::ostringstream>());
    }

    const bool metric_supplied = args.containsElementNamed("inv_metric") &&
                           !Rcpp::as<bool>(args["inv_metric_na"]);
    auto metric_ctxs = make_metric_contexts(model, args, metric, num_chains);
    const bool multi_chain = num_chains > 1;

    // Sampling runs in a native coordinator thread.  Ctrl-C is observed by
    // the R thread below and relayed to all Stan/TBB workers through this
    // native-only callback.
    std::atomic<bool> cancel_requested{false};
    newstan::r_interrupt interrupt(&cancel_requested);

    int return_code = stan::services::error_codes::CONFIG;
    std::atomic<bool> finished{false};
    std::mutex completion_mutex;
    std::condition_variable completion_cv;
    std::exception_ptr worker_error;

    // Everything in this lambda is restricted to C++/Stan state.  In
    // particular, it must not allocate R objects or call into Rcpp.
    std::thread worker([&] {
    // A raw std::thread is outside TBB's scheduler, and the sampling service
    // initializes chains before it enters parallel_for.  Its AD stack must be
    // explicit.  Attach an observer for the lifetime of this job as well: it
    // initializes a separate AD tape in every TBB worker that executes a
    // chain.
    stan::math::ChainableStack autodiff_stack;
    stan::math::ad_tape_observer autodiff_observer;
    try {
    // ── Validation & dispatch ───────────────────────────────────────
    if (algorithm == "hmc" && adapt_engaged && num_warmup == 0) {
      std::ostringstream msg;
      msg << "num_warmup must be > 0 when adapt_engaged is TRUE.";
      logger.error(msg.str());
      // return_code remains CONFIG

    // ── Dispatch: fixed_param ───────────────────────────────────────
    } else if (algorithm == "fixed_param") {
      if (multi_chain) {
        return_code = stan::services::sample::fixed_param(
            model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
            init_radius, num_samples, num_thin, refresh,
            interrupt, logger, init_writers, sample_writers, diag_writers);
      } else {
        return_code = stan::services::sample::fixed_param(
            model, *init_ctxs[0], seed, chain_id, init_radius,
            num_samples, num_thin, refresh,
            interrupt, logger, init_writers[0],
            sample_writers[0], diag_writers[0]);
      }

    // ── Dispatch: HMC + NUTS ────────────────────────────────────────
    } else if (algorithm == "hmc" && engine == "nuts") {

      if (adapt_engaged) {
        // ── NUTS with adaptation ────────────────────────────────────

        if (metric == "unit_e") {
          return_code = stan::services::sample::hmc_nuts_unit_e_adapt(
              model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
              init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              interrupt, logger,
              init_writers, sample_writers, diag_writers,
              metric_writers);

        } else if (metric == "diag_e") {
          if (metric_supplied) {
            return_code = stan::services::sample::hmc_nuts_diag_e_adapt(
                model, static_cast<size_t>(num_chains), init_ctxs, metric_ctxs,
                seed, chain_id, init_radius, num_warmup, num_samples, num_thin,
                save_warmup, refresh, stepsize, stepsize_jitter, max_depth,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger,
                init_writers, sample_writers, diag_writers,
                metric_writers);
          } else {
            return_code = stan::services::sample::hmc_nuts_diag_e_adapt(
                model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger,
                init_writers, sample_writers, diag_writers,
                metric_writers);
          }

        } else if (metric == "dense_e") {
          if (metric_supplied) {
            return_code = stan::services::sample::hmc_nuts_dense_e_adapt(
                model, static_cast<size_t>(num_chains), init_ctxs, metric_ctxs,
                seed, chain_id, init_radius, num_warmup, num_samples, num_thin,
                save_warmup, refresh, stepsize, stepsize_jitter, max_depth,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger,
                init_writers, sample_writers, diag_writers,
                metric_writers);
          } else {
            return_code = stan::services::sample::hmc_nuts_dense_e_adapt(
                model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger,
                init_writers, sample_writers, diag_writers,
                metric_writers);
          }
        }

      } else {
        // ── NUTS without adaptation (fixed stepsize) ────────────────

        if (metric == "unit_e") {
          if (multi_chain) {
            return_code = stan::services::sample::hmc_nuts_unit_e(
                model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                interrupt, logger, init_writers, sample_writers, diag_writers);
          } else {
            return_code = stan::services::sample::hmc_nuts_unit_e(
                model, *init_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          }
        } else if (metric_supplied && multi_chain) {
          std::ostringstream msg;
          msg << "inv_metric with non-adaptive " << metric
              << " is only supported for a single chain. "
              << "Set adapt_engaged = TRUE for multi-chain with custom metric.";
          logger.error(msg.str());
          return_code = stan::services::error_codes::CONFIG;
        } else if (metric == "diag_e") {
          if (metric_supplied) {
              return_code = stan::services::sample::hmc_nuts_diag_e(
                  model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                  num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers[0],
                  sample_writers[0], diag_writers[0]);
          } else {
            if (multi_chain) {
              return_code = stan::services::sample::hmc_nuts_diag_e(
                  model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                  init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers, sample_writers, diag_writers);
            } else {
              return_code = stan::services::sample::hmc_nuts_diag_e(
                  model, *init_ctxs[0], seed, chain_id, init_radius,
                  num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers[0],
                  sample_writers[0], diag_writers[0]);
            }
          }
        } else if (metric == "dense_e") {
          if (metric_supplied) {
              return_code = stan::services::sample::hmc_nuts_dense_e(
                  model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                  num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers[0],
                  sample_writers[0], diag_writers[0]);
          } else {
            if (multi_chain) {
              return_code = stan::services::sample::hmc_nuts_dense_e(
                  model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                  init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers, sample_writers, diag_writers);
            } else {
              return_code = stan::services::sample::hmc_nuts_dense_e(
                  model, *init_ctxs[0], seed, chain_id, init_radius,
                  num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers[0],
                  sample_writers[0], diag_writers[0]);
            }
          }
        }
      }

    // ── Dispatch: HMC + static ───────────────────────────────────────
    } else if (algorithm == "hmc" && engine == "static") {
      // Static HMC is single-chain only (no multi-chain overloads in Stan)
      if (multi_chain) {
        std::ostringstream msg;
        msg << "Static HMC only supports a single chain. Set chains = 1.";
        logger.error(msg.str());
        return_code = stan::services::error_codes::CONFIG;
      } else if (adapt_engaged && num_warmup == 0) {
        std::ostringstream msg;
        msg << "num_warmup must be > 0 when adapt_engaged is TRUE.";
        logger.error(msg.str());
        return_code = stan::services::error_codes::CONFIG;

      } else if (adapt_engaged) {
        // ── Static HMC with adaptation ──────────────────────────────

        if (metric == "unit_e") {
          return_code = stan::services::sample::hmc_static_unit_e_adapt(
              model, *init_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              delta, gamma, kappa, t0,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);

        } else if (metric == "diag_e") {
          if (metric_supplied) {
            return_code = stan::services::sample::hmc_static_diag_e_adapt(
                model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          } else {
            return_code = stan::services::sample::hmc_static_diag_e_adapt(
                model, *init_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          }

        } else if (metric == "dense_e") {
          if (metric_supplied) {
            return_code = stan::services::sample::hmc_static_dense_e_adapt(
                model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          } else {
            return_code = stan::services::sample::hmc_static_dense_e_adapt(
                model, *init_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                delta, gamma, kappa, t0,
                init_buffer, term_buffer, window,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          }
        }

      } else {
        // ── Static HMC without adaptation (fixed stepsize) ──────────

        if (metric == "unit_e") {
          return_code = stan::services::sample::hmc_static_unit_e(
              model, *init_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);

        } else if (metric == "diag_e") {
          if (metric_supplied) {
            return_code = stan::services::sample::hmc_static_diag_e(
                model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          } else {
            return_code = stan::services::sample::hmc_static_diag_e(
                model, *init_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          }

        } else if (metric == "dense_e") {
          if (metric_supplied) {
            return_code = stan::services::sample::hmc_static_dense_e(
                model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          } else {
            return_code = stan::services::sample::hmc_static_dense_e(
                model, *init_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, int_time,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          }
        }
      }

    // ── Unknown algorithm ───────────────────────────────────────────
    } else {
      std::ostringstream msg;
      msg << "Unknown sampling algorithm: " << algorithm;
      logger.error(msg.str());
      return_code = stan::services::error_codes::CONFIG;
    }

    } catch (...) {
      worker_error = std::current_exception();
    }
    finished.store(true, std::memory_order_release);
    completion_cv.notify_one();
    });

    // Keep the original R thread responsive for console output and Ctrl-C.
    // Rcpp protects this interrupt probe from R's longjmp; on Ctrl-C we first
    // cancel and join the worker before propagating the interrupt to R.
    bool interrupted = false;
    while (!finished.load(std::memory_order_acquire)) {
      logger.flush();
      if (!interrupted && user_interrupt_pending()) {
        interrupted = true;
        cancel_requested.store(true, std::memory_order_release);
      }

      std::unique_lock<std::mutex> lock(completion_mutex);
      completion_cv.wait_for(lock, std::chrono::milliseconds(50), [&] {
        return finished.load(std::memory_order_acquire);
      });
    }
    worker.join();
    logger.flush();

    if (worker_error) {
      try {
        std::rethrow_exception(worker_error);
      } catch (const std::exception& e) {
        Rcpp::stop(e.what());
      } catch (...) {
        Rcpp::stop("Unknown exception in sampling worker.");
      }
    }
    if (interrupted) {
      Rcpp::stop("Sampling interrupted.");
    }

    // ─── Combine results (R thread only) ────────────────────────────

    Rcpp::List combined = stack_writer_chains(sample_writers, num_chains);

    return Rcpp::List::create(
      Rcpp::_["samples"] = combined,
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "sample",
      Rcpp::_["algorithm"] = algorithm,
      Rcpp::_["engine"] = engine,
      Rcpp::_["metric"] = metric
    );
  }
}

#endif
