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
#include <stan/services/util/create_unit_e_diag_inv_metric.hpp>
#include <stan/services/util/create_unit_e_dense_inv_metric.hpp>
#include <sstream>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include "r_metric_writer.hpp"
#include "r_worker.hpp"
#include <newstan/r_data_context.hpp>
#include <newstan/r_metric_context.hpp>
#include "stack_writer_chains.hpp"

namespace newstan {

  template <class Model>
  std::vector<std::shared_ptr<stan::io::var_context>> make_metric_contexts(
      Model& model, const Rcpp::List& args, const std::string& metric,
      int num_chains, bool metric_supplied) {
    if (metric == "unit_e") return {};
    const size_t num_params = model.num_params_r();

    if (!metric_supplied) {
      // Build the same default unit inverse metric that the no-metric
      // overloads of hmc_nuts_{diag,dense}_e_adapt would otherwise
      // construct internally, so dispatch can always call the
      // metric-taking overload.
      std::shared_ptr<stan::io::var_context> default_ctx =
          metric == "diag_e"
              ? std::make_shared<stan::io::array_var_context>(
                    stan::services::util::create_unit_e_diag_inv_metric(
                        num_params))
              : std::make_shared<stan::io::array_var_context>(
                    stan::services::util::create_unit_e_dense_inv_metric(
                        num_params));
      return std::vector<std::shared_ptr<stan::io::var_context>>(num_chains,
                                                                   default_ctx);
    }

    Rcpp::List inv_metric_list = Rcpp::as<Rcpp::List>(args["inv_metric"]);
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
        dimensions = {static_cast<size_t>(vector.size())};
      } else {
        Rcpp::NumericMatrix matrix = Rcpp::as<Rcpp::NumericMatrix>(metric_value);
        values.assign(matrix.begin(), matrix.end());
        dimensions = {static_cast<size_t>(matrix.nrow()),
                      static_cast<size_t>(matrix.ncol())};
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
    const bool show_exceptions = Rcpp::as<bool>(args["show_exceptions"]);

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

    newstan::r_logger logger(verbose, show_exceptions);

    const auto saved_rows = [num_thin](int iterations) {
      return iterations / num_thin + (iterations % num_thin != 0);
    };
    const int expected_rows = saved_rows(num_samples)
        + (save_warmup ? saved_rows(num_warmup) : 0);
    const unsigned int init_buffer = static_cast<unsigned int>(init_buffer_arg);
    const unsigned int term_buffer = static_cast<unsigned int>(term_buffer_arg);
    const unsigned int window = static_cast<unsigned int>(window_arg);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    // --- Build per-chain contexts and writers ---
    const auto init_ctx = std::make_shared<newstan::r_data_context>(init_list);
    std::vector<std::shared_ptr<newstan::r_data_context>> init_ctxs(
        num_chains, init_ctx);
    std::vector<newstan::r_discard_writer> init_writers;
    std::vector<newstan::r_sample_writer> sample_writers;
    std::vector<newstan::r_discard_writer> diag_writers;
    std::vector<newstan::r_metric_writer> metric_writers;

    init_writers.reserve(num_chains);
    sample_writers.reserve(num_chains);
    diag_writers.reserve(num_chains);
    metric_writers.reserve(num_chains);

    for (int i = 0; i < num_chains; ++i) {
      init_writers.emplace_back();
      sample_writers.emplace_back(expected_rows);
      diag_writers.emplace_back();
      metric_writers.emplace_back();
    }

    const bool metric_supplied = args.containsElementNamed("inv_metric");
    auto metric_ctxs = make_metric_contexts(model, args, metric, num_chains,
                                             metric_supplied);
    const bool multi_chain = num_chains > 1;

    int return_code = newstan::run_on_worker_thread(
        logger, "Sampling",
        [&](newstan::r_interrupt& interrupt) -> int {
    int return_code = stan::services::error_codes::CONFIG;
    // --- Validation & dispatch ---
    if (algorithm == "hmc" && adapt_engaged && num_warmup == 0) {
      std::ostringstream msg;
      msg << "num_warmup must be > 0 when adapt_engaged is TRUE.";
      logger.error(msg.str());
      // return_code remains CONFIG

    // --- Dispatch: fixed_param ---
    } else if (algorithm == "fixed_param") {
      // The multi-chain overload accepts num_chains == 1 fine, so it is
      // always used regardless of chain count.
      return_code = stan::services::sample::fixed_param(
          model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
          init_radius, num_samples, num_thin, refresh,
          interrupt, logger, init_writers, sample_writers, diag_writers);

    // --- Dispatch: HMC + NUTS ---
    } else if (algorithm == "hmc" && engine == "nuts") {

      if (adapt_engaged) {
        // --- NUTS with adaptation ---
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
          // make_metric_contexts() fills metric_ctxs with a default unit
          // metric when none was supplied, so the metric-taking overload
          // is always used.
          return_code = stan::services::sample::hmc_nuts_diag_e_adapt(
              model, static_cast<size_t>(num_chains), init_ctxs, metric_ctxs,
              seed, chain_id, init_radius, num_warmup, num_samples, num_thin,
              save_warmup, refresh, stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger,
              init_writers, sample_writers, diag_writers,
              metric_writers);

        } else if (metric == "dense_e") {
          return_code = stan::services::sample::hmc_nuts_dense_e_adapt(
              model, static_cast<size_t>(num_chains), init_ctxs, metric_ctxs,
              seed, chain_id, init_radius, num_warmup, num_samples, num_thin,
              save_warmup, refresh, stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger,
              init_writers, sample_writers, diag_writers,
              metric_writers);
        }

      } else {
        // --- NUTS without adaptation (fixed stepsize) ---
        if (metric == "unit_e") {
          return_code = stan::services::sample::hmc_nuts_unit_e(
              model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
              init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              interrupt, logger, init_writers, sample_writers, diag_writers);
        } else if (metric_supplied && multi_chain) {
          std::ostringstream msg;
          msg << "inv_metric with non-adaptive " << metric
              << " is only supported for a single chain. "
              << "Set adapt_engaged = TRUE for multi-chain with custom metric.";
          logger.error(msg.str());
          return_code = stan::services::error_codes::CONFIG;
        } else if (metric == "diag_e") {
          if (metric_supplied) {
            // Guaranteed single-chain by the metric_supplied && multi_chain
            // guard above; there is no metric-taking multi-chain overload
            // for non-adaptive NUTS.
            return_code = stan::services::sample::hmc_nuts_diag_e(
                model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                interrupt, logger, init_writers[0],
                sample_writers[0], diag_writers[0]);
          } else {
            return_code = stan::services::sample::hmc_nuts_diag_e(
                model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                interrupt, logger, init_writers, sample_writers, diag_writers);
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
            return_code = stan::services::sample::hmc_nuts_dense_e(
                model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                interrupt, logger, init_writers, sample_writers, diag_writers);
          }
        }
      }

    // --- Dispatch: HMC + static ---
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
        // --- Static HMC with adaptation ---
        if (metric == "unit_e") {
          return_code = stan::services::sample::hmc_static_unit_e_adapt(
              model, *init_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              delta, gamma, kappa, t0,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);

        } else if (metric == "diag_e") {
          return_code = stan::services::sample::hmc_static_diag_e_adapt(
              model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);

        } else if (metric == "dense_e") {
          return_code = stan::services::sample::hmc_static_dense_e_adapt(
              model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);
        }

      } else {
        // --- Static HMC without adaptation (fixed stepsize) ---
        if (metric == "unit_e") {
          return_code = stan::services::sample::hmc_static_unit_e(
              model, *init_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);

        } else if (metric == "diag_e") {
          return_code = stan::services::sample::hmc_static_diag_e(
              model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);

        } else if (metric == "dense_e") {
          return_code = stan::services::sample::hmc_static_dense_e(
              model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, int_time,
              interrupt, logger, init_writers[0],
              sample_writers[0], diag_writers[0]);
        }
      }

    // --- Unknown algorithm ---
    } else {
      std::ostringstream msg;
      msg << "Unknown sampling algorithm: " << algorithm;
      logger.error(msg.str());
      return_code = stan::services::error_codes::CONFIG;
    }
    return return_code;
        });

    // --- Combine results (R thread only) ---
    Rcpp::NumericVector combined = return_code == 0
        ? writer_chains_to_array(sample_writers)
        : Rcpp::NumericVector(0);
    Rcpp::CharacterVector output(logger.history().begin(), logger.history().end());

    bool metric_captured = false;
    for (int i = 0; i < num_chains; ++i) {
      if (metric_writers[i].has_metric()) {
        metric_captured = true;
        break;
      }
    }

    Rcpp::List result = Rcpp::List::create(
      Rcpp::_["samples"] = combined,
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["inv_metric"] = R_NilValue,
      Rcpp::_["step_size"] = R_NilValue,
      Rcpp::_["output"] = output
    );

    if (metric_captured) {
      Rcpp::List inv_metric(num_chains);
      Rcpp::NumericVector step_size(num_chains);
      for (int i = 0; i < num_chains; ++i) {
        const newstan::r_metric_writer& w = metric_writers[i];
        step_size[i] = w.stepsize();
        if (w.is_dense()) {
          inv_metric[i] = w.inv_metric_matrix();
        } else {
          inv_metric[i] = w.inv_metric_vector();
        }
      }
      result["inv_metric"] = inv_metric;
      result["step_size"] = step_size;
    }

    return result;
  }
}

#endif
