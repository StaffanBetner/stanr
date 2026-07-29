#ifndef NEWSTAN_RUN_SAMPLING_HPP
#define NEWSTAN_RUN_SAMPLING_HPP

#include <Rcpp.h>
#include <memory>
#include <stan/io/array_var_context.hpp>
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
#include <sstream>
#include "get_arg.hpp"
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include "r_data_context.hpp"
#include "stack_writer_chains.hpp"

namespace newstan {

  template <class Model>
  Rcpp::List run_sampling(Model& model, Rcpp::List args) {
    // ── Algorithm & engine (CmdStan-style) ─────────────────────────
    // algorithm: "hmc" or "fixed_param"
    // engine: "nuts" or "static" (only for hmc)
    std::string algorithm = get_string(args, "algorithm", "hmc");
    std::string engine    = get_string(args, "engine", "nuts");
    std::string metric    = get_string(args, "metric", "diag_e");
    bool adapt_engaged    = get_bool(args, "adapt_engaged", true);

    unsigned int seed           = Rcpp::as<unsigned int>(args["seed"]);
    unsigned int chain_id       = Rcpp::as<unsigned int>(args["chain_id"]);
    int num_chains              = Rcpp::as<int>(args["chains"]);
    double init_radius          = Rcpp::as<double>(args["init_radius"]);
    int num_warmup              = Rcpp::as<int>(args["num_warmup"]);
    int num_samples             = Rcpp::as<int>(args["num_samples"]);
    int num_thin                = get_int(args, "thin", 1);
    bool save_warmup            = Rcpp::as<bool>(args["save_warmup"]);
    int refresh                 = get_int(args, "refresh", 100);
    double stepsize             = get_double(args, "stepsize", 1.0);
    double stepsize_jitter      = get_double(args, "stepsize_jitter", 0.0);
    int max_depth               = get_int(args, "max_depth", 10);
    double int_time             = get_double(args, "int_time", 10.0);

    // Adaptation parameters
    double delta      = get_double(args, "delta", 0.8);
    double gamma      = get_double(args, "gamma", 0.05);
    double kappa      = get_double(args, "kappa", 0.75);
    double t0         = get_double(args, "t0", 10.0);

    // Adaptation window parameters (for diag_e and dense_e metrics)
    unsigned int init_buffer = get_uint(args, "init_buffer", 75);
    unsigned int term_buffer = get_uint(args, "term_buffer", 50);
    unsigned int window      = get_uint(args, "window", 25);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    // Compute expected rows per chain for preallocation
    int expected_rows = ((save_warmup ? num_warmup : 0) + num_samples) / num_thin;

    // ── Build per-chain contexts and writers ────────────────────────
    std::vector<std::shared_ptr<newstan::r_data_context>> init_ctxs(num_chains);
    std::vector<newstan::r_sample_writer> init_writers;
    std::vector<newstan::r_sample_writer> sample_writers;
    std::vector<newstan::r_diagnostic_writer> diag_writers;
    std::vector<stan::callbacks::json_writer<std::ostringstream>> metric_writers;

    init_writers.reserve(num_chains);
    sample_writers.reserve(num_chains);
    diag_writers.reserve(num_chains);
    metric_writers.reserve(num_chains);

    for (int i = 0; i < num_chains; ++i) {
      init_ctxs[i] = std::make_shared<newstan::r_data_context>(init_list);
      init_writers.emplace_back(1);
      sample_writers.emplace_back(expected_rows);
      diag_writers.emplace_back(expected_rows);
      metric_writers.emplace_back(std::make_unique<std::ostringstream>());
    }

    // ── Build per-chain metric contexts from inv_metric ─────────────
    // User can provide:
    //   - Single vector/matrix: same metric for all chains
    //   - List of vectors/matrices: one metric per chain
    // Stored as shared_ptr<var_context> (same pattern as CmdStan)
    bool metric_supplied = args.containsElementNamed("inv_metric") &&
                           !Rcpp::as<bool>(args["inv_metric_na"]);
    std::vector<std::shared_ptr<stan::io::var_context>> metric_ctxs;

    if (metric_supplied) {
      Rcpp::List inv_metric_list = Rcpp::as<Rcpp::List>(args["inv_metric"]);
      size_t num_params = model.num_params_r();

      // Determine if user provided one metric (recycled) or one per chain
      bool per_chain = inv_metric_list.length() == static_cast<R_xlen_t>(num_chains);

      metric_ctxs.reserve(num_chains);
      for (int i = 0; i < num_chains; ++i) {
        SEXP inv_metric_sexp = inv_metric_list[per_chain ? i : 0];

        if (metric == "diag_e") {
          // Diagonal metric: vector of length num_params
          Rcpp::NumericVector inv_metric_vec = Rcpp::as<Rcpp::NumericVector>(inv_metric_sexp);
          std::vector<double> vals(inv_metric_vec.begin(), inv_metric_vec.end());
          std::vector<std::vector<size_t>> dims{{num_params}};
          metric_ctxs.emplace_back(std::make_shared<stan::io::array_var_context>(
              std::vector<std::string>{"inv_metric"}, vals, dims));
        } else if (metric == "dense_e") {
          // Dense metric: matrix of size num_params x num_params
          Rcpp::NumericMatrix inv_metric_mat = Rcpp::as<Rcpp::NumericMatrix>(inv_metric_sexp);
          std::vector<double> vals(inv_metric_mat.begin(), inv_metric_mat.end());
          std::vector<std::vector<size_t>> dims{{num_params, num_params}};
          metric_ctxs.emplace_back(std::make_shared<stan::io::array_var_context>(
              std::vector<std::string>{"inv_metric"}, vals, dims));
        } else {
          // unit_e: inv_metric is ignored
          metric_ctxs.emplace_back(nullptr);
        }
      }
    }

    newstan::r_interrupt interrupt;
    newstan::r_logger logger;

    int return_code = stan::services::error_codes::CONFIG;

    // ── Validation & dispatch ───────────────────────────────────────
    if (algorithm == "hmc" && adapt_engaged && num_warmup == 0) {
      std::ostringstream msg;
      msg << "num_warmup must be > 0 when adapt_engaged is TRUE.";
      logger.error(msg.str());
      // return_code remains CONFIG

    // ── Dispatch: fixed_param ───────────────────────────────────────
    } else if (algorithm == "fixed_param") {
      if (num_chains > 1) {
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
          if (metric_supplied) {
            return_code = stan::services::sample::hmc_nuts_unit_e_adapt(
                model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                delta, gamma, kappa, t0,
                interrupt, logger,
                init_writers, sample_writers, diag_writers,
                metric_writers);
          } else {
            return_code = stan::services::sample::hmc_nuts_unit_e_adapt(
                model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
                init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
                stepsize, stepsize_jitter, max_depth,
                delta, gamma, kappa, t0,
                interrupt, logger,
                init_writers, sample_writers, diag_writers,
                metric_writers);
          }

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
          if (num_chains > 1) {
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
        } else if (metric == "diag_e") {
          if (metric_supplied) {
            // With user-supplied metric, use single-chain with metric context
            // (multi-chain non-adaptive diag_e doesn't accept metric contexts in Stan)
            if (num_chains > 1) {
              std::ostringstream msg;
              msg << "inv_metric with non-adaptive diag_e is only supported for a single chain. "
                  << "Set adapt_engaged = TRUE for multi-chain with custom metric.";
              logger.error(msg.str());
              return_code = stan::services::error_codes::CONFIG;
            } else {
              return_code = stan::services::sample::hmc_nuts_diag_e(
                  model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                  num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers[0],
                  sample_writers[0], diag_writers[0]);
            }
          } else {
            if (num_chains > 1) {
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
            if (num_chains > 1) {
              std::ostringstream msg;
              msg << "inv_metric with non-adaptive dense_e is only supported for a single chain. "
                  << "Set adapt_engaged = TRUE for multi-chain with custom metric.";
              logger.error(msg.str());
              return_code = stan::services::error_codes::CONFIG;
            } else {
              return_code = stan::services::sample::hmc_nuts_dense_e(
                  model, *init_ctxs[0], *metric_ctxs[0], seed, chain_id, init_radius,
                  num_warmup, num_samples, num_thin, save_warmup, refresh,
                  stepsize, stepsize_jitter, max_depth,
                  interrupt, logger, init_writers[0],
                  sample_writers[0], diag_writers[0]);
            }
          } else {
            if (num_chains > 1) {
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
      if (num_chains > 1) {
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

    // ─── Combine results ────────────────────────────────────────────

    Rcpp::List combined = stack_writer_chains(sample_writers, num_chains);

    // ─── Flush buffered log messages on the main R thread ───────────

    logger.flush();

    return Rcpp::List::create(
      Rcpp::_["samples"] = combined,
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "sampling",
      Rcpp::_["algorithm"] = algorithm,
      Rcpp::_["engine"] = engine,
      Rcpp::_["metric"] = metric
    );
  }
}

#endif
