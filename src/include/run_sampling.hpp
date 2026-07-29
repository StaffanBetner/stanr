#ifndef NEWSTAN_RUN_SAMPLING_HPP
#define NEWSTAN_RUN_SAMPLING_HPP

#include <Rcpp.h>
#include <stan/services/sample/hmc_nuts_unit_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_diag_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_dense_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_unit_e.hpp>
#include <stan/services/sample/hmc_nuts_diag_e.hpp>
#include <stan/services/sample/hmc_nuts_dense_e.hpp>
#include <stan/services/sample/fixed_param.hpp>
#include "get_arg.hpp"
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include "r_data_context.hpp"
#include "stack_writer_chains.hpp"

namespace newstan {
  template <class Model>
  Rcpp::List run_sampling(Model& model, Rcpp::List args) {
    std::string algorithm = get_string(args, "algorithm", "hmc");
    std::string engine = get_string(args, "engine", "nuts");
    std::string metric    = get_string(args, "metric", "diag_e");

    unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
    unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
    int num_chains        = Rcpp::as<int>(args["chains"]);
    double init_radius    = Rcpp::as<double>(args["init_radius"]);
    int num_warmup        = Rcpp::as<int>(args["num_warmup"]);
    int num_samples       = Rcpp::as<int>(args["num_samples"]);
    int num_thin          = get_int(args, "thin", 1);
    bool save_warmup      = Rcpp::as<bool>(args["save_warmup"]);
    int refresh           = get_int(args, "refresh", 100);
    double stepsize       = get_double(args, "stepsize", 1.0);
    double stepsize_jitter = get_double(args, "stepsize_jitter", 0.0);
    int max_depth         = get_int(args, "max_depth", 10);

    // Adaptation parameters
    double delta     = get_double(args, "delta", 0.8);
    double gamma     = get_double(args, "gamma", 0.05);
    double kappa     = get_double(args, "kappa", 0.75);
    double t0        = get_double(args, "t0", 10.0);

    // Adaptation window parameters (for diag_e and dense_e metrics)
    unsigned int init_buffer  = get_uint(args, "init_buffer", 75);
    unsigned int term_buffer  = get_uint(args, "term_buffer", 50);
    unsigned int window       = get_uint(args, "window", 25);

    Rcpp::List init_list  = Rcpp::as<Rcpp::List>(args["init"]);

    // Compute expected rows per chain for preallocation
    int expected_rows = ((save_warmup ? num_warmup : 0) + num_samples) / num_thin;

    // Build per-chain contexts and writers
    // Data contexts must be pointers (Stan expects vector<PointerType> and accesses *init[i])
    // Writers must be objects (Stan accesses writer[i] directly as a reference)
    std::vector<newstan::r_data_context*> data_ctxs(num_chains);
    std::vector<newstan::r_data_context*> init_ctxs(num_chains);
    std::vector<newstan::r_sample_writer> init_writers;
    std::vector<newstan::r_sample_writer> sample_writers;
    std::vector<newstan::r_diagnostic_writer> diag_writers;
    std::vector<newstan::r_metric_writer> metric_writers;

    init_writers.reserve(num_chains);
    sample_writers.reserve(num_chains);
    diag_writers.reserve(num_chains);
    metric_writers.reserve(num_chains);

    for (int i = 0; i < num_chains; ++i) {
      init_ctxs[i] = new newstan::r_data_context(init_list);
      init_writers.emplace_back(1);  // init writer only receives one row
      sample_writers.emplace_back(expected_rows);
      diag_writers.emplace_back(expected_rows);
      metric_writers.emplace_back();
    }

    newstan::r_interrupt interrupt;
    newstan::r_logger logger;

    int return_code = stan::services::error_codes::CONFIG;

    // ── NUTS with adaptation ──────────────────────────────────────

    if (algorithm == "NUTS") {
      if (metric == "unit_e") {
        if (num_chains > 1) {
          return_code = stan::services::sample::hmc_nuts_unit_e_adapt(
              model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
              init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              interrupt, logger,
              init_writers, sample_writers, diag_writers, metric_writers);
        } else {
          return_code = stan::services::sample::hmc_nuts_unit_e_adapt(
              model, *init_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              interrupt, logger, init_writers[0], sample_writers[0],
              diag_writers[0], metric_writers[0]);
        }
      } else if (metric == "diag_e") {
        if (num_chains > 1) {
          return_code = stan::services::sample::hmc_nuts_diag_e_adapt(
              model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
              init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger,
              init_writers, sample_writers, diag_writers, metric_writers);
        } else {
          return_code = stan::services::sample::hmc_nuts_diag_e_adapt(
              model, *init_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger, init_writers[0], sample_writers[0],
              diag_writers[0], metric_writers[0]);
        }
      } else if (metric == "dense_e") {
        if (num_chains > 1) {
          return_code = stan::services::sample::hmc_nuts_dense_e_adapt(
              model, static_cast<size_t>(num_chains), init_ctxs, seed, chain_id,
              init_radius, num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger,
              init_writers, sample_writers, diag_writers, metric_writers);
        } else {
          return_code = stan::services::sample::hmc_nuts_dense_e_adapt(
              model, *init_ctxs[0], seed, chain_id, init_radius,
              num_warmup, num_samples, num_thin, save_warmup, refresh,
              stepsize, stepsize_jitter, max_depth,
              delta, gamma, kappa, t0,
              init_buffer, term_buffer, window,
              interrupt, logger, init_writers[0], sample_writers[0],
              diag_writers[0], metric_writers[0]);
        }
      }

    // ── NUTS without adaptation (FIXED) ───────────────────────────

    } else if (algorithm == "NUTS_FIXED") {
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
      } else if (metric == "dense_e") {
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

    // ── HMC (non-NUTS) with adaptation ────────────────────────────

    } else if (algorithm == "HMC") {
      // Static HMC not yet implemented
      std::ostringstream msg;
      msg << "HMC (non-NUTS) sampling not yet implemented.";
      logger.error(msg.str());
      return_code = stan::services::error_codes::CONFIG;

    // ── Fixed param ───────────────────────────────────────────────

    } else if (algorithm == "Fixed_param") {
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

    } else {
      std::ostringstream msg;
      msg << "Unknown sampling algorithm: " << algorithm;
      logger.error(msg.str());
      return_code = stan::services::error_codes::CONFIG;
    }

    // ─── Combine results ──────────────────────────────────────────

    Rcpp::List combined = stack_writer_chains(sample_writers, num_chains);

    // ─── Flush buffered log messages on the main R thread ─────────

    logger.flush();

    // ─── Clean up ──────────────────────────────────────────────────
    for (int i = 0; i < num_chains; ++i) {
      delete init_ctxs[i];
    }
    return Rcpp::List::create(
      Rcpp::_["samples"] = combined,
      Rcpp::_["return_code"] = return_code,
      Rcpp::_["method"] = "sampling",
      Rcpp::_["algorithm"] = algorithm,
      Rcpp::_["metric"] = metric
    );
  }
}

#endif
