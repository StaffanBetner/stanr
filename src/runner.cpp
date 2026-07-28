#include <stan/math/prim/fun/Eigen.hpp>
#include <tbb/global_control.h>
#include <Rcpp.h>
#include <RcppEigen.h>

// ─── Stan services headers ────────────────────────────────────────
#include <stan/services/error_codes.hpp>
#include <stan/callbacks/stream_logger.hpp>
#include <stan/services/sample/hmc_nuts_unit_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_diag_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_dense_e_adapt.hpp>
#include <stan/services/sample/hmc_nuts_unit_e.hpp>
#include <stan/services/sample/hmc_nuts_diag_e.hpp>
#include <stan/services/sample/hmc_nuts_dense_e.hpp>
#include <stan/services/sample/fixed_param.hpp>
#include <stan/services/optimize/bfgs.hpp>
#include <stan/services/optimize/lbfgs.hpp>
#include <stan/services/optimize/newton.hpp>
#include <stan/services/diagnose/diagnose.hpp>
#include <stan/services/experimental/advi/fullrank.hpp>
#include <stan/services/experimental/advi/meanfield.hpp>
#include <stan/services/pathfinder/multi.hpp>
#include <stan/services/pathfinder/single.hpp>
#include <stan/services/sample/standalone_gqs.hpp>
#include <stan/services/optimize/laplace_sample.hpp>
#include <stan/math/prim/core/init_threadpool_tbb.hpp>
#include <stan/model/model_base.hpp>

// ─── Local headers ────────────────────────────────────────────────
#include "r_data_context.hpp"
#include "r_output.hpp"
#include "r_interrupt.h"

// ===================================================================
// Helper: extract optional double from args list
// ===================================================================

static double get_double(Rcpp::List args, const std::string& key, double default_val) {
  if (args.containsElementNamed(key.c_str()))
    return Rcpp::as<double>(args[key]);
  return default_val;
}

static int get_int(Rcpp::List args, const std::string& key, int default_val) {
  if (args.containsElementNamed(key.c_str()))
    return Rcpp::as<int>(args[key]);
  return default_val;
}

static unsigned int get_uint(Rcpp::List args, const std::string& key, unsigned int default_val) {
  if (args.containsElementNamed(key.c_str()))
    return Rcpp::as<unsigned int>(args[key]);
  return default_val;
}

static std::string get_string(Rcpp::List args, const std::string& key, const std::string& default_val) {
  if (args.containsElementNamed(key.c_str()))
    return Rcpp::as<std::string>(args[key]);
  return default_val;
}

// ===================================================================
// Helper: combine per-chain samples and diagnostics into a single result
// ===================================================================

Rcpp::DataFrame combine_chain_results(
    const std::vector<newstan::r_sample_writer>& sample_writers,
    const std::vector<newstan::r_diagnostic_writer>& diag_writers,
    int num_chains) {
  // Use the diagnostic writer as the source — it contains all parameter
  // columns plus extra diagnostic columns (p_*, g_*).  Stack chains
  // into one data.frame and append a "chain" column at the end.

  int total_rows = 0;
  int n_cols = 0;
  for (int i = 0; i < num_chains; ++i) {
    total_rows += diag_writers[i].n_rows();
    if (i == 0) n_cols = diag_writers[i].n_cols();
  }

  // Build column vectors from stacked per-chain data
  Rcpp::List df_list(n_cols);
  for (int j = 0; j < n_cols; ++j) {
    Rcpp::NumericVector col(total_rows);
    int offset = 0;
    for (int i = 0; i < num_chains; ++i) {
      const auto& chain_vals = diag_writers[i].values_col(j);
      for (size_t k = 0; k < chain_vals.size(); ++k) {
        col[offset + k] = chain_vals[k];
      }
      offset += chain_vals.size();
    }
    df_list[j] = col;
  }

  // Append chain ID as the last column
  Rcpp::IntegerVector chain_col(total_rows);
  int offset = 0;
  for (int i = 0; i < num_chains; ++i) {
    int n = diag_writers[i].n_rows();
    for (int j = 0; j < n; ++j) chain_col[offset + j] = i + 1;
    offset += n;
  }
  df_list.push_back(chain_col);

  Rcpp::DataFrame df = Rcpp::DataFrame(df_list);

  // Set column names from first chain's diagnostic writer + "chain"
  Rcpp::CharacterVector names(total_rows > 0 ? n_cols + 1 : 0);
  if (num_chains > 0 && !diag_writers[0].colnames().empty()) {
    for (int j = 0; j < n_cols; ++j) {
      names[j] = diag_writers[0].colnames()[j];
    }
  }
  if (n_cols + 1 <= names.size()) names[n_cols] = ".chain";
  df.names() = names;

  return df;
}

// ===================================================================
// Helper: dispatch sampling to single-chain or multi-chain overload
// Returns a List with samples, diagnostics, and return_code.
// ===================================================================

template <class Model>
Rcpp::List run_sampling(Model& model, Rcpp::List args) {
  std::string algorithm = get_string(args, "algorithm", "NUTS");
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
  stan::callbacks::stream_writer info(std::cout);
  stan::callbacks::stream_writer err(std::cerr);
  stan::callbacks::stream_logger logger(std::cout, std::cout, std::cout,
                                        std::cerr, std::cerr);

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

  Rcpp::List combined = combine_chain_results(sample_writers, diag_writers, num_chains);


  // ─── Clean up ──────────────────────────────────────────────────
/*
  for (int i = 0; i < num_chains; ++i) {
    delete init_ctxs[i];
  }
*/
  return Rcpp::List::create(
    Rcpp::_["samples"] = combined,
    Rcpp::_["return_code"] = return_code,
    Rcpp::_["method"] = "sampling",
    Rcpp::_["algorithm"] = algorithm,
    Rcpp::_["metric"] = metric
  );
}

// ===================================================================
// Helper: dispatch optimizing
// Returns a List with results and return_code.
// ===================================================================

template <class Model>
Rcpp::List run_optimizing(Model& model, Rcpp::List args) {
  std::string algorithm = get_string(args, "algorithm", "bfgs");

  unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
  unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
  double init_radius    = Rcpp::as<double>(args["init_radius"]);
  int iter              = get_int(args, "iter", 2000);
  bool save_iterations  = Rcpp::as<bool>(args["save_iterations"]);
  bool verbose          = Rcpp::as<bool>(args["verbose"]);

  Rcpp::List data_list  = Rcpp::as<Rcpp::List>(args["data"]);
  Rcpp::List init_list  = args.containsElementNamed("init")
                      ? Rcpp::as<Rcpp::List>(args["init"])
                      : data_list;

  newstan::r_data_context init_ctx(init_list);
  newstan::r_sample_writer sample_writer;
  newstan::r_logger logger(verbose);
  newstan::r_interrupt interrupt;

  int return_code = stan::services::error_codes::CONFIG;

  if (algorithm == "newton") {
    return_code = stan::services::optimize::newton(
        model, init_ctx, seed, chain_id, init_radius,
        iter, save_iterations,
        interrupt, logger,
        /*init_writer=*/sample_writer, sample_writer);
  } else if (algorithm == "bfgs") {
    double init_alpha      = get_double(args, "init_alpha", 0.001);
    double tol_obj         = get_double(args, "tol_obj", 1e-12);
    double tol_rel_obj     = get_double(args, "tol_rel_obj", 10000.0);
    double tol_grad        = get_double(args, "tol_grad", 1e-8);
    double tol_rel_grad    = get_double(args, "tol_rel_grad", 1e7);
    double tol_param       = get_double(args, "tol_param", 1e-8);
    int refresh            = get_int(args, "refresh", 100);

    return_code = stan::services::optimize::bfgs(
        model, init_ctx, seed, chain_id, init_radius,
        init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
        iter, save_iterations, refresh,
        interrupt, logger,
        /*init_writer=*/sample_writer, sample_writer);
  } else if (algorithm == "lbfgs") {
    double init_alpha      = get_double(args, "init_alpha", 0.001);
    double tol_obj         = get_double(args, "tol_obj", 1e-12);
    double tol_rel_obj     = get_double(args, "tol_rel_obj", 10000.0);
    double tol_grad        = get_double(args, "tol_grad", 1e-8);
    double tol_rel_grad    = get_double(args, "tol_rel_grad", 1e7);
    double tol_param       = get_double(args, "tol_param", 1e-8);
    int history_size       = get_int(args, "history_size", 5);
    int refresh            = get_int(args, "refresh", 100);

    return_code = stan::services::optimize::lbfgs(
        model, init_ctx, seed, chain_id, init_radius,
        history_size, init_alpha, tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
        iter, save_iterations, refresh,
        interrupt, logger,
        /*init_writer=*/sample_writer, sample_writer);
  } else {
    std::ostringstream msg;
    msg << "Unknown optimization algorithm: " << algorithm;
    logger.error(msg.str());
    return_code = stan::services::error_codes::CONFIG;
  }

  // Extract results from sample_writer
  Rcpp::DataFrame df = sample_writer.to_dataframe();
  Rcpp::NumericVector par_vec;
  double lp_val = NA_REAL;

  if (df.nrow() > 0 && df.ncol() >= 1) {
    // Last row contains the solution; first column is lp__
    int last_row = df.nrow() - 1;
    Rcpp::CharacterVector colnames = df.names();
    for (int j = 0; j < df.ncol(); ++j) {
      if (colnames[j] == "lp__") {
        Rcpp::NumericVector col = df[j];
        lp_val = col[last_row];
        break;
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::_["par"] = df,
    Rcpp::_["value"] = lp_val,
    Rcpp::_["return_code"] = return_code,
    Rcpp::_["method"] = "optimizing",
    Rcpp::_["algorithm"] = algorithm
  );
}

// ===================================================================
// Helper: dispatch gradient check (diagnose)
// Returns number of failed parameters.
// ===================================================================

template <class Model>
Rcpp::List run_diagnose(Model& model, Rcpp::List args) {
  unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
  unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
  double init_radius    = Rcpp::as<double>(args["init_radius"]);
  double epsilon        = get_double(args, "epsilon", 1e-6);
  double error_thresh   = get_double(args, "error", 1e-6);
  bool verbose          = Rcpp::as<bool>(args["verbose"]);

  Rcpp::List init_list = args.containsElementNamed("init")
                      ? Rcpp::as<Rcpp::List>(args["init"])
                      : Rcpp::as<Rcpp::List>(args["data"]);

  newstan::r_data_context init_ctx(init_list);
  newstan::r_sample_writer sample_writer;
  newstan::r_logger logger(verbose);
  newstan::r_interrupt interrupt;

  // diagnose returns number of failed parameters (not error code)
  int n_failed = stan::services::diagnose::diagnose(
      model, init_ctx, seed, chain_id, init_radius,
      epsilon, error_thresh,
      interrupt, logger,
      /*init_writer=*/sample_writer, sample_writer);

  return Rcpp::List::create(
    Rcpp::_["num_failed"] = n_failed,
    Rcpp::_["return_code"] = n_failed == 0 ? 0 : 1,
    Rcpp::_["method"] = "diagnose"
  );
}

// ===================================================================
// Helper: dispatch ADVI
// ===================================================================

template <class Model>
Rcpp::List run_advi(Model& model, Rcpp::List args) {
  std::string algorithm = get_string(args, "algorithm", "fullrank");

  unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
  unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
  double init_radius    = Rcpp::as<double>(args["init_radius"]);
  int iter              = get_int(args, "iter", 10000);
  int grad_samples      = get_int(args, "grad_samples", 1);
  int elbo_samples      = get_int(args, "elbo_samples", 100);
  double tol_rel_obj    = get_double(args, "tol_rel_obj", 0.01);
  double eta            = get_double(args, "eta", 1.0);
  bool adapt_engaged    = Rcpp::as<bool>(args["adapt_engaged"]);
  int adapt_iter        = get_int(args, "adapt_iter", 50);
  int eval_elbo         = get_int(args, "eval_elbo", 100);
  int output_samples    = get_int(args, "output_samples", 1000);
  bool verbose          = Rcpp::as<bool>(args["verbose"]);

  Rcpp::List data_list  = Rcpp::as<Rcpp::List>(args["data"]);
  Rcpp::List init_list  = args.containsElementNamed("init")
                      ? Rcpp::as<Rcpp::List>(args["init"])
                      : data_list;

  newstan::r_data_context init_ctx(init_list);
  newstan::r_sample_writer sample_writer;
  newstan::r_logger logger(verbose);
  newstan::r_interrupt interrupt;

  int return_code = stan::services::error_codes::CONFIG;

  if (algorithm == "fullrank") {
    return_code = stan::services::experimental::advi::fullrank(
        model, init_ctx, seed, chain_id, init_radius,
        grad_samples, elbo_samples, iter, tol_rel_obj, eta,
        adapt_engaged, adapt_iter, eval_elbo, output_samples,
        interrupt, logger,
        /*init_writer=*/sample_writer, sample_writer,
        /*diagnostic_writer=*/sample_writer);
  } else if (algorithm == "meanfield") {
    return_code = stan::services::experimental::advi::meanfield(
        model, init_ctx, seed, chain_id, init_radius,
        grad_samples, elbo_samples, iter, tol_rel_obj, eta,
        adapt_engaged, adapt_iter, eval_elbo, output_samples,
        interrupt, logger,
        /*init_writer=*/sample_writer, sample_writer,
        /*diagnostic_writer=*/sample_writer);
  }

  return Rcpp::List::create(
    Rcpp::_["return_code"] = return_code,
    Rcpp::_["method"] = "advi",
    Rcpp::_["algorithm"] = algorithm
  );
}

// ===================================================================
// Helper: dispatch generated quantities (standalone_gqs)
// ===================================================================

template <class Model>
Rcpp::List run_standalone_gqs(Model& model, Rcpp::List args) {
  unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
  unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
  bool verbose          = Rcpp::as<bool>(args["verbose"]);

  // draws: Eigen::MatrixXd (rows=samples, columns=parameters)
  Eigen::Map<Eigen::MatrixXd> draws =
      Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(args["draws"]);

  newstan::r_sample_writer sample_writer;
  newstan::r_logger logger(verbose);
  newstan::r_interrupt interrupt;

  int return_code = stan::services::standalone_generate(
      model, draws, seed,
      interrupt, logger, sample_writer);

  return Rcpp::List::create(
    Rcpp::_["samples"] = sample_writer.to_dataframe(),
    Rcpp::_["return_code"] = return_code,
    Rcpp::_["method"] = "standalone_gqs"
  );
}

// ===================================================================
// Helper: dispatch pathfinder
// ===================================================================

template <class Model>
Rcpp::List run_pathfinder(Model& model, Rcpp::List args) {
  unsigned int seed     = Rcpp::as<unsigned int>(args["seed"]);
  unsigned int chain_id = Rcpp::as<unsigned int>(args["chain_id"]);
  double init_radius    = Rcpp::as<double>(args["init_radius"]);
  int iter              = get_int(args, "iter", 500);
  int history_size      = get_int(args, "history_size", 5);
  int num_elbo_draws    = get_int(args, "num_elbo_draws", 64);
  int num_draws         = get_int(args, "num_draws", 300);
  bool verbose          = Rcpp::as<bool>(args["verbose"]);

  Rcpp::List data_list  = Rcpp::as<Rcpp::List>(args["data"]);
  Rcpp::List init_list  = args.containsElementNamed("init")
                      ? Rcpp::as<Rcpp::List>(args["init"])
                      : data_list;

  newstan::r_data_context init_ctx(init_list);
  newstan::r_sample_writer sample_writer;
  newstan::r_metric_writer metric_writer;
  newstan::r_logger logger(verbose);
  newstan::r_interrupt interrupt;

  int return_code = stan::services::pathfinder::pathfinder_lbfgs_single<false, false>(
      model, init_ctx, seed, chain_id, init_radius,
      history_size, 0.001,  // init_alpha, default
      1e-12, 10000.0, 1e-8, 1e7, 1e-8,  // tolerances
      iter, num_elbo_draws, num_draws, false,
      get_int(args, "refresh", 100),
      interrupt, logger,
      sample_writer, sample_writer,
      metric_writer,
      true);  // calculate_lp

  return Rcpp::List::create(
    Rcpp::_["return_code"] = return_code,
    Rcpp::_["method"] = "pathfinder"
  );
}

extern "C" SEXP newstan_run(SEXP model_ptr, SEXP args) {
  stan::math::init_threadpool_tbb();

  auto model = Rcpp::XPtr<stan::model::model_base>(model_ptr);

  // Extract method from args
  std::string method = get_string(args, "method", "sampling");

  int return_code = stan::services::error_codes::CONFIG;

  if (method == "sampling") {
    return Rcpp::wrap(run_sampling(*model, args));
  } else if (method == "optimizing") {
    return Rcpp::wrap(run_optimizing(*model, args));
  } else if (method == "diagnose") {
    return Rcpp::wrap(run_diagnose(*model, args));
  } else if (method == "advi") {
    return Rcpp::wrap(run_advi(*model, args));
  } else if (method == "standalone_gqs") {
    return Rcpp::wrap(run_standalone_gqs(*model, args));
  } else if (method == "pathfinder") {
    return Rcpp::wrap(run_pathfinder(*model, args));
  } else {
    std::ostringstream msg;
    msg << "Unknown method: " << method;
    std::cout << msg.str() << std::endl;
    return_code = stan::services::error_codes::CONFIG;
  }

  // Fallback: build minimal result list
  return Rcpp::List::create(
    Rcpp::_["return_code"] = return_code,
    Rcpp::_["method"] = method
  );
}

extern "C" SEXP r_data_context(SEXP data_list) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::io::var_context> m(new newstan::r_data_context(Rcpp::List(data_list)));
  return Rcpp::wrap(m);
  END_RCPP
}
