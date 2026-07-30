#ifndef NEWSTAN_RUN_PATHFINDER
#define NEWSTAN_RUN_PATHFINDER

#include <Rcpp.h>
#include <stan/services/pathfinder/single.hpp>
#include <stan/services/pathfinder/multi.hpp>
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
#include <vector>
#include "r_output.hpp"
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include <newstan/r_data_context.hpp>

namespace newstan {
  template <class Model>
  Rcpp::List run_pathfinder(Model& model, Rcpp::List args) {
    const unsigned int seed = Rcpp::as<unsigned int>(args["seed"]);
    const unsigned int chain_id = Rcpp::as<unsigned int>(args["id"]);
    const double init_radius = Rcpp::as<double>(args["init_radius"]);
    const int max_lbfgs_iters = Rcpp::as<int>(args["max_lbfgs_iters"]);
    const int history_size = Rcpp::as<int>(args["history_size"]);
    const int num_elbo_draws = Rcpp::as<int>(args["num_elbo_draws"]);
    const int num_draws = Rcpp::as<int>(args["num_draws"]);
    const int num_paths = Rcpp::as<int>(args["num_paths"]);
    const int num_psis_draws = Rcpp::as<int>(args["num_psis_draws"]);
    const double init_alpha = Rcpp::as<double>(args["init_alpha"]);
    const double tol_obj = Rcpp::as<double>(args["tol_obj"]);
    const double tol_rel_obj = Rcpp::as<double>(args["tol_rel_obj"]);
    const double tol_grad = Rcpp::as<double>(args["tol_grad"]);
    const double tol_rel_grad = Rcpp::as<double>(args["tol_rel_grad"]);
    const double tol_param = Rcpp::as<double>(args["tol_param"]);
    const bool save_single_paths = Rcpp::as<bool>(args["save_single_paths"]);
    const bool psis_resample = Rcpp::as<bool>(args["psis_resample"]);
    const bool calculate_lp = Rcpp::as<bool>(args["calculate_lp"]);
    const int refresh = Rcpp::as<int>(args["refresh"]);
    const bool verbose = Rcpp::as<bool>(args["verbose"]);

    Rcpp::List init_list = Rcpp::as<Rcpp::List>(args["init"]);

    newstan::r_logger logger(verbose);
    // Single-path execution remains on the R thread. Multi-path execution is
    // moved below into a coordinator thread, so it uses an atomic interrupt.
    newstan::r_interrupt interrupt(num_paths <= 1);

    int return_code;

    if (num_paths <= 1) {
      // Single pathfinder
      newstan::r_data_context init_ctx(init_list);
      newstan::r_sample_writer sample_writer(num_draws);
      stan::callbacks::json_writer<std::ostringstream> metric_writer(
          std::make_unique<std::ostringstream>());

      return_code = stan::services::pathfinder::pathfinder_lbfgs_single<false, false>(
          model, init_ctx, seed, chain_id, init_radius,
          history_size, init_alpha,
          tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
          max_lbfgs_iters, num_elbo_draws, num_draws, save_single_paths,
          refresh,
          interrupt, logger,
          sample_writer, sample_writer,
          metric_writer,
          calculate_lp);

      logger.flush();
      return Rcpp::List::create(
        Rcpp::_["return_code"] = return_code,
        Rcpp::_["method"] = "pathfinder",
        Rcpp::_["draws"] = sample_writer.to_r_matrix()
      );
    } else {
      // Multi-path Pathfinder runs in a coordinator std::thread. All data
      // contexts and writers are C++ owned before that thread is launched.
      // The immutable context is safe to share across all paths.
      const auto init_ctx = std::make_unique<newstan::r_data_context>(init_list);
      std::vector<stan::io::var_context*> init_ctxs(num_paths, init_ctx.get());

      // The package returns only the PSIS-resampled draws.  Base writers are
      // intentional no-ops: reporting them as valid would make Stan retain
      // every per-path candidate draw even though none is exposed to R.
      std::vector<stan::callbacks::writer> single_param_writers(num_paths);
      // Per-path diagnostic writers need structured_writer interface
      std::vector<stan::callbacks::json_writer<std::ostringstream>> single_diag_writers;
      single_diag_writers.reserve(num_paths);
      for (int i = 0; i < num_paths; ++i) {
        single_diag_writers.emplace_back(std::make_unique<std::ostringstream>());
      }

      // Final combined parameter writer
      newstan::r_sample_writer param_writer(num_psis_draws);
      // Final diagnostic writer (structured_writer)
      stan::callbacks::json_writer<std::ostringstream> diag_writer(
          std::make_unique<std::ostringstream>());

      // Init writers (one per path, for writing initial values)
      std::vector<stan::callbacks::writer> init_writers(num_paths);

      std::atomic<bool> cancel_requested{false};
      newstan::r_interrupt worker_interrupt(&cancel_requested);
      std::atomic<bool> finished{false};
      std::mutex completion_mutex;
      std::condition_variable completion_cv;
      std::exception_ptr worker_error;

      std::thread worker([&] {
        // The coordinator is not a TBB worker, so it needs an AD stack for
        // Pathfinder's serial setup. The observer installs one per TBB path.
        stan::math::ChainableStack autodiff_stack;
        stan::math::ad_tape_observer autodiff_observer;
        try {
          return_code = stan::services::pathfinder::pathfinder_lbfgs_multi(
              model, std::move(init_ctxs), seed, chain_id, init_radius,
              history_size, init_alpha,
              tol_obj, tol_rel_obj, tol_grad, tol_rel_grad, tol_param,
              max_lbfgs_iters, num_elbo_draws, num_draws, num_psis_draws,
              num_paths, save_single_paths, refresh, worker_interrupt, logger,
              init_writers, single_param_writers, single_diag_writers,
              param_writer, diag_writer, calculate_lp, psis_resample);
        } catch (...) {
          worker_error = std::current_exception();
        }
        finished.store(true, std::memory_order_release);
        completion_cv.notify_one();
      });

      bool interrupted = false;
      while (!finished.load(std::memory_order_acquire)) {
        // Deliberately uses the package's normal logger flush; callers that
        // need a non-R console route may configure that separately.
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
          Rcpp::stop("Unknown exception in Pathfinder worker.");
        }
      }
      if (interrupted) Rcpp::stop("Pathfinder interrupted.");

      return Rcpp::List::create(
        Rcpp::_["return_code"] = return_code,
        Rcpp::_["method"] = "pathfinder",
        Rcpp::_["draws"] = param_writer.to_r_matrix()
      );
    }
  }
}

#endif
