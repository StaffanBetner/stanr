#ifndef STANR_R_WORKER_HPP
#define STANR_R_WORKER_HPP

#include <Rcpp.h>
#include <stan/math/rev/core/chainablestack.hpp>
#include <stan/math/rev/core/init_chainablestack.hpp>
#include <stan/services/error_codes.hpp>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <exception>
#include <mutex>
#include <string>
#include <thread>
#include <utility>
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace stanr {

// Sampling/pathfinder block synchronously with no way to yield to R's event
// loop, so their Stan service call runs on a native coordinator std::thread
// while the R main thread polls every 50ms to flush the buffered logger and
// check for Ctrl-C. Rules for the worker lambda passed to
// run_on_worker_thread():
//
// 1. Never touch the R API from the worker thread -- no Rcpp:: calls, no R
//    allocation, no Rprintf/Rcpp::stop etc. Only plain C++/Stan/Eigen/std::
//    state. The logger buffers messages behind a mutex for this reason;
//    flushing to the console happens only on the main thread's poll loop.
// 2. A raw std::thread sits outside TBB's scheduler, but Stan's multi-chain
//    services initialize chains before entering tbb::parallel_for. The
//    lambda must construct stan::math::ChainableStack autodiff_stack; and
//    stan::math::ad_tape_observer autodiff_observer; at its top, for the
//    job's lifetime -- the observer installs a fresh AD tape in every TBB
//    worker thread that ends up executing a chain.
// 3. On Ctrl-C, the main thread sets an atomic cancellation flag and waits
//    for the worker to finish/join (never abandoned) before raising the
//    R-level interrupt via Rcpp::stop(...).
// 4. Exceptions thrown in the worker must be captured via std::exception_ptr
//    in the lambda's try/catch(...), then rethrown and converted to
//    Rcpp::stop(...) on the main thread after join() -- never let one cross
//    the thread boundary directly, and never call Rcpp::stop from the
//    worker thread itself.
//
// fn's signature is int fn(stanr::r_interrupt& interrupt); it runs
// entirely on the worker thread and must obey the constraints above.
// Returns the int fn produced. `what` is used verbatim in both "<what>
// interrupted." and "Unknown exception in <what> worker.".
template <class F>
int run_on_worker_thread(stanr::r_logger& logger, const char* what,
                          F&& fn) {
  std::atomic<bool> cancel_requested{false};
  stanr::r_interrupt interrupt(&cancel_requested, what);

  std::atomic<bool> finished{false};
  std::mutex completion_mutex;
  std::condition_variable completion_cv;
  std::exception_ptr worker_error;
  int return_code = stan::services::error_codes::CONFIG;

  std::thread worker([&] {
    // Rule 2 above: explicit AD stack plus tape observer for this job.
    stan::math::ChainableStack autodiff_stack;
    stan::math::ad_tape_observer autodiff_observer;
    try {
      return_code = fn(interrupt);
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
      Rcpp::stop(std::string("Unknown exception in ") + what + " worker.");
    }
  }
  if (interrupted) {
    Rcpp::stop(std::string(what) + " interrupted.");
  }

  return return_code;
}

}  // namespace stanr

#endif  // STANR_R_WORKER_HPP
