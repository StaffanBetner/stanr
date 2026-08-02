#ifndef NEWSTAN_R_WORKER_HPP
#define NEWSTAN_R_WORKER_HPP

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

namespace newstan {

// Sampling/pathfinder run their Stan service call on a native coordinator
// std::thread while the R main thread polls a 50ms loop to flush the
// buffered logger and check for Ctrl-C.  This exists because Stan's
// sampling/pathfinder services block synchronously and provide no way to
// yield to R's event loop mid-run; running them on a separate thread lets
// the main R thread stay responsive.  Rules that govern the worker lambda
// passed to run_on_worker_thread():
//
// 1. Code inside the worker-thread lambda must never touch the R API -- no
//    Rcpp:: calls, no R allocation, no Rprintf/Rcpp::stop etc.  Only plain
//    C++/Stan/Eigen/std:: state.  The logger buffers messages behind a
//    mutex specifically so the worker can log without touching R; flushing
//    to the console happens only on the main thread's poll loop.
// 2. A raw std::thread is outside TBB's scheduler, and Stan's multi-chain
//    services initialize chains before entering tbb::parallel_for.  The
//    worker lambda must construct stan::math::ChainableStack autodiff_stack;
//    and stan::math::ad_tape_observer autodiff_observer; at its top, for the
//    lifetime of the job -- the observer installs a fresh AD tape in every
//    TBB worker thread that ends up executing a chain.
// 3. On Ctrl-C, the main thread sets an atomic cancellation flag, waits for
//    the worker to actually finish/join (never abandon the thread), *then*
//    raises the R-level interrupt via Rcpp::stop(...).
// 4. Any exception thrown inside the worker must be captured via
//    std::exception_ptr inside the lambda's try/catch(...), then rethrown
//    and converted to Rcpp::stop(...) on the main thread after join() --
//    never let an exception cross the thread boundary directly, and never
//    call Rcpp::stop from the worker thread itself.
//
// fn's signature is int fn(newstan::r_interrupt& interrupt); it runs
// entirely on the worker thread and must obey the constraints above.
// Returns the int fn produced.  `what` is used verbatim in "<what>
// interrupted."; `unknown_exception_what` is used verbatim in "Unknown
// exception in <unknown_exception_what> worker." -- these are separate
// parameters (rather than one reused string) because the two pre-existing
// call sites disagree on capitalization ("Sampling interrupted." vs.
// "Unknown exception in sampling worker.", lowercase) and this preserves
// both exactly.
template <class F>
int run_on_worker_thread(newstan::r_logger& logger, const char* what,
                          const char* unknown_exception_what, F&& fn) {
  std::atomic<bool> cancel_requested{false};
  newstan::r_interrupt interrupt(&cancel_requested);

  std::atomic<bool> finished{false};
  std::mutex completion_mutex;
  std::condition_variable completion_cv;
  std::exception_ptr worker_error;
  int return_code = stan::services::error_codes::CONFIG;

  // Everything in this lambda is restricted to C++/Stan state.  In
  // particular, it must not allocate R objects or call into Rcpp.
  std::thread worker([&] {
    // A raw std::thread is outside TBB's scheduler, and the sampling service
    // initializes chains before it enters parallel_for.  Its AD stack must
    // be explicit.  Attach an observer for the lifetime of this job as
    // well: it initializes a separate AD tape in every TBB worker that
    // executes a chain.
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
      Rcpp::stop(std::string("Unknown exception in ") + unknown_exception_what
                 + " worker.");
    }
  }
  if (interrupted) {
    Rcpp::stop(std::string(what) + " interrupted.");
  }

  return return_code;
}

}  // namespace newstan

#endif  // NEWSTAN_R_WORKER_HPP
