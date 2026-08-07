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
// loop, so their Stan service runs on a native coordinator std::thread while
// the R main thread polls every 50ms to flush the logger and check Ctrl-C.
// The worker lambda must: (1) never touch the R API; (2) construct a
// ChainableStack + ad_tape_observer at its top; (3) on Ctrl-C the main
// thread sets an atomic flag and joins before raising the interrupt; (4)
// capture exceptions via std::exception_ptr and rethrow on the main thread.
// fn is int fn(stanr::r_interrupt&); `what` names the operation.
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
    // AD stack plus tape observer for this job.
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

  // Keep the R thread responsive for console output and Ctrl-C; on Ctrl-C
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
