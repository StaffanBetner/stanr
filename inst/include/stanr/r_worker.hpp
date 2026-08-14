#ifndef STANR_R_WORKER_HPP
#define STANR_R_WORKER_HPP

#include <cpp11.hpp>
#include <stan/math/rev/core/chainablestack.hpp>
#include <stan/math/rev/core/init_chainablestack.hpp>
#include <stan/services/error_codes.hpp>
#include <atomic>
#include <chrono>
#include <exception>
#include <future>
#include <string>
#include <utility>
#include "r_interrupt.hpp"
#include "r_logger.hpp"

namespace stanr {

#if defined(__EMSCRIPTEN__) && !defined(__EMSCRIPTEN_PTHREADS__)
template <class F>
int run_on_worker_thread(stanr::r_logger& logger, const char* what,
                          F&& fn) {
  stanr::r_interrupt interrupt(&logger, what);

  stan::math::ChainableStack autodiff_stack;
  stan::math::ad_tape_observer autodiff_observer;
  int return_code = stan::services::error_codes::CONFIG;
  try {
    return_code = fn(interrupt);
  } catch (const std::exception& e) {
    logger.flush();
    cpp11::stop("%s", e.what());
  } catch (...) {
    logger.flush();
    cpp11::stop("Unknown exception in %s worker.", what);
  }
  logger.flush();
  return return_code;
}
#else
template <class F>
int run_on_worker_thread(stanr::r_logger& logger, const char* what,
                          F&& fn) {
  std::atomic<bool> cancel_requested{false};
  stanr::r_interrupt interrupt(&cancel_requested, what);

  // std::launch::async must be explicit: the default policy (async |
  // deferred) permits the implementation to defer the call, in which case
  // wait_for() below would return future_status::deferred forever instead
  // of timeout/ready.
  std::future<int> worker = std::async(std::launch::async, [&] {
    // AD stack plus tape observer for this job.
    stan::math::ChainableStack autodiff_stack;
    stan::math::ad_tape_observer autodiff_observer;
    return fn(interrupt);
  });

  // Keep the R thread responsive for console output and Ctrl-C; on Ctrl-C
  // cancel and wait for the worker to unwind before propagating the
  // interrupt to R.
  bool interrupted = false;
  while (worker.wait_for(std::chrono::milliseconds(50)) !=
         std::future_status::ready) {
    logger.flush();
    if (!interrupted && user_interrupt_pending()) {
      interrupted = true;
      cancel_requested.store(true, std::memory_order_release);
    }
  }
  logger.flush();

  // get() blocks only long enough to retrieve the already-ready result, and
  // rethrows any exception the task threw.
  try {
    int return_code = worker.get();
    if (interrupted) {
      cpp11::stop("%s interrupted.", what);
    }
    return return_code;
  } catch (const std::exception& e) {
    cpp11::stop("%s", e.what());
  } catch (...) {
    cpp11::stop("Unknown exception in %s worker.", what);
  }
}
#endif  // __EMSCRIPTEN__ && !__EMSCRIPTEN_PTHREADS__

}  // namespace stanr

#endif  // STANR_R_WORKER_HPP
