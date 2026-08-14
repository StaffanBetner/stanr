#ifndef STANR_R_WORKER_HPP
#define STANR_R_WORKER_HPP

#include <cpp11.hpp>
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

#if defined(__EMSCRIPTEN__) && !defined(__EMSCRIPTEN_PTHREADS__)
template <class F>
int run_on_worker_thread(stanr::r_logger& logger, const char* what,
                          F&& fn) {
  std::atomic<bool> cancel_requested{false};
  stanr::r_interrupt interrupt(&cancel_requested, what);

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
      cpp11::stop("%s", e.what());
    } catch (...) {
      cpp11::stop("Unknown exception in %s worker.", what);
    }
  }
  if (interrupted) {
    cpp11::stop("%s interrupted.", what);
  }

  return return_code;
}
#endif  // __EMSCRIPTEN__ && !__EMSCRIPTEN_PTHREADS__

}  // namespace stanr

#endif  // STANR_R_WORKER_HPP
