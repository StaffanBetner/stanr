#ifndef STANR_R_INTERRUPT_H
#define STANR_R_INTERRUPT_H

#include <Rcpp.h>
#include <stan/callbacks/interrupt.hpp>

#include <atomic>
#include <stdexcept>

namespace stanr {

// Must be called from R's original thread; Rcpp converts a pending
// interrupt into this exception.
inline bool user_interrupt_pending() {
  try {
    Rcpp::checkUserInterrupt();
    return false;
  } catch (const Rcpp::internal::InterruptedException&) {
    return true;
  }
}

// Stan callback::interrupt. Worker-thread users pass an atomic cancellation
// flag; synchronous R-thread services may opt into Rcpp interrupt polling.
class r_interrupt : public stan::callbacks::interrupt {
 private:
  const std::atomic<bool>* cancel_requested_;
  const char* what_;
  bool check_r_;

 public:
  // May run on a Stan/TBB worker, so only inspect native state; the R
  // thread sets the flag on Ctrl-C. `what` names the operation and must
  // outlive this object.
  explicit r_interrupt(const std::atomic<bool>* cancel_requested = nullptr,
                        const char* what = "Sampling")
      : cancel_requested_(cancel_requested), what_(what), check_r_(false) {}

  // For synchronous services; do not use from a worker thread.
  explicit r_interrupt(bool check_r)
      : cancel_requested_(nullptr), what_("Sampling"), check_r_(check_r) {}

  void operator()() override {
    if (cancel_requested_ &&
        cancel_requested_->load(std::memory_order_relaxed)) {
      throw std::runtime_error(std::string(what_) + " interrupted.");
    }
    if (check_r_) Rcpp::checkUserInterrupt();
  }
};

}  // namespace stanr

#endif  // STANR_R_INTERRUPT_H
