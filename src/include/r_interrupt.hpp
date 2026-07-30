#ifndef NEWSTAN_R_INTERRUPT_H
#define NEWSTAN_R_INTERRUPT_H

#include <Rcpp.h>
#include <stan/callbacks/interrupt.hpp>

#include <atomic>
#include <stdexcept>

namespace newstan {

// Must be called from R's original thread. Rcpp contains the R longjmp and
// converts a pending interrupt into this exception.
inline bool user_interrupt_pending() {
  try {
    Rcpp::checkUserInterrupt();
    return false;
  } catch (const Rcpp::internal::InterruptedException&) {
    return true;
  }
}

/**
 * Stan callback::interrupt wrapper.  Worker-thread users pass an atomic
 * cancellation flag; synchronous services that execute on the R thread may
 * opt into Rcpp interrupt polling.
 *
 * Called by Stan algorithms at the top of their main loops to check
 * for user interrupts (Ctrl-C).
 */
class r_interrupt : public stan::callbacks::interrupt {
 private:
  const std::atomic<bool>* cancel_requested_;
  bool check_r_;

 public:
  // This callback can run on a Stan/TBB worker.  It must therefore only
  // inspect native state; the R thread detects Ctrl-C and sets this flag.
  explicit r_interrupt(const std::atomic<bool>* cancel_requested = nullptr)
      : cancel_requested_(cancel_requested), check_r_(false) {}

  // Retained for synchronous services other than sampling.  Do not use this
  // constructor from a worker thread.
  explicit r_interrupt(bool check_r)
      : cancel_requested_(nullptr), check_r_(check_r) {}

  void operator()() override {
    if (cancel_requested_ &&
        cancel_requested_->load(std::memory_order_relaxed)) {
      throw std::runtime_error("Sampling interrupted.");
    }
    if (check_r_) Rcpp::checkUserInterrupt();
  }
};

}  // namespace newstan

#endif  // NEWSTAN_R_INTERRUPT_H
