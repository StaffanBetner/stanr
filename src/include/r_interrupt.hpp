#ifndef NEWSTAN_R_INTERRUPT_H
#define NEWSTAN_R_INTERRUPT_H

#include <Rcpp.h>
#include <stan/callbacks/interrupt.hpp>

namespace newstan {

/**
 * Stan callback::interrupt wrapper.  Rcpp interrupt polling is only enabled
 * for services that execute on the R thread; Stan's multi-chain services
 * invoke callbacks from TBB workers, where calling into R is unsafe.
 *
 * Called by Stan algorithms at the top of their main loops to check
 * for user interrupts (Ctrl-C).
 */
class r_interrupt : public stan::callbacks::interrupt {
 private:
  bool check_r_;

 public:
  explicit r_interrupt(bool check_r = true) : check_r_(check_r) {}

  void operator()() override {
    if (check_r_) Rcpp::checkUserInterrupt();
  }
};

}  // namespace newstan

#endif  // NEWSTAN_R_INTERRUPT_H
