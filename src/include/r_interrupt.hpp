#ifndef NEWSTAN_R_INTERRUPT_H
#define NEWSTAN_R_INTERRUPT_H

#include <R_ext/Utils.h>
#include <stan/callbacks/interrupt.hpp>

namespace newstan {

/**
 * Stan callback::interrupt wrapper that calls R_CheckUserInterrupt().
 *
 * Called by Stan algorithms at the top of their main loops to check
 * for user interrupts (Ctrl-C).
 */
class r_interrupt : public stan::callbacks::interrupt {
 public:
  void operator()() override { return; }
};

}  // namespace newstan

#endif  // NEWSTAN_R_INTERRUPT_H
