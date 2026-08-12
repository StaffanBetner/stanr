#include <tbb/task_arena.h>

#include <Rinternals.h>

extern "C" SEXP stanr_max_concurrency(void) {
  return Rf_ScalarInteger(tbb::this_task_arena::max_concurrency());
}
