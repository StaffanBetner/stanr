#include <cpp11.h>
#include <cpp11/declarations.h>
#include <tbb/task_arena.h>

extern "C" SEXP stanr_max_concurrency(void) {
  BEGIN_CPP11
  return cpp11::as_sexp(tbb::this_task_arena::max_concurrency());
  END_CPP11
}
