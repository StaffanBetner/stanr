#include <cpp11.h>
#include <cpp11/declarations.h>

extern "C" SEXP stanr_xptr_is_null(SEXP ptr) {
  BEGIN_CPP11
  return cpp11::as_sexp(TYPEOF(ptr) != EXTPTRSXP
                          || R_ExternalPtrAddr(ptr) == NULL);
  END_CPP11
}
