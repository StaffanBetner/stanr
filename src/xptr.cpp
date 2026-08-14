#include <cpp11.hpp>
#include <cpp11/declarations.hpp>

extern "C" SEXP stanr_xptr_is_null(SEXP ptr) {
  BEGIN_CPP11
  return cpp11::as_sexp(TYPEOF(ptr) != EXTPTRSXP
                          || R_ExternalPtrAddr(ptr) == NULL);
  END_CPP11
}
