#include <Rinternals.h>

extern "C" SEXP newstan_xptr_is_null(SEXP ptr) {
  return Rf_ScalarLogical(TYPEOF(ptr) != EXTPTRSXP
                          || R_ExternalPtrAddr(ptr) == NULL);
}
