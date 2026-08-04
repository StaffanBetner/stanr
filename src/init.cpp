#include <Rinternals.h>
#include <R_ext/Visibility.h>

extern "C" SEXP stanr_xptr_is_null(SEXP ptr);

extern "C"  {
  static const R_CallMethodDef CallEntries[] = {
    {"stanr_xptr_is_null", (DL_FUNC) &stanr_xptr_is_null, 1},
    {NULL, NULL, 0}
  };

  attribute_visible void R_init_stanr(DllInfo* dll){
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  }
}
