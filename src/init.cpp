#include <Rinternals.h>
#include <R_ext/Visibility.h>

// Forward declaration of the main entry point (defined in runner.cpp)
extern "C"  {
  SEXP newstan_run(SEXP model_ptr, SEXP args);
  SEXP r_data_context(SEXP data_list);

  static const R_CallMethodDef CallEntries[] = {
    {"newstan_run", (DL_FUNC) &newstan_run, 2},
    {"r_data_context", (DL_FUNC) &r_data_context, 1},
    {NULL, NULL, 0}
  };

  attribute_visible void R_init_newstan(DllInfo* dll){
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  }
}
