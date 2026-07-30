#include <Rinternals.h>
#include <R_ext/Visibility.h>

extern "C"  {
  static const R_CallMethodDef CallEntries[] = {
    {NULL, NULL, 0}
  };

  attribute_visible void R_init_newstan(DllInfo* dll){
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  }
}
