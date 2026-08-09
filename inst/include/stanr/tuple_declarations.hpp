#ifndef STANR_TUPLE_DECLARATIONS_HPP
#define STANR_TUPLE_DECLARATIONS_HPP

#include <Rcpp.h>
#include <string>

namespace stanr {

// Accessors over the declaration structure from `model$variables()`: each
// variable is `list(type = <t>, dimensions = <int>)`, where a tuple's `type`
// is a data.frame with columns `type` (string, or nested data.frame for a
// tuple-typed slot) and `dimensions` (the slot's own array-dim count).
struct slot_decl {
  bool is_tuple = false;
  std::string kind;
  Rcpp::List tuple_df;
  int dims = 0;
};

inline int decl_int(SEXP column, int index) {
  if (TYPEOF(column) == INTSXP) return INTEGER(column)[index];
  return static_cast<int>(REAL(column)[index]);
}

inline int tuple_slot_count(Rcpp::List type_df) {
  return Rf_length(type_df["dimensions"]);
}

inline slot_decl tuple_slot(Rcpp::List type_df, int slot) {
  slot_decl out;
  out.dims = decl_int(type_df["dimensions"], slot);
  SEXP type_column = type_df["type"];
  if (TYPEOF(type_column) == STRSXP) {
    out.kind = Rcpp::as<std::string>(STRING_ELT(type_column, slot));
  } else {
    SEXP type = VECTOR_ELT(type_column, slot);
    if (Rf_inherits(type, "data.frame")) {
      out.is_tuple = true;
      out.tuple_df = type;
    } else {
      out.kind = Rcpp::as<std::string>(type);
    }
  }
  return out;
}

}  // namespace stanr

#endif  // STANR_TUPLE_DECLARATIONS_HPP
