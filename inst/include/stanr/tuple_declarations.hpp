#ifndef STANR_TUPLE_DECLARATIONS_HPP
#define STANR_TUPLE_DECLARATIONS_HPP

#include <cpp11.hpp>
#include <string>

namespace stanr {

// Accessors over the declaration structure from `model$variables()`: each
// variable is `list(type = <t>, dimensions = <int>)`, where a tuple's `type`
// is a data.frame with columns `type` (string, or nested data.frame for a
// tuple-typed slot) and `dimensions` (the slot's own array-dim count).
struct slot_decl {
  bool is_tuple = false;
  std::string kind;
  cpp11::list tuple_df;
  int dims = 0;
};

inline int decl_int(SEXP column, int index) {
  if (TYPEOF(column) == INTSXP) return INTEGER(column)[index];
  return static_cast<int>(REAL(column)[index]);
}

inline int tuple_slot_count(cpp11::list type_df) {
  return Rf_length(type_df["dimensions"]);
}

// `x` is either a CHARSXP (from STRING_ELT) or a length-1 STRSXP (from
// VECTOR_ELT); r_string's operator std::string() needs a CHARSXP.
inline std::string sexp_to_string(SEXP x) {
  return TYPEOF(x) == STRSXP ? cpp11::r_string(STRING_ELT(x, 0))
                             : cpp11::r_string(x);
}

inline slot_decl tuple_slot(cpp11::list type_df, int slot) {
  slot_decl out;
  out.dims = decl_int(type_df["dimensions"], slot);
  SEXP type_column = type_df["type"];
  if (TYPEOF(type_column) == STRSXP) {
    out.kind = sexp_to_string(STRING_ELT(type_column, slot));
  } else {
    SEXP type = VECTOR_ELT(type_column, slot);
    if (Rf_inherits(type, "data.frame")) {
      out.is_tuple = true;
      out.tuple_df = type;
    } else {
      out.kind = sexp_to_string(type);
    }
  }
  return out;
}

}  // namespace stanr

#endif  // STANR_TUPLE_DECLARATIONS_HPP
