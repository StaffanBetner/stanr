#' @param data (named list) Values for all or part of the data `block` in the
#'   Stan program. Each element is named after the corresponding Stan
#'   variable and shaped as follows:
#'
#'   | Stan type | R shape |
#'   | --- | --- |
#'   | `complex` | complex scalar (e.g. `1+2i`) |
#'   | `complex_vector[n]`, `complex_row_vector[n]`, `array[n] complex` | complex vector of length `n` |
#'   | `complex_matrix[r, c]` | complex matrix, `r` x `c` |
#'   | `tuple(T1, ..., Tk)` | **unnamed** list of length `k`, element `j` shaped as `Tj` |
#'   | `array[n] tuple(...)` | list of `n` tuple-shaped lists |
#'   | `array[n, m] tuple(...)` | list (over the first index) of lists (over the second index) of tuples |
#'
#'   Real-valued input is accepted wherever a `complex` type is declared --
#'   it is coerced automatically, including inside tuple slots. All other
#'   Stan types use their ordinary R representation (numeric/integer scalars,
#'   vectors, matrices, arrays).
#'
#'   As a power-user escape hatch, tuple variables can instead be supplied
#'   pre-flattened using dotted names (e.g. `list(t.1 = ..., t.2 = ...)` for
#'   a tuple `t`), bypassing the list-shape conversion above; this is not the
#'   primary documented path. One caveat applies: a dotted entry for a
#'   `complex` tuple slot that omits the internal `newstan_array_dims`
#'   attribute is assumed to hold exactly one complex value per enclosing
#'   array element, which is only correct when that slot is itself a plain
#'   (non-container) `complex` -- for anything else (e.g. a
#'   `complex_vector` tuple slot), use the list shapes above instead.
