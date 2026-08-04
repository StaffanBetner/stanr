# R-side flattening of tuple-typed `data =` / `init =` / `unconstrain_variables()`
# values into the dotted-name, per-leaf entries that `r_data_context` (and the
# generated Stan reader) actually expect:
#
#   * Every tuple-typed variable `x` is read by generated code slot-by-slot,
#     never as a whole -- dotted names `x.1`, `x.2.1`, ... one per leaf
#     (non-tuple slot reachable through nested tuples).
#   * For array-of-tuple variables the flat value order is "blocked AoS":
#     enclosing-array elements are enumerated column-major (first index
#     fastest, outer levels slower) and each element's leaf payload is
#     concatenated contiguously -- this is *not* the column-major order of
#     the declared dims.
#   * Complex leaves additionally carry a `newstan_array_dims` attribute (the
#     count of enclosing array dims) so `r_data_context` can build the
#     windowed `vals_c` layout stanc 2.39 requires for tuple-slot complex data
#     (see src/r_data_context.cpp).

# TRUE if `x` has any non-empty name -- used to reject named lists, since
# tuple values are represented as unnamed R lists, never named ones.
.newstan_has_names <- function(x) {
  nm <- names(x)
  !is.null(nm) && any(nzchar(nm))
}

# The "own shape" of one leaf payload: its `dim` attribute if present,
# otherwise its length if > 1, otherwise nothing (a bare scalar). Used both
# to validate that every enclosing-array element supplies the same shape and
# to build the leaf's stored `dim`.
.newstan_payload_shape <- function(x) {
  d <- dim(x)
  if (!is.null(d)) {
    return(as.integer(d))
  }
  if (length(x) > 1L) {
    return(length(x))
  }
  integer(0)
}

# Validate that `value` is an unnamed list nested `k` levels deep (one level
# per enclosing array dimension, outermost first, matching the canonical R
# shape for array-of-tuple values) and return the array's size at each level,
# in Stan declaration order. Errors on non-list/named values and on
# non-rectangular arrays (sibling elements whose nested shape disagrees).
.newstan_tuple_array_shape <- function(name, value, k) {
  if (!is.list(value) || .newstan_has_names(value)) {
    stop(
      "`",
      name,
      "` must be an unnamed list (",
      k,
      if (k == 1L) " level" else " levels",
      " deep, matching its declared array dimensions); see the tuple data ",
      "shape documentation.",
      call. = FALSE
    )
  }
  d1 <- length(value)
  if (k == 1L) {
    return(d1)
  }
  sizes <- NULL
  for (i in seq_len(d1)) {
    shape_i <- .newstan_tuple_array_shape(
      paste0(name, "[[", i, "]]"),
      value[[i]],
      k - 1L
    )
    if (is.null(sizes)) {
      sizes <- shape_i
    } else if (!identical(sizes, shape_i)) {
      stop(
        "`",
        name,
        "` is not a rectangular tuple array: element ",
        i,
        " has shape ",
        paste(shape_i, collapse = "x"),
        " but a previous element has shape ",
        paste(sizes, collapse = "x"),
        ".",
        call. = FALSE
      )
    }
  }
  c(d1, sizes)
}

# Flatten a nested list (already validated by `.newstan_tuple_array_shape`,
# with per-level sizes `sizes`) into a single flat list of elements enumerated
# column-major -- first (outermost) index fastest, later indices slower. This
# is the enumeration order the blocked flat-storage layout described in the
# file header requires for tuple-slot leaves.
.newstan_enumerate_tuple_elements <- function(value, sizes) {
  k <- length(sizes)
  if (k == 0L) {
    return(list(value))
  }
  if (k == 1L) {
    return(value)
  }
  d1 <- sizes[1]
  sub <- lapply(seq_len(d1), function(i) {
    .newstan_enumerate_tuple_elements(value[[i]], sizes[-1])
  })
  rest_n <- prod(sizes[-1])
  out <- vector("list", d1 * rest_n)
  idx <- 1L
  for (r in seq_len(rest_n)) {
    for (i in seq_len(d1)) {
      out[[idx]] <- sub[[i]][[r]]
      idx <- idx + 1L
    }
  }
  out
}

# Build one leaf entry (a non-tuple slot's flattened value across every
# already-enumerated enclosing-array element). `array_sizes` is the
# accumulated enclosing-array dim sizes (outermost first) up to and including
# this leaf's own tuple-array nesting.
.newstan_flatten_tuple_leaf <- function(
  slot_name,
  slot_vals,
  slot_type,
  array_sizes
) {
  if (!length(slot_vals)) {
    stop(
      "`",
      slot_name,
      "` has no enclosing array elements to flatten.",
      call. = FALSE
    )
  }
  ref_shape <- .newstan_payload_shape(slot_vals[[1]])
  if (length(slot_vals) > 1L) {
    for (i in 2:length(slot_vals)) {
      shape_i <- .newstan_payload_shape(slot_vals[[i]])
      if (!identical(shape_i, ref_shape)) {
        stop(
          "`",
          slot_name,
          "` has inconsistent value shapes across enclosing ",
          "array elements: element ",
          i,
          " has shape ",
          if (length(shape_i)) paste(shape_i, collapse = "x") else "scalar",
          " but element 1 has shape ",
          if (length(ref_shape)) paste(ref_shape, collapse = "x") else "scalar",
          ".",
          call. = FALSE
        )
      }
    }
  }
  is_complex_slot <- identical(slot_type, "complex")
  payloads <- if (is_complex_slot) lapply(slot_vals, as.complex) else slot_vals
  flat <- do.call(c, payloads)
  total_dims <- c(array_sizes, ref_shape)
  if (length(total_dims) >= 2L) {
    # A leaf's `dim` is only set once there are >= 2 dims; a single dim
    # stays a bare vector, and a true scalar (no dims at all) stays a bare
    # scalar.
    dim(flat) <- as.integer(total_dims)
  }
  if (is_complex_slot) {
    # `newstan_array_dims` counts only the enclosing array dims -- never
    # this leaf's own container dims (e.g. a complex_vector's own length).
    attr(flat, "newstan_array_dims") <- as.integer(length(array_sizes))
  }
  flat
}

# Recursive worker over one tuple level. `elements` is the flat list of
# already-enumerated tuple instances at this level (each an unnamed list of
# length `nrow(type_df)`); `array_sizes` is the accumulated enclosing-array
# dim sizes up to and including this level's own array (if any); `type_df`
# is this level's slot data.frame (columns `type`, `dimensions`, one row per
# slot -- `type` is a data.frame for a nested-tuple slot, else a string).
# Leaf entries are written into `out` (an environment) keyed by dotted name.
.newstan_flatten_tuple_recurse <- function(
  name,
  elements,
  array_sizes,
  type_df,
  out
) {
  n_slots <- nrow(type_df)
  for (el in elements) {
    if (!is.list(el) || .newstan_has_names(el) || length(el) != n_slots) {
      stop(
        "`",
        name,
        "` must be an unnamed list of length ",
        n_slots,
        " (one entry per tuple slot); see the tuple data shape documentation.",
        call. = FALSE
      )
    }
  }
  for (s in seq_len(n_slots)) {
    slot_name <- paste0(name, ".", s)
    slot_vals <- lapply(elements, `[[`, s)
    slot_type <- type_df$type[[s]]
    slot_dims <- type_df$dimensions[[s]]
    if (is.data.frame(slot_type)) {
      # Nested tuple slot: `slot_dims` is *this slot's own* array dim count
      # (0 for a plain nested tuple, > 0 for e.g. `array[2] tuple(...)`).
      if (identical(slot_dims, 0L) || identical(slot_dims, 0)) {
        new_elements <- slot_vals
        new_array_sizes <- array_sizes
      } else {
        inner_sizes <- NULL
        inner_lists <- vector("list", length(slot_vals))
        for (i in seq_along(slot_vals)) {
          shape_i <- .newstan_tuple_array_shape(
            paste0(slot_name, "[", i, "]"),
            slot_vals[[i]],
            slot_dims
          )
          if (is.null(inner_sizes)) {
            inner_sizes <- shape_i
          } else if (!identical(inner_sizes, shape_i)) {
            stop(
              "`",
              slot_name,
              "` is not a rectangular tuple array across ",
              "enclosing elements: element ",
              i,
              " has shape ",
              paste(shape_i, collapse = "x"),
              " but a previous element has ",
              "shape ",
              paste(inner_sizes, collapse = "x"),
              ".",
              call. = FALSE
            )
          }
          inner_lists[[i]] <- .newstan_enumerate_tuple_elements(
            slot_vals[[i]],
            inner_sizes
          )
        }
        # Concatenating in outer-element order preserves blocked storage:
        # each outer element's inner elements stay contiguous.
        new_elements <- unlist(inner_lists, recursive = FALSE)
        new_array_sizes <- c(array_sizes, inner_sizes)
      }
      .newstan_flatten_tuple_recurse(
        slot_name,
        new_elements,
        new_array_sizes,
        slot_type,
        out
      )
    } else {
      out[[slot_name]] <- .newstan_flatten_tuple_leaf(
        slot_name,
        slot_vals,
        slot_type,
        array_sizes
      )
    }
  }
  invisible(NULL)
}

# Flatten one tuple-typed variable's value into its dotted leaf entries.
# `value`: the canonical R shape for this variable (an unnamed list, or
# nested unnamed lists of depth `n_array_dims` for an array-of-tuple).
# `type_df` / `n_array_dims`: this variable's own `declared[[name]]$type` /
# `$dimensions`.
.newstan_flatten_one_tuple <- function(name, value, type_df, n_array_dims) {
  if (n_array_dims == 0L) {
    elements <- list(value)
    array_sizes <- integer(0)
  } else {
    array_sizes <- .newstan_tuple_array_shape(name, value, n_array_dims)
    elements <- .newstan_enumerate_tuple_elements(value, array_sizes)
  }
  out <- new.env(parent = emptyenv())
  .newstan_flatten_tuple_recurse(name, elements, array_sizes, type_df, out)
  as.list(out)
}

#' Flatten tuple-typed values in a data/init list into dotted leaf entries
#'
#' Replaces every list-valued entry of `values` that is declared as a tuple
#' (per `declared`, one block of `mod$variables()`) with the dotted per-leaf
#' entries `r_data_context` expects. Entries that are not lists pass through
#' unchanged, including any user-supplied dotted entries (the manual escape
#' hatch).
#'
#' @param values A named list of data or init values.
#' @param declared One block of `mod$variables()` (e.g. `$data` or
#'   `$parameters`): a named list where each element is
#'   `list(type = <t>, dimensions = <int>)`.
#'
#' @return `values` with every declared-tuple list entry replaced by its
#'   flattened dotted leaves.
#' @noRd
.newstan_flatten_tuple_values <- function(values, declared) {
  if (!length(values)) {
    return(values)
  }
  is_bare_list <- vapply(values, is.list, logical(1))
  if (!any(is_bare_list)) {
    return(values)
  }

  result <- values
  for (name in names(values)[is_bare_list]) {
    decl <- declared[[name]]
    if (is.null(decl) || !is.data.frame(decl$type)) {
      stop(
        "`",
        name,
        "` is not declared as a tuple; lists are only accepted ",
        "for tuple variables.",
        call. = FALSE
      )
    }
    leaves <- .newstan_flatten_one_tuple(
      name,
      values[[name]],
      decl$type,
      decl$dimensions
    )
    result[[name]] <- NULL
    leaf_names <- names(leaves)
    collision <- leaf_names %in% names(result)
    if (any(collision)) {
      stop(
        "Flattening `",
        name,
        "` would create entries that already exist: ",
        paste0("`", leaf_names[collision], "`", collapse = ", "),
        ". Remove the manually-supplied dotted entry or the list value.",
        call. = FALSE
      )
    }
    result[leaf_names] <- leaves
  }
  result
}

# --- Native output -> canonical R shapes -------------------------------------
#
# The functions below implement the *other* direction: turning the model's
# native flat constrained-parameter output (or, for `variable_skeleton()`, an
# empty shape with no values yet) back into canonical nested-list R shapes.
# This is deliberately NOT the inverse of the flattener above: the native
# flat-draws order for array-of-tuples is *element-major* ("ta.1:1,
# ta.1:2.real, ta.1:2.imag, ta.2:1, ...") -- elements enumerated column-major,
# each element's slots read in full (recursing for nested tuples) before
# moving to the next element. This is simpler than (and distinct from) the
# input side's blocked-AoS order, where a single slot's values across every
# enclosing-array element are contiguous instead.
#
# Both directions share one "sized structure" tree (built by
# `.newstan_sized_structure()`), computed once per `constrain_variables()` /
# `variable_skeleton()` call from `model$variables()` (type structure --
# which base variables/slots are tuple/complex, and nested tuple shape) and
# `model_param_metadata()` (native dotted-name-keyed sizes: per-slot declared
# dims, including enclosing tuple-array sizes, a leaf's own container dims,
# and the trailing complex storage `2`).
#
# One tree node is built per base variable name (e.g. `t`, never `t.1`/
# `t.2`), and is one of:
#
#   * a *leaf* node: `list(kind = <"real"|"int"|"complex">, dims = <int>,
#     legacy = <lgl>)`. `dims` is the leaf's own container dims -- for a
#     top-level (non-tuple) variable this is its full declared shape
#     (unchanged from today, with the trailing complex `2` dropped for
#     `kind = "complex"`); for a tuple slot this is *just* the slot's own
#     container shape, with every enclosing tuple-array dimension already
#     stripped off (see `.newstan_sized_tuple_node()` below). `dims =
#     integer(0)` means scalar. `legacy` is TRUE only for a top-level
#     non-complex (real/int) variable -- see `.newstan_apply_leaf_dims()`
#     for what it changes.
#
#   * a *tuple* node: `list(kind = "tuple", array_dims = <int>, slots =
#     <list>)`. `array_dims` is this tuple's *own* enclosing-array sizes
#     (e.g. `c(2)` for `array[2] tuple(...)`, `integer(0)` for a plain
#     tuple); `slots` is one child node per tuple slot, in slot order,
#     recursing for nested-tuple slots.
#
# `.newstan_sized_structure()` itself returns a named list, one entry per
# base variable, `list(stage = <"parameter"|"transformed_parameter"|
# "generated_quantity">, node = <the node above>)`, in the same order
# `model_param_metadata()` lists variables.

# Drop the first `n` entries of `dims` (used to strip an already-accounted-
# for enclosing-array-dims prefix off a leaf's full declared dims). Returns
# `integer(0)` when `n >= length(dims)` (a bare scalar/no remaining dims),
# rather than the reversed sequence a bare `dims[(n+1):length(dims)]` would
# produce in that case.
.newstan_dims_after <- function(dims, n) {
  if (n >= length(dims)) integer(0) else dims[(n + 1L):length(dims)]
}

# The dotted metadata name of *some* leaf reachable from this tuple node,
# always descending via slot 1 (recursing through nested-tuple slots). Used
# only to read that leaf's full declared dims off `model_param_metadata()`
# in order to locate *this* level's own enclosing-array sizes by position --
# `model_param_metadata()`'s dims order is strictly hierarchical (enclosing
# array dims outermost first, then the leaf's own container dims, then
# complex's trailing `2`), so any leaf beneath the subtree carries the same
# prefix at the same position, regardless of which slot it descends through.
.newstan_first_leaf_metadata_name <- function(dotted_name, type_df) {
  slot_type <- type_df$type[[1]]
  slot_name <- paste0(dotted_name, ".1")
  if (is.data.frame(slot_type)) {
    .newstan_first_leaf_metadata_name(slot_name, slot_type)
  } else {
    slot_name
  }
}

# Build one tuple node (recursing for nested-tuple slots). `dotted_name`:
# this level's dotted metadata-name prefix (matches `model_param_metadata()`
# naming). `type_df`: this level's slot data.frame (columns `type`,
# `dimensions`). `own_array_count`: this level's own enclosing-array
# dimension *count* (a plain integer -- e.g. `1` for `array[2] tuple(...)`,
# `0` for a plain tuple); comes from the parent's declared dims (the
# variable's own `dimensions` at the top level, or a nested-tuple slot's
# `dimensions` one level up). `outer_array_dims`: the *concrete* sizes of
# every enclosing array dimension already accounted for by levels above this
# one (outermost first; `integer(0)` at the top). `metadata_index`: an
# environment mapping dotted metadata name -> `list(dims, stage)`.
.newstan_sized_tuple_node <- function(
  dotted_name,
  type_df,
  own_array_count,
  outer_array_dims,
  metadata_index
) {
  n_slots <- nrow(type_df)
  n_outer <- length(outer_array_dims)
  array_dims <- integer(0)
  if (own_array_count > 0L) {
    probe_name <- .newstan_first_leaf_metadata_name(dotted_name, type_df)
    probe_entry <- metadata_index[[probe_name]]
    if (is.null(probe_entry)) {
      stop(
        "newstan internal error: `model_param_metadata()` is missing an ",
        "entry for `",
        probe_name,
        "`.",
        call. = FALSE
      )
    }
    array_dims <- as.integer(
      probe_entry$dims[seq.int(n_outer + 1L, n_outer + own_array_count)]
    )
  }
  new_outer_array_dims <- c(outer_array_dims, array_dims)
  n_new_outer <- length(new_outer_array_dims)

  slots <- vector("list", n_slots)
  for (s in seq_len(n_slots)) {
    slot_name <- paste0(dotted_name, ".", s)
    slot_type <- type_df$type[[s]]
    slot_own_count <- type_df$dimensions[[s]]
    if (is.data.frame(slot_type)) {
      slots[[s]] <- .newstan_sized_tuple_node(
        slot_name,
        slot_type,
        slot_own_count,
        new_outer_array_dims,
        metadata_index
      )
    } else {
      entry <- metadata_index[[slot_name]]
      if (is.null(entry)) {
        stop(
          "newstan internal error: `model_param_metadata()` is missing an ",
          "entry for `",
          slot_name,
          "`.",
          call. = FALSE
        )
      }
      dims <- as.integer(entry$dims)
      prefix <- if (n_new_outer) dims[seq_len(n_new_outer)] else integer(0)
      if (!identical(prefix, as.integer(new_outer_array_dims))) {
        stop(
          "newstan internal error: `",
          slot_name,
          "`'s declared dimensions ",
          "(",
          paste(dims, collapse = ","),
          ") are inconsistent with the ",
          "enclosing tuple-array sizes (",
          paste(new_outer_array_dims, collapse = ","),
          ").",
          call. = FALSE
        )
      }
      is_complex <- identical(slot_type, "complex")
      own_dims <- .newstan_dims_after(dims, n_new_outer)
      if (is_complex) {
        own_dims <- own_dims[-length(own_dims)]
      }
      slots[[s]] <- list(
        kind = slot_type,
        dims = as.integer(own_dims),
        legacy = FALSE
      )
    }
  }
  list(kind = "tuple", array_dims = array_dims, slots = slots)
}

#' Merge `model$variables()`'s type structure with `model_param_metadata()`'s
#' sizes into one tree per base variable name
#'
#' See the file-level comment above for the tree shape. `ptr_metadata` is the
#' raw list returned by the native `model_param_metadata` call (`names`,
#' `dimensions`, `stages` -- one entry per dotted tuple-slot name or plain
#' variable name).
#'
#' @param model A `StanModel` (used for `$variables()`).
#' @param ptr_metadata The list returned by the native `model_param_metadata`
#'   call.
#'
#' @return A named list (one entry per base variable, in
#'   `model_param_metadata()`'s order), each `list(stage = <chr>, node =
#'   <sized node>)`.
#' @noRd
.newstan_sized_structure <- function(model, ptr_metadata) {
  metadata_names <- ptr_metadata$names
  metadata_index <- new.env(parent = emptyenv())
  for (i in seq_along(metadata_names)) {
    metadata_index[[metadata_names[[i]]]] <- list(
      dims = as.integer(ptr_metadata$dimensions[[i]]),
      stage = ptr_metadata$stages[[i]]
    )
  }

  # Base variable names never contain '.' (Stan identifiers can't), so the
  # base name is always the substring before the first dot -- true whether
  # `metadata_names[i]` is a plain variable name or a dotted tuple-slot path.
  base_names <- sub("\\..*$", "", metadata_names)
  unique_bases <- base_names[!duplicated(base_names)]

  declared <- model$variables()
  declared_all <- c(
    declared$parameters,
    declared$transformed_parameters,
    declared$generated_quantities
  )

  result <- vector("list", length(unique_bases))
  names(result) <- unique_bases
  for (name in unique_bases) {
    decl <- declared_all[[name]]
    if (is.null(decl)) {
      stop(
        "newstan internal error: `",
        name,
        "` from `model_param_metadata()` ",
        "is not declared in `model$variables()`.",
        call. = FALSE
      )
    }
    # A tuple's dotted slot entries carry the stage; a plain variable's own
    # (undotted) entry does. Either way it's the first (and only) metadata
    # row sharing this base name -- tuple slots of one variable always
    # belong to the same block (parameter / transformed parameter /
    # generated quantity), since a whole tuple is read/written together.
    stage <- ptr_metadata$stages[[which(base_names == name)[[1]]]]
    if (is.data.frame(decl$type)) {
      node <- .newstan_sized_tuple_node(
        name,
        decl$type,
        decl$dimensions,
        integer(0),
        metadata_index
      )
    } else {
      entry <- metadata_index[[name]]
      if (is.null(entry)) {
        stop(
          "newstan internal error: `model_param_metadata()` is missing an ",
          "entry for `",
          name,
          "`.",
          call. = FALSE
        )
      }
      dims <- entry$dims
      is_complex <- identical(decl$type, "complex")
      if (is_complex) {
        # `get_dims()` includes the trailing complex storage dim `2` --
        # never part of the canonical R-facing shape.
        dims <- dims[-length(dims)]
      }
      # `legacy = TRUE` only for a plain (non-complex) top-level variable:
      # its shape follows the historical convention, which always sets
      # `dim` once there is at least one declared dimension (even a single
      # one -- see `.newstan_apply_leaf_dims()`). Top-level complex
      # variables instead follow the canonical convention directly
      # ("complex_vector[n] -> complex vector length n", not a 1-d array),
      # matching every tuple-internal leaf.
      node <- list(kind = decl$type, dims = dims, legacy = !is_complex)
    }
    result[[name]] <- list(stage = stage, node = node)
  }
  result
}

# TRUE for a sized-structure entry (`list(stage=, node=)`) whose stage is
# included under the given `transformed_parameters`/`generated_quantities`
# flags -- shared by `variable_skeleton()` and `constrain_variables()` so
# both always agree on which variables are present.
.newstan_sized_stage_kept <- function(
  entry,
  transformed_parameters,
  generated_quantities
) {
  entry$stage == "parameter" ||
    (isTRUE(transformed_parameters) &&
      entry$stage == "transformed_parameter") ||
    (isTRUE(generated_quantities) && entry$stage == "generated_quantity")
}

# Recursively build nested unnamed lists of the given `sizes` (outermost
# first), calling `build_leaf()` at every position -- the canonical
# array-of-tuple shape ("list (over first index) of lists (over second
# index) of ... tuples"). `sizes = integer(0)` (no enclosing array) just
# returns one `build_leaf()` directly.
.newstan_nested_list_shape <- function(sizes, build_leaf) {
  if (!length(sizes)) {
    return(build_leaf())
  }
  n <- sizes[[1]]
  lapply(seq_len(n), function(i) {
    .newstan_nested_list_shape(sizes[-1], build_leaf)
  })
}

# Set a leaf's `dim` attribute according to its declared `dims`, under one of
# two coexisting conventions:
#
#   * "legacy" (`legacy = TRUE`; only ever a top-level, non-complex real/int
#     variable): `dim` is set as soon as `length(dims) >= 1` -- even a single
#     declared dimension gets a `dim` attribute. This matches the historical
#     behavior (`array(NA_real_, dim = dims)` regardless of `length(dims)`)
#     and must stay byte-identical for these variables.
#   * "canonical" (`legacy = FALSE`; every tuple-internal leaf, and top-level
#     complex variables): `dim` is set only when there are >= 2 dimensions. A
#     single dimension stays a bare vector and a scalar stays a bare
#     scalar/value -- both the output-side shape convention ("complex vector
#     length n", not a 1-d array) and the input-side flattening rule use the
#     same convention, so a value's shape round-trips unchanged through both
#     directions.
#
# `values` must already have length `prod(dims)` (or length 1 for a scalar,
# handled by the caller passing `dims = integer(0)`).
.newstan_apply_leaf_dims <- function(values, dims, legacy) {
  if (!length(dims)) {
    return(values[[1]])
  }
  if (legacy || length(dims) >= 2L) {
    dim(values) <- dims
  }
  values
}

# Build one sized-structure node's skeleton (no values yet, just shape/fill):
# complex -> `NA_complex_` (arrays without the trailing 2; a scalar leaf ->
# bare `NA_complex_`); tuple/array-of-tuple -> nested unnamed lists per the
# canonical shape; anything else (real/int) stays byte-identical to the
# historical behavior (`dim` kept as-is, scalar -> `NA_real_`).
.newstan_skeleton_node <- function(node) {
  if (identical(node$kind, "tuple")) {
    return(.newstan_nested_list_shape(
      node$array_dims,
      function() lapply(node$slots, .newstan_skeleton_node)
    ))
  }
  na_value <- if (identical(node$kind, "complex")) NA_complex_ else NA_real_
  n <- if (length(node$dims)) prod(node$dims) else 1L
  values <- rep(na_value, n)
  .newstan_apply_leaf_dims(values, node$dims, isTRUE(node$legacy))
}

# A mutable positional reader over a flat numeric vector -- `consume(n)`
# returns the next `n` values (default 1) and advances the cursor;
# `position()` reports how many values have been consumed so far (used to
# verify the whole flat vector was accounted for, see `.newstan_consume_node`
# call sites).
.newstan_flat_reader <- function(values) {
  pos <- 0L
  n_total <- length(values)
  list(
    consume = function(n = 1L) {
      if (pos + n > n_total) {
        stop(
          "newstan internal error: the constrained output has fewer values ",
          "than the variable structure expects.",
          call. = FALSE
        )
      }
      out <- values[seq.int(pos + 1L, pos + n)]
      pos <<- pos + n
      out
    },
    position = function() pos
  )
}

# Reshape a flat list of `prod(dims)` elements -- enumerated column-major,
# first index fastest, exactly the order `.newstan_consume_node()` reads
# tuple-array elements in -- into the canonical nested-list shape ("list over
# first index of lists over second index of ... of tuples"). This is the
# positional inverse of `.newstan_enumerate_tuple_elements()` above (that one
# flattens nested lists to column-major order for the *input*-side contract;
# this rebuilds nested lists from column-major order for the *output*-side
# shape) -- kept as a separate function because the two are read in
# different overall element orders (blocked-AoS on input vs element-major on
# output) even though the column-major enumeration *within* one array level
# is the same rule both directions.
.newstan_reshape_column_major <- function(elements, dims) {
  if (length(dims) <= 1L) {
    return(elements)
  }
  d1 <- dims[[1]]
  rest_dims <- dims[-1]
  rest_n <- prod(rest_dims)
  lapply(seq_len(d1), function(i) {
    sub <- lapply(seq_len(rest_n), function(r) elements[[i + (r - 1L) * d1]])
    .newstan_reshape_column_major(sub, rest_dims)
  })
}

# Consume one sized-structure node's worth of scalars from `reader` (a
# `.newstan_flat_reader()`), in the native flat constrained-draws order
# (distinct from the input side's blocked-AoS order): plain containers
# column-major; complex containers column-major with adjacent (real, imag)
# pairs; plain tuples slot-by-slot; tuple arrays *element-major* -- elements
# enumerated column-major, each element's slots consumed in full (recursing
# for nested tuples) before the next element. Returns the canonical R shape.
.newstan_consume_node <- function(node, reader) {
  if (identical(node$kind, "tuple")) {
    n_elements <- if (length(node$array_dims)) prod(node$array_dims) else 1L
    elements <- lapply(seq_len(n_elements), function(e) {
      lapply(node$slots, .newstan_consume_node, reader = reader)
    })
    if (!length(node$array_dims)) {
      return(elements[[1]])
    }
    return(.newstan_reshape_column_major(elements, node$array_dims))
  }
  if (identical(node$kind, "complex")) {
    n <- if (length(node$dims)) prod(node$dims) else 1L
    parts <- reader$consume(2L * n)
    values <- complex(
      real = parts[seq.int(1L, 2L * n, by = 2L)],
      imaginary = parts[seq.int(2L, 2L * n, by = 2L)]
    )
    return(.newstan_apply_leaf_dims(values, node$dims, isTRUE(node$legacy)))
  }
  # real/int: matches the historical behavior for top-level variables
  # (`legacy = TRUE` there) -- plain numeric, scalar unboxed, `dim` set
  # whenever the declared shape has >= 1 dimension.
  n <- if (length(node$dims)) prod(node$dims) else 1L
  values <- reader$consume(n)
  .newstan_apply_leaf_dims(values, node$dims, isTRUE(node$legacy))
}
