# Normalize the public initialization forms to the named list consumed by the
# native var_context adapter. A scalar is the CmdStan initialization radius.
normalize_init <- function(init) {
  if (isTRUE(length(init) == 1L && is.null(names(init)) &&
            is.numeric(init) && !is.na(init) && init >= 0)) {
    return(list())
  }
  if (is.list(init)) {
    return(init)
  }
  if (is.numeric(init)) {
    if (is.null(names(init)) || any(!nzchar(names(init)))) {
      stop("Numeric init must be a named vector of constrained parameters.",
           call. = FALSE)
    }
    return(as.list(init))
  }
  stop("init must be a non-negative radius, a named list, or a named numeric vector.",
       call. = FALSE)
}

init_radius <- function(init) {
  if (isTRUE(length(init) == 1L && is.null(names(init)) &&
            is.numeric(init) && !is.na(init) && init >= 0)) {
    return(as.double(init))
  }
  2
}

# Construct the concrete model in its generated shared library. The wrapper
# constructs the data context locally, so no R external pointer crosses into
# the model implementation.
new_model_instance <- function(stanmod, data, seed) {
  list(model = stanmod$new_model(data, seed))
}
