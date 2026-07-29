# Normalize the public initialization forms to the named list consumed by the
# native var_context adapter.  The historical scalar 0 means random init.
normalize_init <- function(init) {
  if (isTRUE(length(init) == 1L && is.null(names(init)) &&
            is.numeric(init) && !is.na(init) && init == 0)) {
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
  stop("init must be a named list, a named numeric vector, or 0 for random initialization.",
       call. = FALSE)
}
