# Normalize the public initialization forms to the radius/values pair consumed
# by the native var_context adapter, where `NULL` selects the default radius.
# A scalar is the CmdStan initialization radius; a list or named numeric
# vector supplies constrained parameter values (with a default radius of 2,
# matching CmdStan's default `init` behavior for perturbing any parameters
# not covered by the supplied values).
resolve_init <- function(init) {
  if (is.null(init)) {
    return(list(radius = 2, values = list()))
  }
  is_radius <- isTRUE(
    length(init) == 1L &&
      is.null(names(init)) &&
      is.numeric(init) &&
      !is.na(init) &&
      init >= 0
  )
  if (is_radius) {
    return(list(radius = as.double(init), values = list()))
  }
  if (is.numeric(init) && length(init) == 1L && is.null(names(init))) {
    stop(
      "`init` as a radius must be a single non-negative number.",
      call. = FALSE
    )
  }
  if (is.list(init)) {
    if (length(init) && (is.null(names(init)) || any(!nzchar(names(init))))) {
      stop(
        "`init` must be a named list of constrained parameter values; ",
        "per-chain init lists are not supported (supplied values are ",
        "shared by all chains).",
        call. = FALSE
      )
    }
    return(list(radius = 2, values = init))
  }
  if (is.numeric(init) || is.complex(init)) {
    if (is.null(names(init)) || any(!nzchar(names(init)))) {
      stop(
        "Numeric init must be a named vector of constrained parameters.",
        call. = FALSE
      )
    }
    return(list(radius = 2, values = as.list(init)))
  }
  stop(
    "init must be a non-negative radius, a named list, or a named numeric vector.",
    call. = FALSE
  )
}

# Keep service results small: data, initialization values, draws, and metrics can
# be large and are inputs rather than service configuration.
service_args <- function(args) {
  args[setdiff(
    names(args),
    c("data", "init", "draws", "inv_metric", "diagnostic_names")
  )]
}
