`%||%` <- function(x, y) if (is.null(x)) y else x

.stanr_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

.stanr_int <- function(x, name, min = 0L) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x != floor(x) ||
      x < min ||
      x > .Machine$integer.max
  ) {
    stop("`", name, "` must be a single integer >= ", min, ".", call. = FALSE)
  }
  as.integer(x)
}

# Parses `cpp_options` into ordered `list(name, op, value)` assignments;
# see `stan_model()`'s `@param cpp_options`. Input order preserved so a
# later assignment to the same name takes effect.
.stanr_parse_cpp_options <- function(cpp_options) {
  if (!is.list(cpp_options)) {
    stop("`cpp_options` must be a list.", call. = FALSE)
  }
  if (!length(cpp_options)) {
    return(list())
  }
  nms <- names(cpp_options)
  if (is.null(nms)) {
    nms <- rep("", length(cpp_options))
  }
  nms[is.na(nms)] <- ""
  lapply(seq_along(cpp_options), function(i) {
    nm <- nms[[i]]
    value <- cpp_options[[i]]
    if (nzchar(nm)) {
      if (
        !(is.character(value) || is.logical(value) || is.numeric(value)) ||
          length(value) != 1L ||
          is.na(value)
      ) {
        stop(
          "`cpp_options` values must each be a single non-missing string, ",
          "number, or logical.",
          call. = FALSE
        )
      }
      list(
        name = nm,
        op = "=",
        value = if (is.logical(value)) {
          toupper(as.character(value))
        } else {
          as.character(value)
        }
      )
    } else {
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        stop(
          "Unnamed `cpp_options` entries must each be a single string of ",
          "the form '<NAME> = <value>' or '<NAME> += <value>'.",
          call. = FALSE
        )
      }
      m <- regmatches(
        value,
        regexec("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(\\+=|=)\\s*(.*)$", value)
      )[[1]]
      if (!length(m)) {
        stop(
          "Unnamed `cpp_options` entries must each be a single string of ",
          "the form '<NAME> = <value>' or '<NAME> += <value>', got: \"",
          value,
          "\".",
          call. = FALSE
        )
      }
      list(name = m[[2]], op = m[[3]], value = trimws(m[[4]]))
    }
  })
}

# Normalizes service flags, engages OpenCL if requested. Read via
# `missing()` since NULL is a valid `num_threads` override.
.stanr_common_service_flags <- function(
  show_messages,
  show_exceptions,
  opencl_ids,
  private,
  num_threads,
  refresh
) {
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  out <- list(show_messages = show_messages, show_exceptions = show_exceptions)
  if (!missing(num_threads)) {
    out$num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  }
  if (!missing(refresh)) {
    out$refresh <- .stanr_int(refresh, "refresh")
  }
  out
}

.stanr_seed <- function(seed) {
  if (is.null(seed)) {
    seed <- as.integer(stats::runif(1, 1, .Machine$integer.max))
  }
  if (
    !is.numeric(seed) ||
      length(seed) != 1L ||
      is.na(seed) ||
      seed < 0 ||
      seed > .Machine$integer.max ||
      seed != floor(seed)
  ) {
    stop(
      "`seed` must be NULL or a single integer between 0 and 2^31 - 1.",
      call. = FALSE
    )
  }
  as.integer(seed)
}


# Shared execution path for all StanModel service methods.
.stanr_run_service <- function(
  self,
  data,
  seed,
  init = NULL, # resolves to default init; unused by laplace/generate_quantities
  native_args_fn, # function(seed, resolved_init, model) -> list
  payload_fn # function(result) -> list of method-specific fields
) {
  started <- proc.time()[["elapsed"]]
  seed <- .stanr_seed(seed)
  resolved_init <- resolve_init(init)
  # Tuple-typed values are flattened in r_data_context, which needs the
  # declared structure; only pay the stanc-info cost for list-valued input.
  has_list <- function(x) any(vapply(x, is.list, logical(1)))
  data_declarations <- init_declarations <- NULL
  if (has_list(data) || has_list(resolved_init$values)) {
    declared <- self$variables()
    data_declarations <- declared$data
    init_declarations <- declared$parameters
  }
  model <- self$new_model(data, seed, data_declarations)
  native_args <- native_args_fn(seed, resolved_init, model)
  # `[<- list(...)` keeps NULL present; `$<- NULL` would drop it.
  native_args["init_declarations"] <- list(init_declarations)
  result <- self$run_model(model, native_args)
  payload <- c(
    payload_fn(result),
    list(
      return_code = result$return_code,
      args = service_args(native_args),
      output = result$output %||% character(),
      model_ptr = model
    )
  )
  list(
    payload = payload,
    seed = seed,
    elapsed = proc.time()[["elapsed"]] - started
  )
}

# Bundled Stan library version, memoized (headers can't change mid-session).
.stanr_stan_version <- function() {
  cached <- .stanr_memo$stan_version
  if (!is.null(cached)) {
    return(cached)
  }
  header <- system.file("include", "stan", "version.hpp", package = "stanr")
  value <- if (!nzchar(header) || !file.exists(header)) {
    NA_character_
  } else {
    lines <- readLines(header, warn = FALSE)
    macro_value <- function(macro) {
      line <- grep(
        paste0("^#define[[:space:]]+", macro, "[[:space:]]+"),
        lines,
        value = TRUE
      )
      if (!length(line)) {
        return(NA_character_)
      }
      sub(paste0("^#define[[:space:]]+", macro, "[[:space:]]+"), "", line[[1]])
    }
    paste(
      macro_value("STAN_MAJOR"),
      macro_value("STAN_MINOR"),
      macro_value("STAN_PATCH"),
      sep = "."
    )
  }
  .stanr_memo$stan_version <- value
  value
}

.stanr_as_draws_format <- function(x, format) {
  switch(
    format,
    draws_array = posterior::as_draws_array(x),
    draws_matrix = posterior::as_draws_matrix(x),
    draws_df = posterior::as_draws_df(x),
    draws_list = posterior::as_draws_list(x),
    rvars = posterior::as_draws_rvars(x),
    stop("Unknown draws format `", format, "`.", call. = FALSE)
  )
}

.stanr_bracket_names <- function(names) {
  has_dot <- grepl(".", names, fixed = TRUE)
  if (!any(has_dot)) {
    return(names)
  }
  dotted <- names[has_dot]
  base <- sub("\\..*$", "", dotted)
  indices <- gsub(".", ",", sub("^[^.]*\\.", "", dotted), fixed = TRUE)
  names[has_dot] <- paste0(base, "[", indices, "]")
  names
}

.stanr_normalize_draw_names <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "draws_array")) {
    names <- dimnames(x)[[3]]
    dimnames(x)[[3]] <- .stanr_bracket_names(names)
  } else {
    names <- colnames(x)
    model_columns <- !startsWith(names, ".")
    names[model_columns] <- .stanr_bracket_names(names[model_columns])
    colnames(x) <- names
  }
  x
}

.stanr_xptr_is_null <- function(ptr) {
  # XPtr addresses don't survive serialize()/readRDS(); the restored one is
  # null, so native calls fail loudly rather than silently.
  is.null(ptr) || .Call(stanr_xptr_is_null, ptr)
}

.stanr_rename_draw_columns <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- .stanr_normalize_draw_names(x)
  names <- if (inherits(x, "draws_array")) dimnames(x)[[3]] else colnames(x)
  names[names == "log_p__"] <- "lp__"
  names[names == "log_q__" | names == "log_g__"] <- "lp_approx__"
  if (inherits(x, "draws_array")) {
    dimnames(x)[[3]] <- names
  } else {
    colnames(x) <- names
  }
  x
}

# Validate and resolve chain count/IDs for sampling
.stanr_validate_chains <- function(chains, chain_ids) {
  chains <- .stanr_int(chains, "chains", min = 1L)
  if (
    !is.numeric(chain_ids) ||
      length(chain_ids) != chains ||
      anyNA(chain_ids) ||
      any(chain_ids != floor(chain_ids))
  ) {
    stop(
      "`chain_ids` must be ",
      chains,
      " integer value(s).",
      call. = FALSE
    )
  }
  chain_ids <- as.integer(chain_ids)
  if (anyDuplicated(chain_ids) || any(diff(chain_ids) != 1L)) {
    stop(
      "The current backend requires `chain_ids` to be unique consecutive integers.",
      call. = FALSE
    )
  }

  list(chains = chains, chain_ids = chain_ids)
}

# Wraps a single metric in a list (recycled across chains) or validates
# a per-chain list.
.stanr_normalize_inv_metric <- function(
  inv_metric,
  metric,
  chains
) {
  if (is.null(inv_metric)) {
    return(NULL)
  }

  if (metric == "unit_e") {
    warning("inv_metric is ignored when metric = 'unit_e'", call. = FALSE)
    return(NULL)
  }

  if (!is.list(inv_metric)) {
    inv_metric <- list(inv_metric)
  }

  if (length(inv_metric) > 1L && length(inv_metric) != chains) {
    stop(
      "inv_metric must be a single metric or a list of length ",
      chains,
      " (one per chain).",
      call. = FALSE
    )
  }

  inv_metric
}

# Normalizes init to the radius/values pair the native adapter consumes.
# A scalar is the CmdStan radius; a list/named vector supplies constrained
# values (default radius 2, matching CmdStan).
resolve_init <- function(init) {
  if (is.null(init)) {
    return(list(radius = 2, values = list()))
  }
  is_radius <- isTRUE(
    length(init) == 1L &&
      is.null(names(init)) &&
      is.numeric(init) &&
      !is.na(init) &&
      is.finite(init) &&
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

# Keep service results small: drop large inputs (data, init, draws, metrics).
service_args <- function(args) {
  args[setdiff(
    names(args),
    c(
      "data",
      "init",
      "init_declarations",
      "draws",
      "inv_metric",
      "diagnostic_names"
    )
  )]
}
