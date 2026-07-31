.newstan_as_draws_format <- function(x, format) {
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

.newstan_select_draws <- function(x, variables = NULL) {
  if (is.null(variables)) return(x)
  posterior::subset_draws(x, variable = variables)
}

.newstan_bracket_names <- function(names) {
  vapply(names, function(name) {
    pieces <- strsplit(name, ".", fixed = TRUE)[[1]]
    if (length(pieces) == 1L) return(name)
    paste0(pieces[[1]], "[", paste(pieces[-1L], collapse = ","), "]")
  }, character(1), USE.NAMES = FALSE)
}

.newstan_normalize_draw_names <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "draws_array")) {
    names <- dimnames(x)[[3]]
    dimnames(x)[[3]] <- .newstan_bracket_names(names)
  } else {
    names <- colnames(x)
    model_columns <- !startsWith(names, ".")
    names[model_columns] <- .newstan_bracket_names(names[model_columns])
    colnames(x) <- names
  }
  x
}

.newstan_rename_draw_columns <- function(x) {
  if (is.null(x)) return(NULL)
  x <- .newstan_normalize_draw_names(x)
  names <- if (inherits(x, "draws_array")) dimnames(x)[[3]] else colnames(x)
  names[names == "log_p__"] <- "lp__"
  names[names == "log_q__" | names == "log_g__"] <- "lp_approx__"
  if (inherits(x, "draws_array")) dimnames(x)[[3]] <- names else colnames(x) <- names
  x
}

.newstan_merge_payload_draws <- function(payload) {
  if (is.null(payload$draws) || is.null(payload$diagnostics)) return(payload)
  lhs <- posterior::as_draws_matrix(payload$draws)
  rhs <- posterior::as_draws_matrix(payload$diagnostics)
  if (nrow(lhs) == nrow(rhs)) {
    rhs <- rhs[, setdiff(colnames(rhs), colnames(lhs)), drop = FALSE]
    payload$draws <- cbind(lhs, rhs)
  }
  payload
}

#' Base class for in-process Stan fits
#'
#' @noRd
StanFit <- R6Class(
  "StanFit",
  public = list(
    initialize = function(payload = list(), model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list(), default_format = "draws_matrix") {
      private$payload_ <- payload
      private$model_ <- model
      private$data_ <- data
      private$seed_ <- seed
      private$init_ <- init
      private$elapsed_ <- elapsed
      private$default_format_ <- default_format
      private$draws_ <- if (is.list(payload))
        .newstan_normalize_draw_names(payload$draws) else NULL
      private$diagnostics_ <- if (is.list(payload))
        .newstan_normalize_draw_names(payload$diagnostics) else NULL
      return_code <- if (is.list(payload)) payload$return_code else NA_integer_
      chains <- metadata$chains %||% 1L
      private$return_codes_ <- rep(as.integer(return_code %||% NA_integer_),
                                   length.out = chains)
      private$metadata_ <- utils::modifyList(
        list(
          seed = seed,
          data = data,
          arguments = if (is.list(payload)) payload$args %||% list() else list(),
          model_name = if (inherits(model, "StanModel")) model$model_name() else NULL
        ),
        metadata
      )
      private$output_ <- if (is.list(payload)) payload$output %||% character() else character()
      if (inherits(model, "StanModel")) {
        private$initialize_pointer()
      }
      invisible(self)
    },

    draws = function(variables = NULL, inc_warmup = FALSE, format = NULL) {
      if (is.null(private$draws_)) {
        stop("This fit does not contain draws.", call. = FALSE)
      }
      draws <- private$draws_
      if (isTRUE(inc_warmup) && !is.null(private$warmup_draws_)) {
        draws <- posterior::bind_draws(private$warmup_draws_, draws,
                                       along = "iteration")
      }
      draws <- .newstan_select_draws(draws, variables)
      .newstan_as_draws_format(draws, format %||% private$default_format_)
    },

    summary = function(variables = NULL, ...) {
      posterior::summarise_draws(
        self$draws(variables = variables, format = private$default_format_), ...
      )
    },

    print = function(variables = NULL, ..., digits = 2, max_rows = 20) {
      out <- self$summary(variables = variables, ...)
      print(utils::head(out, max_rows), digits = digits)
      invisible(self)
    },

    return_codes = function() private$return_codes_,
    metadata = function() private$metadata_,
    output = function(id = NULL) {
      if (is.null(id)) return(private$output_)
      if (is.list(private$output_)) return(private$output_[[id]])
      private$output_
    },
    time = function() list(total = private$elapsed_),
    init = function() private$init_,
    code = function() {
      if (!inherits(private$model_, "StanModel")) character() else private$model_$code()
    },
    materialize = function(...) invisible(self),
    save_object = function(file, ...) {
      saveRDS(self, file = file, ...)
      invisible(normalizePath(file, mustWork = TRUE))
    },
    lp = function() self$draws(variables = "lp__"),
    lp_approx = function() self$draws(variables = "lp_approx__"),

    init_model_methods = function(seed = 1, verbose = FALSE) {
      private$seed_ <- .newstan_seed(seed)
      private$initialize_pointer(force = TRUE)
      invisible(self)
    },

    log_prob = function(unconstrained_variables, jacobian = TRUE) {
      private$check_jacobian(jacobian)
      as.numeric(private$native_call(
        "model_log_prob", as.double(unconstrained_variables), jacobian
      ))
    },
    grad_log_prob = function(unconstrained_variables, jacobian = TRUE) {
      private$check_jacobian(jacobian)
      private$native_call("model_grad_log_prob",
                          as.double(unconstrained_variables), jacobian)
    },
    hessian = function(unconstrained_variables, jacobian = TRUE) {
      private$check_jacobian(jacobian)
      private$native_call("model_hessian", as.double(unconstrained_variables),
                          jacobian)
    },
    unconstrain_variables = function(variables) {
      if (!is.list(variables) || is.null(names(variables))) {
        stop("`variables` must be a named list of constrained values.",
             call. = FALSE)
      }
      tryCatch(
        private$native_call("model_unconstrain", variables),
        error = function(error) {
          stop(
            "Could not unconstrain variables; check parameter names, ",
            "dimensions, and bounds. ", conditionMessage(error),
            call. = FALSE
          )
        }
      )
    },
    unconstrain_draws = function(
      draws = NULL,
      format = getOption("newstan_draws_format", "draws_array"),
      inc_warmup = FALSE
    ) {
      source <- draws %||% self$draws(
        inc_warmup = inc_warmup, format = "draws_df"
      )
      source <- posterior::as_draws_df(source)
      private$ensure_native()
      native_names <- private$model_$native_function(
        "model_constrained_names"
      )(private$model_ptr_, FALSE, FALSE)
      draw_names <- .newstan_bracket_names(native_names)
      missing <- setdiff(draw_names, posterior::variables(source))
      if (length(missing)) {
        stop("The draws are missing model parameters: ",
             paste(missing, collapse = ", "), ".", call. = FALSE)
      }
      values <- as.matrix(as.data.frame(source, check.names = FALSE)[draw_names])
      result <- private$model_$native_function("model_unconstrain_matrix")(
        private$model_ptr_, values
      )
      colnames(result) <- .newstan_bracket_names(colnames(result))
      result <- posterior::as_draws_df(data.frame(
        as.data.frame(result, check.names = FALSE),
        .chain = source$.chain,
        .iteration = source$.iteration,
        .draw = source$.draw,
        check.names = FALSE
      ))
      .newstan_as_draws_format(result, format)
    },
    constrain_variables = function(unconstrained_variables,
                                   transformed_parameters = TRUE,
                                   generated_quantities = TRUE) {
      private$ensure_native()
      flat <- private$model_$native_function("model_constrain")(
        private$model_ptr_, private$rng_ptr_,
        as.double(unconstrained_variables),
        as.logical(transformed_parameters), as.logical(generated_quantities)
      )
      skeleton <- self$variable_skeleton(transformed_parameters,
                                         generated_quantities)
      private$relist_constrained(flat, skeleton)
    },
    variable_skeleton = function(transformed_parameters = TRUE,
                                 generated_quantities = TRUE) {
      metadata <- private$native_call("model_param_metadata")
      private$metadata_skeleton(
        metadata,
        transformed_parameters = transformed_parameters,
        generated_quantities = generated_quantities
      )
    }
  ),
  private = list(
    payload_ = NULL,
    model_ = NULL,
    data_ = NULL,
    seed_ = NULL,
    init_ = NULL,
    elapsed_ = NULL,
    default_format_ = NULL,
    draws_ = NULL,
    warmup_draws_ = NULL,
    diagnostics_ = NULL,
    warmup_diagnostics_ = NULL,
    return_codes_ = NULL,
    metadata_ = NULL,
    output_ = NULL,
    model_ptr_ = NULL,
    rng_ptr_ = NULL,
    initialize_pointer = function(force = FALSE) {
      if (!inherits(private$model_, "StanModel")) return(invisible(NULL))
      if (!force && !is.null(private$model_ptr_)) return(invisible(NULL))
      private$model_ptr_ <- private$model_$new_model(private$data_, private$seed_)
      rng <- private$model_$native_function("new_base_rng", required = FALSE)
      private$rng_ptr_ <- if (is.null(rng)) NULL else rng(private$seed_)
      invisible(NULL)
    },
    ensure_native = function() {
      if (!inherits(private$model_, "StanModel")) {
        stop("This fit does not retain a model binding.", call. = FALSE)
      }
      private$initialize_pointer()
      probe <- private$model_$native_function("model_num_upars", required = FALSE)
      if (!is.null(probe)) {
        valid <- tryCatch({ probe(private$model_ptr_); TRUE }, error = function(e) FALSE)
        if (!valid) {
          private$model_$compile(force_recompile = TRUE, quiet = TRUE)
          private$initialize_pointer(force = TRUE)
        }
      }
      invisible(NULL)
    },
    native_call = function(name, ...) {
      private$ensure_native()
      private$model_$native_function(name)(private$model_ptr_, ...)
    },
    check_jacobian = function(jacobian) {
      if (!is.logical(jacobian) || length(jacobian) != 1L || is.na(jacobian)) {
        stop("`jacobian` must be TRUE or FALSE.", call. = FALSE)
      }
      invisible(NULL)
    },
    relist_constrained = function(flat, skeleton) {
      flat_names <- .newstan_bracket_names(names(flat))
      result <- lapply(names(skeleton), function(name) {
        indices <- flat_names == name | startsWith(flat_names, paste0(name, "["))
        values <- unname(flat[indices])
        template <- skeleton[[name]]
        if (!is.null(dim(template))) {
          array(values, dim = dim(template))
        } else if (length(template) == 1L) {
          values[[1]]
        } else {
          values
        }
      })
      names(result) <- names(skeleton)
      result
    },
    metadata_skeleton = function(metadata, transformed_parameters,
                                 generated_quantities) {
      if (is.list(metadata) &&
          all(c("names", "dimensions", "stages") %in% names(metadata))) {
        keep <- metadata$stages == "parameter" |
          (isTRUE(transformed_parameters) &
             metadata$stages == "transformed_parameter") |
          (isTRUE(generated_quantities) &
             metadata$stages == "generated_quantity")
        result <- lapply(which(keep), function(i) {
          dims <- metadata$dimensions[[i]]
          if (!length(dims)) NA_real_ else array(NA_real_, dim = dims)
        })
        names(result) <- metadata$names[keep]
        return(result)
      }
      if (is.data.frame(metadata)) {
        name_col <- intersect(c("name", "variable"), names(metadata))[[1]]
        dim_col <- intersect(c("dimensions", "dims"), names(metadata))
        stages <- if ("stage" %in% names(metadata)) metadata$stage else
          rep("parameter", nrow(metadata))
        keep <- stages == "parameter" |
          (isTRUE(transformed_parameters) & stages == "transformed_parameter") |
          (isTRUE(generated_quantities) & stages == "generated_quantity")
        result <- lapply(which(keep), function(i) {
          dims <- if (length(dim_col)) metadata[[dim_col[[1]]]][[i]] else integer()
          if (!length(dims)) NA_real_ else array(NA_real_, dim = dims)
        })
        names(result) <- metadata[[name_col]][keep]
        return(result)
      }
      names <- private$model_$native_function("model_constrained_names")(
        private$model_ptr_, transformed_parameters, generated_quantities
      )
      names <- unique(sub("\\[.*$", "", names))
      stats::setNames(rep(list(NA_real_), length(names)), names)
    }
  ),
  cloneable = FALSE
)

#' MCMC fit
#' @noRd
StanMCMC <- R6Class(
  "StanMCMC",
  inherit = StanFit,
  public = list(
    initialize = function(payload = list(), model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list()) {
      warmup <- NULL
      warmup_diagnostics <- NULL
      if (isTRUE(payload$args$save_warmup) && !is.null(payload$draws)) {
        all_draws <- posterior::as_draws_array(payload$draws)
        warmup_iterations <- ceiling(
          (payload$args$num_warmup %||% 0L) / (payload$args$thin %||% 1L)
        )
        if (warmup_iterations > 0L &&
            posterior::niterations(all_draws) >= warmup_iterations) {
          warmup <- all_draws[seq_len(warmup_iterations), , , drop = FALSE]
          sampling_iterations <- if (warmup_iterations < posterior::niterations(all_draws)) {
            seq.int(warmup_iterations + 1L, posterior::niterations(all_draws))
          } else {
            integer()
          }
          payload$draws <- all_draws[sampling_iterations, , , drop = FALSE]
          if (!is.null(payload$diagnostics)) {
            all_diagnostics <- posterior::as_draws_array(payload$diagnostics)
            warmup_diagnostics <- all_diagnostics[
              seq_len(warmup_iterations), , , drop = FALSE
            ]
            payload$diagnostics <- all_diagnostics[
              sampling_iterations, , , drop = FALSE
            ]
          }
        }
      }
      super$initialize(payload, model, data, seed, init, elapsed, metadata,
                       default_format = "draws_array")
      private$warmup_draws_ <- .newstan_normalize_draw_names(warmup)
      private$warmup_diagnostics_ <-
        .newstan_normalize_draw_names(warmup_diagnostics)
    },
    sampler_diagnostics = function(inc_warmup = FALSE,
                                   format = "draws_array") {
      diagnostics <- private$diagnostics_
      if (is.null(diagnostics)) {
        stop("This fit does not contain sampler diagnostics.", call. = FALSE)
      }
      if (isTRUE(inc_warmup) && !is.null(private$warmup_diagnostics_)) {
        diagnostics <- posterior::bind_draws(
          private$warmup_diagnostics_, diagnostics, along = "iteration"
        )
      }
      .newstan_as_draws_format(diagnostics, format)
    },
    num_chains = function() private$metadata_$chains %||%
      posterior::nchains(private$draws_),
    diagnostic_summary = function() {
      diagnostics <- self$sampler_diagnostics(format = "draws_matrix")
      data.frame(
        num_divergent = if ("divergent__" %in% colnames(diagnostics))
          sum(diagnostics[, "divergent__"] > 0) else NA_integer_,
        num_max_treedepth = if ("treedepth__" %in% colnames(diagnostics))
          sum(diagnostics[, "treedepth__"] >=
                (private$metadata_$arguments$max_depth %||% 10L)) else NA_integer_
      )
    },
    inv_metric = function(matrix = TRUE) {
      metric <- private$payload_$inv_metric
      if (is.null(metric)) {
        stop("The native service did not retain an inverse metric.", call. = FALSE)
      }
      metric
    }
  ),
  cloneable = FALSE
)

#' Maximum-likelihood fit
#' @noRd
StanMLE <- R6Class(
  "StanMLE",
  inherit = StanFit,
  public = list(
    initialize = function(payload = list(), model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list()) {
      par <- payload$par %||% numeric()
      if (!is.null(names(par))) {
        par <- par[names(par) != "lp__"]
        names(par) <- .newstan_bracket_names(names(par))
      }
      payload$par <- par
      if (length(par)) {
        payload$draws <- matrix(
          c(payload$value %||% NA_real_, unname(par)), nrow = 1L,
          dimnames = list(NULL, c("lp__", names(par)))
        )
      }
      super$initialize(payload, model, data, seed, init, elapsed, metadata,
                       default_format = "draws_matrix")
    },
    mle = function(variables = NULL) {
      value <- private$payload_$par %||% numeric()
      if (!is.null(variables)) value <- value[variables]
      value
    },
    summary = function(variables = NULL, ...) {
      value <- self$mle(variables)
      data.frame(variable = names(value), estimate = unname(value),
                 row.names = NULL)
    }
  ),
  cloneable = FALSE
)

#' Laplace approximation fit
#' @noRd
StanLaplace <- R6Class(
  "StanLaplace",
  inherit = StanFit,
  public = list(
    initialize = function(payload = list(), model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list(), mode = NULL) {
      private$mode_ <- mode
      payload$draws <- .newstan_rename_draw_columns(payload$draws)
      super$initialize(payload, model, data, seed, init, elapsed, metadata,
                       default_format = "draws_matrix")
    },
    mode = function() private$mode_
  ),
  private = list(mode_ = NULL),
  cloneable = FALSE
)

#' Variational inference fit
#' @noRd
StanVB <- R6Class(
  "StanVB",
  inherit = StanFit,
  public = list(
    initialize = function(payload = list(), model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list()) {
      # Native ADVI output arrives as a posterior object, but row/column
      # normalization intentionally changes its metadata. Work on the
      # underlying matrix to avoid posterior warning that metadata was
      # discarded, then let the base fit constructor rebuild the draw format.
      draws <- if (is.null(payload$draws)) NULL else
        posterior::as_draws_matrix(payload$draws)
      draws <- .newstan_rename_draw_columns(draws)
      requested <- payload$args$output_samples %||% NULL
      if (!is.null(draws) && !is.null(requested) && nrow(draws) == requested + 1L) {
        draws <- draws[-1L, , drop = FALSE]
      }
      if (!is.null(draws) && "lp__" %in% colnames(draws) &&
          all(is.na(draws[, "lp__"]) | draws[, "lp__"] == 0)) {
        draws <- draws[, setdiff(colnames(draws), "lp__"), drop = FALSE]
      }
      payload$draws <- draws
      super$initialize(payload, model, data, seed, init, elapsed, metadata,
                       default_format = "draws_matrix")
    }
  ),
  cloneable = FALSE
)

#' Pathfinder approximation fit
#' @noRd
StanPathfinder <- R6Class(
  "StanPathfinder",
  inherit = StanFit,
  public = list(
    initialize = function(payload = list(), model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list()) {
      payload <- .newstan_merge_payload_draws(payload)
      payload$draws <- .newstan_rename_draw_columns(payload$draws)
      super$initialize(payload, model, data, seed, init, elapsed, metadata,
                       default_format = "draws_matrix")
    }
  ),
  cloneable = FALSE
)

#' Generated quantities fit
#' @noRd
StanGQ <- R6Class(
  "StanGQ",
  inherit = StanFit,
  public = list(
    initialize = function(payload = list(), model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list()) {
      super$initialize(payload, model, data, seed, init, elapsed, metadata,
                       default_format = "draws_array")
    },
    num_chains = function() {
      if (is.null(private$draws_)) return(0L)
      posterior::nchains(private$draws_)
    }
  ),
  cloneable = FALSE
)

#' Gradient diagnostic fit
#' @noRd
StanDiagnose <- R6Class(
  "StanDiagnose",
  inherit = StanFit,
  public = list(
    initialize = function(payload = NA_integer_, model = NULL, data = list(),
                          seed = 1L, init = NULL, elapsed = NA_real_,
                          metadata = list()) {
      metadata$num_failed <- as.integer(payload)
      super$initialize(
        list(return_code = 0L, gradients = attr(payload, "gradients")),
        model, data, seed, init, elapsed, metadata,
        default_format = "draws_matrix"
      )
    },
    gradients = function() private$payload_$gradients %||% data.frame(),
    num_failed = function() private$metadata_$num_failed
  ),
  cloneable = FALSE
)
