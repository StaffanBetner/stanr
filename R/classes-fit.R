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
  if (is.null(variables)) {
    return(x)
  }
  posterior::subset_draws(x, variable = variables)
}

.newstan_bracket_names <- function(names) {
  vapply(
    names,
    function(name) {
      pieces <- strsplit(name, ".", fixed = TRUE)[[1]]
      if (length(pieces) == 1L) {
        return(name)
      }
      paste0(pieces[[1]], "[", paste(pieces[-1L], collapse = ","), "]")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.newstan_normalize_draw_names <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
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
  if (is.null(x)) {
    return(NULL)
  }
  x <- .newstan_normalize_draw_names(x)
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

.newstan_merge_payload_draws <- function(payload) {
  if (is.null(payload$draws) || is.null(payload$diagnostics)) {
    return(payload)
  }
  lhs <- posterior::as_draws_matrix(payload$draws)
  rhs <- posterior::as_draws_matrix(payload$diagnostics)
  if (nrow(lhs) == nrow(rhs)) {
    rhs <- rhs[, setdiff(colnames(rhs), colnames(lhs)), drop = FALSE]
    payload$draws <- cbind(lhs, rhs)
  }
  payload
}

# StanFit class definition -----------------------------------------------------

#' StanFit objects
#'
#' @name StanFit
#' @description `StanFit` is the base class for fitted model objects returned by
#'   the inference methods of [`StanModel`]. Subclasses include [`StanMCMC`]
#'   (MCMC sampling), [`StanMLE`] (optimization), [`StanLaplace`] (Laplace
#'   approximation), [`StanVB`] (variational inference), [`StanPathfinder`]
#'   (Pathfinder), [`StanGQ`] (generated quantities), and [`StanDiagnose`]
#'   (gradient diagnostics).
#'
#' @section Methods: All `StanFit` objects share the following methods:
#'
#'  ## Draws and summaries
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$draws()`][fit-method-draws] | Extract posterior draws or point estimates. |
#'  [`$summary()`][fit-method-summary] | Summarize posterior draws or point estimates. |
#'  [`$print()`][fit-method-print] | Print a summary of the fit. |
#'
#'  ## Fit information
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$return_codes()`][fit-method-fit-info] | Return the Stan return codes. |
#'  [`$metadata()`][fit-method-fit-info] | Return fit metadata. |
#'  [`$output()`][fit-method-fit-info] | Return Stan output messages. |
#'  [`$time()`][fit-method-fit-info] | Return timing information. |
#'  [`$init()`][fit-method-fit-info] | Return user-specified initial values. |
#'  [`$code()`][fit-method-fit-info] | Return the Stan program code. |
#'
#'  ## Log density
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$lp()`][fit-method-lp] | Extract the log density (`lp__`) draws. |
#'  [`$lp_approx()`][fit-method-lp] | Extract the log density approximation draws. |
#'
#'  ## Model methods
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$init_model_methods()`][fit-method-init_model_methods] | Initialize model methods for computing log probability and transformations. |
#'  [`$log_prob()`][fit-method-model-methods] | Compute the log probability. |
#'  [`$grad_log_prob()`][fit-method-model-methods] | Compute the gradient of the log probability. |
#'  [`$hessian()`][fit-method-model-methods] | Compute the Hessian of the log probability. |
#'  [`$constrain_variables()`][fit-method-model-methods] | Constrain unconstrained parameters. |
#'  [`$unconstrain_variables()`][fit-method-model-methods] | Unconstrain parameters. |
#'  [`$unconstrain_draws()`][fit-method-model-methods] | Unconstrain posterior draws. |
#'  [`$variable_skeleton()`][fit-method-model-methods] | Return a skeleton of the variable structure. |
#'
#'  ## Saving
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$save_object()`][fit-method-save] | Save the fitted model object to a file. |
#'
NULL

StanFit <- R6Class(
  "StanFit",
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list(),
      default_format = "draws_matrix"
    ) {
      private$payload_ <- payload
      private$model_ <- model
      private$data_ <- data
      private$seed_ <- seed
      private$init_ <- init
      private$elapsed_ <- elapsed
      private$default_format_ <- default_format
      private$draws_ <- if (is.list(payload)) {
        .newstan_normalize_draw_names(payload$draws)
      } else {
        NULL
      }
      private$diagnostics_ <- if (is.list(payload)) {
        .newstan_normalize_draw_names(payload$diagnostics)
      } else {
        NULL
      }
      return_code <- if (is.list(payload)) payload$return_code else NA_integer_
      chains <- metadata$chains %||% 1L
      private$return_codes_ <- rep(
        as.integer(return_code %||% NA_integer_),
        length.out = chains
      )
      private$metadata_ <- utils::modifyList(
        list(
          seed = seed,
          data = data,
          arguments = if (is.list(payload)) {
            payload$args %||% list()
          } else {
            list()
          },
          model_name = if (inherits(model, "StanModel")) {
            model$model_name()
          } else {
            NULL
          }
        ),
        metadata
      )
      private$output_ <- if (is.list(payload)) {
        payload$output %||% character()
      } else {
        character()
      }
      if (inherits(model, "StanModel")) {
        private$initialize_pointer()
      }
      invisible(self)
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
      if (!inherits(private$model_, "StanModel")) {
        return(invisible(NULL))
      }
      if (!force && !is.null(private$model_ptr_)) {
        return(invisible(NULL))
      }
      private$model_ptr_ <- private$model_$new_model(
        private$data_,
        private$seed_
      )
      rng <- private$model_$native_function("new_base_rng", required = FALSE)
      private$rng_ptr_ <- if (is.null(rng)) NULL else rng(private$seed_)
      invisible(NULL)
    },
    ensure_native = function() {
      if (!inherits(private$model_, "StanModel")) {
        stop("This fit does not retain a model binding.", call. = FALSE)
      }
      private$initialize_pointer()
      probe <- private$model_$native_function(
        "model_num_upars",
        required = FALSE
      )
      if (!is.null(probe)) {
        valid <- tryCatch(
          {
            probe(private$model_ptr_)
            TRUE
          },
          error = function(e) FALSE
        )
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
        indices <- flat_names == name |
          startsWith(flat_names, paste0(name, "["))
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
    metadata_skeleton = function(
      metadata,
      transformed_parameters,
      generated_quantities
    ) {
      if (
        is.list(metadata) &&
          all(c("names", "dimensions", "stages") %in% names(metadata))
      ) {
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
        stages <- if ("stage" %in% names(metadata)) {
          metadata$stage
        } else {
          rep("parameter", nrow(metadata))
        }
        keep <- stages == "parameter" |
          (isTRUE(transformed_parameters) & stages == "transformed_parameter") |
          (isTRUE(generated_quantities) & stages == "generated_quantity")
        result <- lapply(which(keep), function(i) {
          dims <- if (length(dim_col)) {
            metadata[[dim_col[[1]]]][[i]]
          } else {
            integer()
          }
          if (!length(dims)) NA_real_ else array(NA_real_, dim = dims)
        })
        names(result) <- metadata[[name_col]][keep]
        return(result)
      }
      names <- private$model_$native_function("model_constrained_names")(
        private$model_ptr_,
        transformed_parameters,
        generated_quantities
      )
      names <- unique(sub("\\[.*$", "", names))
      stats::setNames(rep(list(NA_real_), length(names)), names)
    }
  ),
  cloneable = FALSE
)

# StanFit draws and summary methods --------------------------------------------

#' Extract posterior draws
#'
#' @name fit-method-draws
#' @aliases draws
#' @family StanFit methods
#'
#' @description Extract draws or point estimates from fitted model objects using
#'   formats provided by the \pkg{posterior} package. Depending on the fitting
#'   method, these are posterior draws from MCMC, approximate posterior draws
#'   from variational inference, Laplace approximation, or Pathfinder,
#'   standalone generated quantities, or a point estimate from optimization.
#'
#' @param variables (character vector) Which variables to extract. If `NULL`
#'   (the default), all variables are returned.
#' @param inc_warmup (logical) Should warmup draws be included? Defaults to
#'   `FALSE`. Only applies to [`StanMCMC`] objects.
#' @param format (string) The format of the returned draws. Must be a valid
#'   format from the \pkg{posterior} package. Defaults depend on the fitting
#'   method: `"draws_array"` for MCMC and generated quantities, `"draws_matrix"`
#'   for all other methods.
#'
#' @return An object from the \pkg{posterior} package in the requested format.
#'
#' @seealso [`$summary()`][fit-method-summary]
#'
NULL

fit_draws <- function(variables = NULL, inc_warmup = FALSE, format = NULL) {
  if (is.null(private$draws_)) {
    stop("This fit does not contain draws.", call. = FALSE)
  }
  draws <- private$draws_
  if (isTRUE(inc_warmup) && !is.null(private$warmup_draws_)) {
    draws <- posterior::bind_draws(
      private$warmup_draws_,
      draws,
      along = "iteration"
    )
  }
  draws <- .newstan_select_draws(draws, variables)
  .newstan_as_draws_format(draws, format %||% private$default_format_)
}
StanFit$set("public", "draws", fit_draws)

#' Summarize posterior draws
#'
#' @name fit-method-summary
#' @aliases summary
#' @family StanFit methods
#'
#' @description Compute summary statistics for posterior draws or point
#'   estimates using [posterior::summarise_draws()].
#'
#' @param ... Additional arguments passed to [posterior::summarise_draws()].
#'
#' @return A data frame of summary statistics.
#'
#' @seealso [`$draws()`][fit-method-draws], [`$print()`][fit-method-print]
#'
NULL

fit_summary <- function(variables = NULL, ...) {
  posterior::summarise_draws(
    self$draws(variables = variables, format = private$default_format_),
    ...
  )
}
StanFit$set("public", "summary", fit_summary)

#' Print fit summary
#'
#' @name fit-method-print
#' @aliases print
#' @family StanFit methods
#'
#' @description Print a summary table of posterior draws or point estimates.
#'
#' @param digits (integer) Number of digits for numeric summary statistics.
#' @param max_rows (integer) Maximum number of rows to print.
#'
#' @return The fitted model object, invisibly.
#'
#' @seealso [`$summary()`][fit-method-summary]
#'
NULL

fit_print <- function(variables = NULL, ..., digits = 2, max_rows = 20) {
  out <- self$summary(variables = variables, ...)
  print(utils::head(out, max_rows), digits = digits)
  invisible(self)
}
StanFit$set("public", "print", fit_print)

# StanFit information methods --------------------------------------------------

#' Access fit information
#'
#' @name fit-method-fit-info
#' @family StanFit methods
#'
#' @description These methods access information stored in a [`StanFit`] object.
#'
#'   ```
#'   return_codes()
#'   metadata()
#'   output(id = NULL)
#'   time()
#'   init()
#'   code()
#'   ```
#'
#' @param id (integer) Chain or path index for accessing per-chain output.
#'   If `NULL` (the default), all output is returned.
#'
#' @return
#' * `$return_codes()` returns an integer vector of Stan return codes (one per
#'   chain or path). A return code of `0` indicates success.
#' * `$metadata()` returns a named list of fit metadata including the seed,
#'   data, arguments, and model name.
#' * `$output()` returns character vector(s) of Stan output messages.
#' * `$time()` returns a list with timing information.
#' * `$init()` returns the user-specified initial values, or `NULL` if none
#'   were provided.
#' * `$code()` returns the Stan program as a string, or `character()` if the
#'   model is not available.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

fit_return_codes <- function() private$return_codes_
StanFit$set("public", "return_codes", fit_return_codes)

fit_metadata <- function() private$metadata_
StanFit$set("public", "metadata", fit_metadata)

fit_output <- function(id = NULL) {
  if (is.null(id)) {
    return(private$output_)
  }
  if (is.list(private$output_)) {
    return(private$output_[[id]])
  }
  private$output_
}
StanFit$set("public", "output", fit_output)

fit_time <- function() list(total = private$elapsed_)
StanFit$set("public", "time", fit_time)

fit_init <- function() private$init_
StanFit$set("public", "init", fit_init)

fit_code <- function() {
  if (!inherits(private$model_, "StanModel")) {
    character()
  } else {
    private$model_$code()
  }
}
StanFit$set("public", "code", fit_code)

# StanFit log density methods --------------------------------------------------

#' Extract log density draws
#'
#' @name fit-method-lp
#' @family StanFit methods
#'
#' @description Extract the log density (`lp__`) or log density approximation
#'   (`lp_approx__`) draws from a fitted model object.
#'
#'   ```
#'   lp()
#'   lp_approx()
#'   ```
#'
#' @return The log density draws in the default format for the fitting method.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

fit_lp <- function() self$draws(variables = "lp__")
StanFit$set("public", "lp", fit_lp)

fit_lp_approx <- function() self$draws(variables = "lp_approx__")
StanFit$set("public", "lp_approx", fit_lp_approx)

# StanFit save methods ---------------------------------------------------------

#' Save fitted model object
#'
#' @name fit-method-save
#' @family StanFit methods
#'
#' @description Methods for saving fitted model objects.
#'
#'   ```
#'   save_object(file, ...)
#'   ```
#'
#' @param file (string) Path where the file should be saved.
#' @param ... Additional arguments passed to [base::saveRDS()].
#'
#' @return
#' * `$save_object()` returns the normalized file path, invisibly.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

fit_save_object <- function(file, ...) {
  saveRDS(self, file = file, ...)
  invisible(normalizePath(file, mustWork = TRUE))
}
StanFit$set("public", "save_object", fit_save_object)

# StanFit model methods --------------------------------------------------------

#' Initialize model methods
#'
#' @name fit-method-init_model_methods
#' @aliases init_model_methods
#' @family StanFit methods
#'
#' @description Initialize the model pointer for computing log probability,
#'   gradients, Hessians, and parameter transformations. This is called
#'   automatically when the fit object is created if the model is available.
#'
#' @param seed (integer) The random seed for initializing the model.
#' @param verbose (logical) Whether to show verbose output.
#'
#' @return The fitted model object, invisibly.
#'
#' @seealso [`$log_prob()`][fit-method-model-methods],
#'   [`$constrain_variables()`][fit-method-model-methods]
#'
NULL

fit_init_model_methods <- function(seed = 1, verbose = FALSE) {
  private$seed_ <- .newstan_seed(seed)
  private$initialize_pointer(force = TRUE)
  invisible(self)
}
StanFit$set("public", "init_model_methods", fit_init_model_methods)

#' Compute log probability and transformations
#'
#' @name fit-method-model-methods
#' @family StanFit methods
#'
#' @description These methods compute the log probability, gradients, Hessians,
#'   and parameter transformations using the compiled Stan model. They require
#'   the model to be available and initialized via
#'   [`$init_model_methods()`][fit-method-init_model_methods].
#'
#'   ```
#'   log_prob(unconstrained_variables, jacobian = TRUE)
#'   grad_log_prob(unconstrained_variables, jacobian = TRUE)
#'   hessian(unconstrained_variables, jacobian = TRUE)
#'   constrain_variables(unconstrained_variables,
#'                       transformed_parameters = TRUE,
#'                       generated_quantities = TRUE)
#'   unconstrain_variables(variables)
#'   unconstrain_draws(draws = NULL, format, inc_warmup = FALSE)
#'   variable_skeleton(transformed_parameters = TRUE,
#'                     generated_quantities = TRUE)
#'   ```
#'
#' @param unconstrained_variables (numeric vector) The unconstrained parameter
#'   values at which to evaluate the function.
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacob of the inverse transformation?
#' @param variables (named list) Constrained parameter values to unconstrain.
#' @param draws A posterior draws object, or `NULL` to use the fit's draws.
#' @param format (string) The output format from the \pkg{posterior} package.
#' @param inc_warmup (logical) Include warmup draws?
#' @param transformed_parameters (logical) Include transformed parameters?
#' @param generated_quantities (logical) Include generated quantities?
#'
#' @return
#' * `$log_prob()` returns the log probability as a numeric scalar.
#' * `$grad_log_prob()` returns a list with `value` (log probability) and
#'   `gradient` (gradient vector).
#' * `$hessian()` returns a list with `value` (log probability), `gradient`,
#'   and `hessian` (Hessian matrix).
#' * `$constrain_variables()` returns a named list of constrained parameter
#'   values.
#' * `$unconstrain_variables()` returns a numeric vector of unconstrained
#'   parameter values.
#' * `$unconstrain_draws()` returns a posterior draws object with unconstrained
#'   parameter values.
#' * `$variable_skeleton()` returns a named list with the structure of the
#'   constrained parameter space.
#'
#' @seealso [`$init_model_methods()`][fit-method-init_model_methods]
#'
NULL

fit_log_prob <- function(unconstrained_variables, jacobian = TRUE) {
  private$check_jacobian(jacobian)
  as.numeric(private$native_call(
    "model_log_prob",
    as.double(unconstrained_variables),
    jacobian
  ))
}
StanFit$set("public", "log_prob", fit_log_prob)

fit_grad_log_prob <- function(unconstrained_variables, jacobian = TRUE) {
  private$check_jacobian(jacobian)
  private$native_call(
    "model_grad_log_prob",
    as.double(unconstrained_variables),
    jacobian
  )
}
StanFit$set("public", "grad_log_prob", fit_grad_log_prob)

fit_hessian <- function(unconstrained_variables, jacobian = TRUE) {
  private$check_jacobian(jacobian)
  private$native_call(
    "model_hessian",
    as.double(unconstrained_variables),
    jacobian
  )
}
StanFit$set("public", "hessian", fit_hessian)

fit_constrain_variables <- function(
  unconstrained_variables,
  transformed_parameters = TRUE,
  generated_quantities = TRUE
) {
  private$ensure_native()
  flat <- private$model_$native_function("model_constrain")(
    private$model_ptr_,
    private$rng_ptr_,
    as.double(unconstrained_variables),
    as.logical(transformed_parameters),
    as.logical(generated_quantities)
  )
  skeleton <- self$variable_skeleton(
    transformed_parameters,
    generated_quantities
  )
  private$relist_constrained(flat, skeleton)
}
StanFit$set("public", "constrain_variables", fit_constrain_variables)

fit_unconstrain_variables <- function(variables) {
  if (!is.list(variables) || is.null(names(variables))) {
    stop(
      "`variables` must be a named list of constrained values.",
      call. = FALSE
    )
  }
  tryCatch(
    private$native_call("model_unconstrain", variables),
    error = function(error) {
      stop(
        "Could not unconstrain variables; check parameter names, ",
        "dimensions, and bounds. ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}
StanFit$set("public", "unconstrain_variables", fit_unconstrain_variables)

fit_unconstrain_draws <- function(
  draws = NULL,
  format = getOption("newstan_draws_format", "draws_array"),
  inc_warmup = FALSE
) {
  source <- draws %||%
    self$draws(
      inc_warmup = inc_warmup,
      format = "draws_df"
    )
  source <- posterior::as_draws_df(source)
  private$ensure_native()
  native_names <- private$model_$native_function(
    "model_constrained_names"
  )(private$model_ptr_, FALSE, FALSE)
  draw_names <- .newstan_bracket_names(native_names)
  missing <- setdiff(draw_names, posterior::variables(source))
  if (length(missing)) {
    stop(
      "The draws are missing model parameters: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  values <- as.matrix(as.data.frame(source, check.names = FALSE)[
    draw_names
  ])
  result <- private$model_$native_function("model_unconstrain_matrix")(
    private$model_ptr_,
    values
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
}
StanFit$set("public", "unconstrain_draws", fit_unconstrain_draws)

fit_variable_skeleton <- function(
  transformed_parameters = TRUE,
  generated_quantities = TRUE
) {
  metadata <- private$native_call("model_param_metadata")
  private$metadata_skeleton(
    metadata,
    transformed_parameters = transformed_parameters,
    generated_quantities = generated_quantities
  )
}
StanFit$set("public", "variable_skeleton", fit_variable_skeleton)

# StanMCMC class ---------------------------------------------------------------

#' StanMCMC objects
#'
#' @name StanMCMC
#' @description A `StanMCMC` object is returned by [`$sample()`][model-method-sample]
#'   and contains MCMC posterior draws, sampler diagnostics, and fit metadata.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanMCMC` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$sampler_diagnostics()`][fit-method-mcmc] | Extract sampler diagnostics. |
#'  [`$num_chains()`][fit-method-mcmc] | Return the number of chains. |
#'  [`$diagnostic_summary()`][fit-method-mcmc] | Summarize sampler diagnostics. |
#'  [`$inv_metric()`][fit-method-mcmc] | Return the inverse mass matrix. |
#'
NULL

StanMCMC <- R6Class(
  "StanMCMC",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list()
    ) {
      warmup <- NULL
      warmup_diagnostics <- NULL
      if (isTRUE(payload$args$save_warmup) && !is.null(payload$draws)) {
        all_draws <- posterior::as_draws_array(payload$draws)
        warmup_iterations <- ceiling(
          (payload$args$num_warmup %||% 0L) / (payload$args$thin %||% 1L)
        )
        if (
          warmup_iterations > 0L &&
            posterior::niterations(all_draws) >= warmup_iterations
        ) {
          warmup <- all_draws[seq_len(warmup_iterations), , , drop = FALSE]
          sampling_iterations <- if (
            warmup_iterations < posterior::niterations(all_draws)
          ) {
            seq.int(warmup_iterations + 1L, posterior::niterations(all_draws))
          } else {
            integer()
          }
          payload$draws <- all_draws[sampling_iterations, , , drop = FALSE]
          if (!is.null(payload$diagnostics)) {
            all_diagnostics <- posterior::as_draws_array(payload$diagnostics)
            warmup_diagnostics <- all_diagnostics[
              seq_len(warmup_iterations),
              ,
              ,
              drop = FALSE
            ]
            payload$diagnostics <- all_diagnostics[
              sampling_iterations,
              ,
              ,
              drop = FALSE
            ]
          }
        }
      }
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_array"
      )
      private$warmup_draws_ <- .newstan_normalize_draw_names(warmup)
      private$warmup_diagnostics_ <-
        .newstan_normalize_draw_names(warmup_diagnostics)
    }
  ),
  cloneable = FALSE
)

#' MCMC sampler diagnostics
#'
#' @name fit-method-mcmc
#' @family StanFit methods
#'
#' @description Methods specific to [`StanMCMC`] objects for accessing sampler
#'   diagnostics and chain information.
#'
#'   ```
#'   sampler_diagnostics(inc_warmup = FALSE, format = "draws_array")
#'   num_chains()
#'   diagnostic_summary()
#'   inv_metric(matrix = TRUE)
#'   ```
#'
#'
#' @return
#' * `$sampler_diagnostics()` returns sampler diagnostics (e.g., `divergent__`,
#'   `treedepth__`, `accept__`) as a posterior draws object.
#' * `$num_chains()` returns the number of MCMC chains.
#' * `$diagnostic_summary()` returns a data frame with counts of divergent
#'   transitions and max treedepth warnings.
#' * `$inv_metric()` returns the inverse mass matrix used during sampling.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

mcmc_sampler_diagnostics <- function(
  inc_warmup = FALSE,
  format = "draws_array"
) {
  diagnostics <- private$diagnostics_
  if (is.null(diagnostics)) {
    stop("This fit does not contain sampler diagnostics.", call. = FALSE)
  }
  if (isTRUE(inc_warmup) && !is.null(private$warmup_diagnostics_)) {
    diagnostics <- posterior::bind_draws(
      private$warmup_diagnostics_,
      diagnostics,
      along = "iteration"
    )
  }
  .newstan_as_draws_format(diagnostics, format)
}
StanMCMC$set("public", "sampler_diagnostics", mcmc_sampler_diagnostics)

mcmc_num_chains <- function() {
  private$metadata_$chains %||%
    posterior::nchains(private$draws_)
}
StanMCMC$set("public", "num_chains", mcmc_num_chains)

mcmc_diagnostic_summary <- function() {
  diagnostics <- self$sampler_diagnostics(format = "draws_matrix")
  data.frame(
    num_divergent = if ("divergent__" %in% colnames(diagnostics)) {
      sum(diagnostics[, "divergent__"] > 0)
    } else {
      NA_integer_
    },
    num_max_treedepth = if ("treedepth__" %in% colnames(diagnostics)) {
      sum(
        diagnostics[, "treedepth__"] >=
          (private$metadata_$arguments$max_depth %||% 10L)
      )
    } else {
      NA_integer_
    }
  )
}
StanMCMC$set("public", "diagnostic_summary", mcmc_diagnostic_summary)

mcmc_inv_metric <- function(matrix = TRUE) {
  metric <- private$payload_$inv_metric
  if (is.null(metric)) {
    stop(
      "The native service did not retain an inverse metric.",
      call. = FALSE
    )
  }
  metric
}
StanMCMC$set("public", "inv_metric", mcmc_inv_metric)

# StanMLE class ----------------------------------------------------------------

#' StanMLE objects
#'
#' @name StanMLE
#' @description A `StanMLE` object is returned by
#'   [`$optimize()`][model-method-optimize] and contains a point estimate
#'   (MAP or MLE) and optimization metadata.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanMLE` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$mle()`][fit-method-mle] | Extract the point estimate as a numeric vector. |
#'
NULL

StanMLE <- R6Class(
  "StanMLE",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list()
    ) {
      par <- payload$par %||% numeric()
      if (!is.null(names(par))) {
        par <- par[names(par) != "lp__"]
        names(par) <- .newstan_bracket_names(names(par))
      }
      payload$par <- par
      if (length(par)) {
        payload$draws <- matrix(
          c(payload$value %||% NA_real_, unname(par)),
          nrow = 1L,
          dimnames = list(NULL, c("lp__", names(par)))
        )
      }
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    }
  ),
  cloneable = FALSE
)

#' Extract point estimate
#'
#' @name fit-method-mle
#' @aliases mle
#' @family StanFit methods
#'
#' @description Extract the maximum likelihood or maximum a posteriori estimate
#'   from a [`StanMLE`] object as a named numeric vector.
#'
#' @param variables (character vector) Which variables to extract. If `NULL`,
#'   all variables are returned.
#'
#' @return A named numeric vector of parameter estimates.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

mle_mle <- function(variables = NULL) {
  value <- private$payload_$par %||% numeric()
  if (!is.null(variables)) {
    value <- value[variables]
  }
  value
}
StanMLE$set("public", "mle", mle_mle)

mle_summary <- function(variables = NULL, ...) {
  value <- self$mle(variables)
  data.frame(
    variable = names(value),
    estimate = unname(value),
    row.names = NULL
  )
}
StanMLE$set("public", "summary", mle_summary)

# StanLaplace class ------------------------------------------------------------

#' StanLaplace objects
#'
#' @name StanLaplace
#' @description A `StanLaplace` object is returned by
#'   [`$laplace()`][model-method-laplace] and contains draws from a Gaussian
#'   approximation to the posterior centered at the mode.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanLaplace` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$mode()`][fit-method-laplace-mode] | Return the mode used for the approximation. |
#'
NULL

StanLaplace <- R6Class(
  "StanLaplace",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list(),
      mode = NULL
    ) {
      private$mode_ <- mode
      payload$draws <- .newstan_rename_draw_columns(payload$draws)
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    }
  ),
  private = list(mode_ = NULL),
  cloneable = FALSE
)

#' Extract Laplace mode
#'
#' @name fit-method-laplace-mode
#' @aliases mode
#' @family StanFit methods
#'
#' @description Return the mode (point estimate) used as the center of the
#'   Laplace approximation. This is either a [`StanMLE`] object from a prior
#'   call to [`$optimize()`][model-method-optimize], or a numeric vector
#'   provided directly.
#'
#' @return The mode object or numeric vector.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

laplace_mode <- function() private$mode_
StanLaplace$set("public", "mode", laplace_mode)

# StanVB class -----------------------------------------------------------------

#' StanVB objects
#'
#' @name StanVB
#' @description A `StanVB` object is returned by
#'   [`$variational()`][model-method-variational] and contains approximate
#'   posterior draws from Automatic Differentiation Variational Inference (ADVI).
#'
NULL

StanVB <- R6Class(
  "StanVB",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list()
    ) {
      draws <- if (is.null(payload$draws)) {
        NULL
      } else {
        posterior::as_draws_matrix(payload$draws)
      }
      draws <- .newstan_rename_draw_columns(draws)
      requested <- payload$args$output_samples %||% NULL
      if (
        !is.null(draws) && !is.null(requested) && nrow(draws) == requested + 1L
      ) {
        draws <- draws[-1L, , drop = FALSE]
      }
      if (
        !is.null(draws) &&
          "lp__" %in% colnames(draws) &&
          all(is.na(draws[, "lp__"]) | draws[, "lp__"] == 0)
      ) {
        draws <- draws[, setdiff(colnames(draws), "lp__"), drop = FALSE]
      }
      payload$draws <- draws
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    }
  ),
  cloneable = FALSE
)

# StanPathfinder class ---------------------------------------------------------

#' StanPathfinder objects
#'
#' @name StanPathfinder
#' @description A `StanPathfinder` object is returned by
#'   [`$pathfinder()`][model-method-pathfinder] and contains approximate
#'   posterior draws from the Pathfinder algorithm.
#'
NULL

StanPathfinder <- R6Class(
  "StanPathfinder",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list()
    ) {
      payload <- .newstan_merge_payload_draws(payload)
      payload$draws <- .newstan_rename_draw_columns(payload$draws)
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    }
  ),
  cloneable = FALSE
)

# StanGQ class -----------------------------------------------------------------

#' StanGQ objects
#'
#' @name StanGQ
#' @description A `StanGQ` object is returned by
#'   [`$generate_quantities()`][model-method-generate-quantities] and contains
#'   the generated quantities computed from posterior draws.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanGQ` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$num_chains()`][fit-method-gq] | Return the number of chains in the draws. |
#'
NULL

StanGQ <- R6Class(
  "StanGQ",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list()
    ) {
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_array"
      )
    }
  ),
  cloneable = FALSE
)

#' Generated quantities chain count
#'
#' @name fit-method-gq
#' @family StanFit methods
#'
#' @description Return the number of chains in a [`StanGQ`] object's draws.
#'
#' @return An integer giving the number of chains, or `0` if no draws are
#'   available.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

gq_num_chains <- function() {
  if (is.null(private$draws_)) {
    return(0L)
  }
  posterior::nchains(private$draws_)
}
StanGQ$set("public", "num_chains", gq_num_chains)

# StanDiagnose class -----------------------------------------------------------

#' StanDiagnose objects
#'
#' @name StanDiagnose
#' @description A `StanDiagnose` object is returned by
#'   [`$diagnose()`][model-method-diagnose] and contains the results of Stan's
#'   gradient checking diagnostics.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanDiagnose` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$gradients()`][fit-method-diagnose] | Return the gradient check results. |
#'  [`$num_failed()`][fit-method-diagnose] | Return the number of failed gradient checks. |
#'
NULL

StanDiagnose <- R6Class(
  "StanDiagnose",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = NA_integer_,
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list()
    ) {
      metadata$num_failed <- as.integer(payload)
      super$initialize(
        list(return_code = 0L, gradients = attr(payload, "gradients")),
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    }
  ),
  cloneable = FALSE
)

#' Gradient diagnostic results
#'
#' @name fit-method-diagnose
#' @family StanFit methods
#'
#' @description Access gradient checking results from a [`StanDiagnose`] object.
#'
#'   ```
#'   gradients()
#'   num_failed()
#'   ```
#'
#' @return
#' * `$gradients()` returns a data frame with gradient check results for each
#'   parameter.
#' * `$num_failed()` returns the number of parameters that failed the gradient
#'   check.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

diagnose_gradients <- function() private$payload_$gradients %||% data.frame()
StanDiagnose$set("public", "gradients", diagnose_gradients)

diagnose_num_failed <- function() private$metadata_$num_failed
StanDiagnose$set("public", "num_failed", diagnose_num_failed)
