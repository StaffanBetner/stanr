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
#'
#'  ## Model methods
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$log_prob()`][fit-method-model-methods] | Compute the log probability. |
#'  [`$grad_log_prob()`][fit-method-model-methods] | Compute the gradient of the log probability. |
#'  [`$hessian()`][fit-method-model-methods] | Compute the Hessian of the log probability. |
#'  [`$constrain_variables()`][fit-method-model-methods] | Constrain unconstrained parameters. |
#'  [`$unconstrain_variables()`][fit-method-model-methods] | Unconstrain parameters. |
#'  [`$unconstrain_draws()`][fit-method-model-methods] | Unconstrain posterior draws. |
#'  [`$variable_skeleton()`][fit-method-model-methods] | Return a skeleton of the variable structure. |
#'
#'  ## Function exposure
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$expose_stan_functions()`][fit-method-expose-stan-functions] | Expose the model's `functions` block as R functions. |
#'  [`$functions`][fit-method-expose-stan-functions] | Environment holding the exposed functions (shared with the model). |
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
      private$model_ <- model
      private$data_ <- data
      private$seed_ <- seed
      private$init_ <- init
      # Reuse the model pointer the run wrapper already constructed (with the
      # same data/seed) instead of re-running `transformed data` on first use.
      private$model_ptr_ <- payload$model_ptr
      private$elapsed_ <- elapsed
      private$default_format_ <- default_format
      private$draws_ <- .stanr_normalize_draw_names(payload$draws)
      private$diagnostics_ <- .stanr_normalize_draw_names(payload$diagnostics)
      private$inv_metric_ <- payload$inv_metric
      private$par_ <- payload$par
      chains <- metadata$chains %||% 1L
      private$return_codes_ <- rep(
        as.integer(payload$return_code %||% NA_integer_),
        length.out = chains
      )
      # `data` is intentionally omitted here and spliced back in by
      # `fit_metadata()` -- storing it a second time (it already lives in
      # `private$data_`) would double its footprint in `saveRDS()`/serialize
      # output, since R serialization does not deduplicate identical objects
      # reachable via two different fields.
      private$metadata_ <- utils::modifyList(
        list(
          seed = seed,
          arguments = payload$args %||% list(),
          model_name = if (inherits(model, "StanModel")) {
            model$model_name()
          } else {
            NULL
          }
        ),
        metadata
      )
      if (!is.null(payload$step_size)) {
        private$metadata_$step_size_adaptation <- as.numeric(payload$step_size)
      }
      private$output_ <- payload$output %||% character()
      invisible(self)
    }
  ),
  private = list(
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
    inv_metric_ = NULL,
    par_ = NULL,
    return_codes_ = NULL,
    metadata_ = NULL,
    output_ = NULL,
    model_ptr_ = NULL,
    rng_ptr_ = NULL,
    native_generation_ = NA_integer_,
    initialize_pointer = function(force = FALSE) {
      if (!inherits(private$model_, "StanModel")) {
        return(invisible(NULL))
      }
      if (force || is.null(private$model_ptr_)) {
        data <- private$data_
        declarations <- if (any(vapply(data, is.list, logical(1)))) {
          private$model_$variables()$data
        }
        private$model_ptr_ <- private$model_$new_model(
          data,
          private$seed_,
          declarations
        )
      }
      if (force || is.null(private$rng_ptr_)) {
        rng <- private$model_$native_function("new_base_rng", required = FALSE)
        private$rng_ptr_ <- if (is.null(rng)) NULL else rng(private$seed_)
      }
      invisible(NULL)
    },
    ensure_native = function() {
      if (!inherits(private$model_, "StanModel")) {
        stop("This fit does not retain a model binding.", call. = FALSE)
      }
      # Generations are plain data serialized with the fit, so they still
      # match after readRDS(). What breaks in a restore is the pointer
      # (nulled) and the sourceCpp wrappers' native symbols, neither of which
      # survives serialization. A failed probe therefore rebuilds the
      # compiled environment; the cached `.so` (keyed on `model_hash`) stays
      # valid.
      if (
        !is.na(private$native_generation_) &&
          private$native_generation_ == private$model_$compile_generation() &&
          !.stanr_xptr_is_null(private$model_ptr_)
      ) {
        return(invisible(NULL))
      }
      # A generation change means the model was recompiled underneath this
      # fit: `model_ptr_`/`rng_ptr_` came from the superseded artifact and
      # must be rebuilt, not probed through its vtable (see
      # `.stanr_forced_rebuild_target()`). A restore (`native_generation_`
      # NA) is not a generation change -- there the pointer is genuinely
      # absent and the probe/recovery path below handles it.
      generation_changed <- !is.na(private$native_generation_) &&
        private$native_generation_ != private$model_$compile_generation()
      private$initialize_pointer(force = generation_changed)
      probe <- private$model_$native_function(
        "model_num_upars",
        required = FALSE
      )
      if (is.null(probe)) {
        private$native_generation_ <- private$model_$compile_generation()
        return(invisible(NULL))
      }
      valid <- tryCatch(
        {
          probe(private$model_ptr_)
          TRUE
        },
        error = function(e) FALSE
      )
      if (!valid) {
        private$model_$compile(force_recompile = FALSE, quiet = TRUE)
        private$initialize_pointer(force = TRUE)
      }
      private$native_generation_ <- private$model_$compile_generation()
      invisible(NULL)
    },
    native_call = function(name, ...) {
      private$ensure_native()
      private$model_$native_function(name)(private$model_ptr_, ...)
    },
    # The declared type structure the native relist/skeleton builders need,
    # covering every output block.
    variable_declarations = function() {
      declared <- private$model_$variables()
      c(
        declared$parameters,
        declared$transformed_parameters,
        declared$generated_quantities
      )
    },
    check_jacobian = function(jacobian) {
      if (!is.logical(jacobian) || length(jacobian) != 1L || is.na(jacobian)) {
        stop("`jacobian` must be TRUE or FALSE.", call. = FALSE)
      }
      invisible(NULL)
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
#'   `FALSE`. Errors unless the fit is a [`$sample()`][model-method-sample] run
#'   with `save_warmup = TRUE`.
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
  inc_warmup <- .stanr_flag(inc_warmup, "inc_warmup")
  if (is.null(private$draws_)) {
    stop("This fit does not contain draws.", call. = FALSE)
  }
  draws <- private$draws_
  if (inc_warmup) {
    if (is.null(private$warmup_draws_)) {
      stop(
        "warmup draws were not saved; only `$sample()` runs with ",
        "`save_warmup = TRUE` store them.",
        call. = FALSE
      )
    }
    draws <- posterior::bind_draws(
      private$warmup_draws_,
      draws,
      along = "iteration"
    )
  }
  if (!is.null(variables)) {
    draws <- posterior::subset_draws(draws, variable = variables)
  }
  .stanr_as_draws_format(draws, format %||% private$default_format_)
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
  # Summarise only the variables that will print: ESS/R-hat over thousands
  # of parameters just to `head()` the result is the expensive part.
  if (is.null(variables) && !is.null(private$draws_)) {
    variables <- utils::head(posterior::variables(private$draws_), max_rows)
  }
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
#'   output()
#'   time()
#'   init()
#'   code()
#'   ```
#'
#' @return
#' * `$return_codes()` returns an integer vector with the Stan service's
#'   return code replicated once per chain (the in-process services report a
#'   single code for the whole run). A return code of `0` indicates success.
#' * `$metadata()` returns a named list of fit metadata including the seed,
#'   data, arguments, and model name. For adaptive MCMC fits,
#'   `step_size_adaptation` is a numeric vector of one adapted step size per
#'   chain; it is absent when sampling ran without adaptation.
#' * `$output()` returns a character vector of Stan output messages from the
#'   run. Messages from all chains or paths are interleaved in one vector;
#'   per-chain attribution is not available.
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

fit_metadata <- function() {
  # `data` is stored only in `private$data_` (see `initialize()`) to avoid
  # serializing it twice; splice it back in here so the public return value
  # is unchanged. Insert right after `seed` to preserve the original field
  # order (`seed, data, arguments, model_name, ...`) rather than appending
  # `data` at the end, which is what `utils::modifyList()` would do.
  seed_pos <- which(names(private$metadata_) == "seed")
  after <- if (length(seed_pos)) seed_pos[[1]] else 0L
  append(private$metadata_, list(data = private$data_), after = after)
}
StanFit$set("public", "metadata", fit_metadata)

fit_output <- function() private$output_
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
#'   (`lp_approx__`) as a numeric vector. `$lp_approx()` is only available on
#'   [`StanLaplace`], [`StanVB`], and [`StanPathfinder`] objects.
#'
#'   ```
#'   lp()
#'   lp_approx()
#'   ```
#'
#' @return A numeric vector, with one element per (post-warmup) draw, or a
#'   single value for optimization.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

fit_lp <- function() {
  as.numeric(self$draws(variables = "lp__", format = "draws_matrix"))
}
StanFit$set("public", "lp", fit_lp)

# Attached to StanLaplace/StanVB/StanPathfinder below, not StanFit itself --
# MCMC, MLE, and GQ have no approximating distribution to report.
fit_lp_approx <- function() {
  as.numeric(self$draws(variables = "lp_approx__", format = "draws_matrix"))
}

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

#' Compute log probability and transformations
#'
#' @name fit-method-model-methods
#' @family StanFit methods
#'
#' @description These methods compute the log probability, gradients, Hessians,
#'   and parameter transformations using the compiled Stan model. They require
#'   the model to be available, and lazily initialize the native model pointer
#'   on first use.
#'
#'   ```
#'   log_prob(unconstrained_variables, jacobian = TRUE)
#'   grad_log_prob(unconstrained_variables, jacobian = TRUE)
#'   hessian(unconstrained_variables, jacobian = TRUE)
#'   constrain_variables(unconstrained_variables,
#'                       transformed_parameters = TRUE,
#'                       generated_quantities = TRUE)
#'   unconstrain_variables(variables)
#'   unconstrain_draws(draws = NULL, format = NULL, inc_warmup = FALSE)
#'   variable_skeleton(transformed_parameters = TRUE,
#'                     generated_quantities = TRUE)
#'   ```
#'
#' @param unconstrained_variables (numeric vector) The unconstrained parameter
#'   values at which to evaluate the function.
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacobian of the inverse transformation?
#' @param variables (named list) Constrained parameter values to unconstrain.
#' @param draws A posterior draws object, or `NULL` to use the fit's draws.
#' @param format (string) The output format from the \pkg{posterior} package.
#'   Defaults to `NULL`, which uses the fit's default draws format (see
#'   [`$draws()`][fit-method-draws]).
#' @param inc_warmup (logical) Include warmup draws?
#' @param transformed_parameters (logical) Include transformed parameters?
#' @param generated_quantities (logical) Include generated quantities?
#'
#' @return
#' * `$log_prob()` returns the log probability as a numeric scalar.
#' * `$grad_log_prob()` returns the gradient as a numeric vector, with the log
#'   probability attached as the `log_prob` attribute.
#' * `$hessian()` returns a list with elements `log_prob`, `grad_log_prob`,
#'   and `hessian`.
#' * `$constrain_variables()` returns a named list of constrained parameter
#'   values.
#' * `$unconstrain_variables()` returns a numeric vector of unconstrained
#'   parameter values.
#' * `$unconstrain_draws()` returns a posterior draws object with unconstrained
#'   parameter values.
#' * `$variable_skeleton()` returns a named list with the structure of the
#'   constrained parameter space.
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
  transformed_parameters <- .stanr_flag(
    transformed_parameters,
    "transformed_parameters"
  )
  generated_quantities <- .stanr_flag(
    generated_quantities,
    "generated_quantities"
  )
  private$ensure_native()
  private$model_$native_function("model_constrain_variables")(
    private$model_ptr_,
    private$rng_ptr_,
    as.double(unconstrained_variables),
    transformed_parameters,
    generated_quantities,
    private$variable_declarations()
  )
}
StanFit$set("public", "constrain_variables", fit_constrain_variables)

fit_unconstrain_variables <- function(variables) {
  if (!is.list(variables) || is.null(names(variables))) {
    stop(
      "`variables` must be a named list of constrained values.",
      call. = FALSE
    )
  }
  if (!inherits(private$model_, "StanModel")) {
    stop("This fit does not retain a model binding.", call. = FALSE)
  }
  # Tuple-typed entries (bare unnamed R lists) are flattened natively, which
  # needs the declared structure; gate the stanc-info cost behind a cheap
  # list check.
  declarations <- if (any(vapply(variables, is.list, logical(1)))) {
    private$model_$variables()$parameters
  }
  tryCatch(
    private$native_call("model_unconstrain", variables, declarations),
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
  format = NULL,
  inc_warmup = FALSE
) {
  format <- format %||% private$default_format_
  inc_warmup <- .stanr_flag(inc_warmup, "inc_warmup")
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
  draw_names <- .stanr_bracket_names(native_names)
  missing <- setdiff(draw_names, posterior::variables(source))
  if (length(missing)) {
    stop(
      "The draws are missing model parameters: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # `subset_draws()` returns columns in `draw_names` order, which is
  # load-bearing: the native call expects native constrained-parameter order.
  values <- posterior::as_draws_matrix(
    posterior::subset_draws(source, variable = draw_names)
  )
  result <- private$model_$native_function("model_unconstrain_matrix")(
    private$model_ptr_,
    values
  )
  colnames(result) <- .stanr_bracket_names(colnames(result))
  result <- posterior::as_draws_df(data.frame(
    as.data.frame(result, check.names = FALSE),
    .chain = source$.chain,
    .iteration = source$.iteration,
    .draw = source$.draw,
    check.names = FALSE
  ))
  .stanr_as_draws_format(result, format)
}
StanFit$set("public", "unconstrain_draws", fit_unconstrain_draws)

fit_variable_skeleton <- function(
  transformed_parameters = TRUE,
  generated_quantities = TRUE
) {
  transformed_parameters <- .stanr_flag(
    transformed_parameters,
    "transformed_parameters"
  )
  generated_quantities <- .stanr_flag(
    generated_quantities,
    "generated_quantities"
  )
  private$native_call(
    "model_variable_skeleton",
    transformed_parameters,
    generated_quantities,
    private$variable_declarations()
  )
}
StanFit$set("public", "variable_skeleton", fit_variable_skeleton)

#' Expose Stan functions from a fitted model
#'
#' @name fit-method-expose-stan-functions
#' @family StanFit methods
#'
#' @description The `$expose_stan_functions()` method of a [`StanFit`]
#'   object delegates to the bound [`StanModel`]'s
#'   [`$expose_stan_functions()`][model-method-expose-stan-functions] -- see
#'   that topic for the full description of `global`/`verbose`, `_rng`
#'   reproducibility, caching, and the serialization caveat.
#'   `$expose_functions()` is an alias. `$functions` is a read-only active
#'   binding returning the model's `$functions` environment -- shared with
#'   the model and with every other fit from the same model, since exposure
#'   is memoized on the model, not per fit.
#'
#'   ```
#'   expose_stan_functions(global = FALSE, verbose = FALSE)
#'   expose_functions(global = FALSE, verbose = FALSE)
#'   ```
#'
#' @param global (logical) Should the exposed functions also be assigned
#'   into the global environment?
#' @param verbose (logical) Should compiler progress messages be printed?
#'
#' @return `$expose_stan_functions()`/`$expose_functions()` return the
#'   model's `$functions` environment, invisibly. Both methods, and reading
#'   `$functions`, error if this fit does not retain a model binding (e.g.
#'   after restoring a fit that was saved without its model).
#'
#' @seealso [`$expose_stan_functions()`][model-method-expose-stan-functions]
#'
NULL

fit_expose_stan_functions <- function(global = FALSE, verbose = FALSE) {
  if (!inherits(private$model_, "StanModel")) {
    stop("This fit does not retain a model binding.", call. = FALSE)
  }
  private$model_$expose_stan_functions(global, verbose)
}
StanFit$set("public", "expose_stan_functions", fit_expose_stan_functions)
StanFit$set("public", "expose_functions", fit_expose_stan_functions)

fit_functions <- function(value) {
  if (!missing(value)) {
    stop("`$functions` is read-only.", call. = FALSE)
  }
  if (!inherits(private$model_, "StanModel")) {
    stop("This fit does not retain a model binding.", call. = FALSE)
  }
  private$model_$functions
}
StanFit$set("active", "functions", fit_functions)

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
      private$warmup_draws_ <- .stanr_normalize_draw_names(
        payload$warmup_draws
      )
      private$warmup_diagnostics_ <- .stanr_normalize_draw_names(
        payload$warmup_diagnostics
      )
      # Mirrors cmdstanr's `CmdStanMCMC$initialize()`: warn about sampler
      # problems unprompted, right after sampling. Skipped when sampling
      # produced nothing to check (`private$diagnostics_` stays `NULL` when
      # `$sample()`'s native call itself failed) or for `fixed_param` runs,
      # which have no HMC diagnostics to report.
      if (!is.null(private$diagnostics_) && !isTRUE(metadata$fixed_param)) {
        invisible(self$diagnostic_summary(
          metadata$diagnostics %||% c("divergences", "treedepth", "ebfmi"),
          quiet = FALSE
        ))
      }
    }
  ),
  cloneable = FALSE
)

#' MCMC-specific methods
#'
#' @name fit-method-mcmc
#' @family StanFit methods
#'
#' @description Methods specific to [`StanMCMC`] objects for accessing sampler
#'   diagnostics, chain information, and model evaluation.
#'
#'   ```
#'   sampler_diagnostics(inc_warmup = FALSE, format = "draws_array")
#'   num_chains()
#'   diagnostic_summary(diagnostics = c("divergences", "treedepth", "ebfmi"),
#'                       quiet = FALSE)
#'   inv_metric(matrix = TRUE)
#'   loo(variables = "log_lik", r_eff = FALSE, moment_match = FALSE, ...)
#'   ```
#'
#' @param matrix (logical) For `$inv_metric()`: return each chain's inverse
#'   metric as a matrix? For a diagonal metric, `TRUE` (the default) wraps the
#'   adapted diagonal in `diag()`; `FALSE` returns it as a vector. For a dense
#'   metric, the matrix is always returned and `matrix = FALSE` is an error.
#' @param diagnostics (character vector) For `$diagnostic_summary()`: which
#'   diagnostics to check. One or more of `"divergences"`, `"treedepth"`, and
#'   `"ebfmi"`. The default checks all three; `NULL` or `""` checks none.
#' @param quiet (logical) For `$diagnostic_summary()`: should messages about
#'   problems found in the requested diagnostics be suppressed? The default,
#'   `FALSE`, prints them in addition to returning the values. Diagnostics
#'   that could not be computed at all (rather than computed and found
#'   problematic) always warn, regardless of `quiet`.
#'
#' @return
#' * `$sampler_diagnostics()` returns sampler diagnostics (e.g., `divergent__`,
#'   `treedepth__`, `accept__`) as a posterior draws object.
#' * `$num_chains()` returns the number of MCMC chains.
#' * `$diagnostic_summary()` returns a list with one element per requested
#'   diagnostic: `num_divergent` (divergent transitions per chain),
#'   `num_max_treedepth` (iterations per chain that hit the max treedepth),
#'   and/or `ebfmi` (E-BFMI per chain). An element is `NA` for every chain if
#'   the corresponding diagnostic was not collected (e.g. `divergent__`/
#'   `treedepth__`/`energy__` are unavailable for the `static` engine, or are
#'   all-`NA` for `fixed_param` runs).
#' * `$inv_metric()` returns a list (one element per chain) of the inverse
#'   mass matrix adapted during sampling. Errors if the fit was not sampled
#'   with adaptation.
#' * `$loo()` returns a LOO-CV object from the \pkg{loo} package.
#'
#' @seealso [`$draws()`][fit-method-draws], [`$loo()`][fit-method-loo]
#'
NULL

mcmc_sampler_diagnostics <- function(
  inc_warmup = FALSE,
  format = "draws_array"
) {
  inc_warmup <- .stanr_flag(inc_warmup, "inc_warmup")
  diagnostics <- private$diagnostics_
  if (is.null(diagnostics)) {
    stop("This fit does not contain sampler diagnostics.", call. = FALSE)
  }
  if (inc_warmup) {
    if (is.null(private$warmup_diagnostics_)) {
      stop(
        "warmup draws were not saved; only `$sample()` runs with ",
        "`save_warmup = TRUE` store them.",
        call. = FALSE
      )
    }
    diagnostics <- posterior::bind_draws(
      private$warmup_diagnostics_,
      diagnostics,
      along = "iteration"
    )
  }
  .stanr_as_draws_format(diagnostics, format)
}
StanMCMC$set("public", "sampler_diagnostics", mcmc_sampler_diagnostics)

mcmc_num_chains <- function() {
  private$metadata_$chains %||%
    posterior::nchains(private$draws_)
}
StanMCMC$set("public", "num_chains", mcmc_num_chains)

mcmc_diagnostic_summary <- function(
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  quiet = FALSE
) {
  quiet <- .stanr_flag(quiet, "quiet")
  if (!length(diagnostics) || identical(diagnostics, "")) {
    return(list())
  }
  diagnostics <- match.arg(
    diagnostics,
    choices = c("divergences", "treedepth", "ebfmi"),
    several.ok = TRUE
  )

  draws <- self$sampler_diagnostics(format = "draws_array")
  n_chains <- self$num_chains()
  vars <- posterior::variables(draws)
  n_draws <- posterior::niterations(draws) * n_chains
  # `fixed_param` runs store a single dummy (1 iteration x 1 chain) all-NA
  # placeholder for diagnostics regardless of how many chains were actually
  # sampled (see the `fixed_param` branch of `stan_model_sample()`'s
  # `payload_fn`), so the chain dimension can't always be indexed up to
  # `n_chains`; treat that mismatch the same as "diagnostic not collected"
  # and stay silent -- there's nothing wrong to warn about, just nothing to
  # check.
  collected <- posterior::nchains(draws) == n_chains

  out <- list()

  if ("divergences" %in% diagnostics) {
    num_divergent <- if (collected && "divergent__" %in% vars) {
      vapply(
        seq_len(n_chains),
        function(i) sum(draws[, i, "divergent__"] > 0),
        integer(1)
      )
    } else {
      rep(NA_integer_, n_chains)
    }
    if (!quiet) {
      .stanr_check_divergences(num_divergent, n_draws)
    }
    out$num_divergent <- num_divergent
  }

  if ("treedepth" %in% diagnostics) {
    max_depth <- private$metadata_$arguments$max_depth %||% 10L
    num_max_treedepth <- if (collected && "treedepth__" %in% vars) {
      vapply(
        seq_len(n_chains),
        function(i) sum(draws[, i, "treedepth__"] >= max_depth),
        integer(1)
      )
    } else {
      rep(NA_integer_, n_chains)
    }
    if (!quiet) {
      .stanr_check_max_treedepth(num_max_treedepth, n_draws, max_depth)
    }
    out$num_max_treedepth <- num_max_treedepth
  }

  if ("ebfmi" %in% diagnostics) {
    out$ebfmi <- if (collected && "energy__" %in% vars) {
      .stanr_check_ebfmi(draws, n_chains, quiet)
    } else {
      rep(NA_real_, n_chains)
    }
  }

  out
}
StanMCMC$set("public", "diagnostic_summary", mcmc_diagnostic_summary)

mcmc_inv_metric <- function(matrix = TRUE) {
  matrix <- .stanr_flag(matrix, "matrix")
  metric <- private$inv_metric_
  if (is.null(metric)) {
    stop(
      "no adapted metric is available; the metric is only captured when ",
      "sampling runs with adaptation",
      call. = FALSE
    )
  }
  dense <- is.matrix(metric[[1]])
  if (dense) {
    if (!matrix) {
      stop(
        "`matrix = FALSE` is only available for diagonal metrics.",
        call. = FALSE
      )
    }
    return(metric)
  }
  if (matrix) {
    lapply(metric, function(v) diag(v, nrow = length(v)))
  } else {
    metric
  }
}
StanMCMC$set("public", "inv_metric", mcmc_inv_metric)

#' Leave-one-out cross-validation (LOO-CV)
#'
#' @name fit-method-loo
#' @aliases loo
#' @family StanFit methods
#'
#' @description The `$loo()` method computes approximate LOO-CV using the
#'   \pkg{loo} package. In order to use this method you must compute and save
#'   the pointwise log-likelihood in your Stan program. See [loo::loo.array()]
#'   and the \pkg{loo} package [vignettes](https://mc-stan.org/loo/articles/)
#'   for details.
#'
#' @param variables (string) The name of the variable in the Stan program
#'   containing the pointwise log-likelihood. The default is to look for
#'   `"log_lik"`. This argument is passed to the [`$draws()`][fit-method-draws]
#'   method.
#' @param r_eff (multiple options) How to handle the `r_eff` argument for
#'   `loo()`. `r_eff` measures the amount of autocorrelation in MCMC draws, and
#'   is used to compute more accurate ESS and MCSE estimates for pointwise and
#'   total ELPDs.
#'   * `TRUE` will call [loo::relative_eff()] to compute the `r_eff`
#'   argument to pass to [loo::loo.array()].
#'   * `FALSE` (the default) or `NULL` will avoid computing `r_eff`,
#'   which can be very slow. The reported ESS and MCSE estimates may be
#'   over-optimistic if the posterior draws are far from independent.
#'   * If `r_eff` is anything else, that object will be passed as the `r_eff`
#'   argument to [loo::loo.array()].
#' @param moment_match (logical) Whether to use a
#'   [moment-matching][loo::loo_moment_match()] correction for problematic
#'   observations. The default is `FALSE`. Using `moment_match = TRUE` uses
#'   the fit's model methods (see [fit-method-model-methods]) to automatically
#'   supply the functions for the `log_lik_i`, `unconstrain_pars`,
#'   `log_prob_upars`, and `log_lik_i_upars` arguments to
#'   [loo::loo_moment_match()].
#' @param ... Other arguments (e.g., `cores`, `save_psis`, etc.) passed to
#'   [loo::loo.array()] or [loo::loo_moment_match.default()]
#'   (if `moment_match = TRUE` is set).
#'
#' @return The object returned by [loo::loo.array()] or
#'   [loo::loo_moment_match.default()].
#'
#' @references
#' * Vehtari, A., Gelman, A., and Gabry, J. (2017). Practical Bayesian model
#'   evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing*, 27(5), 1413-1432.
#'   doi:10.1007/s11222-016-9696-4.
#' * Vehtari, A., Simpson, D., Gelman, A., Yao, Y., and Gabry, J. (2024).
#'   Pareto smoothed importance sampling.
#'   *Journal of Machine Learning Research*, 25(72), 1-58.
#' * Paananen, T., Piironen, J., Buerkner, P.-C., and Vehtari, A. (2021).
#'   Implicitly adaptive importance sampling.
#'   *Statistics and Computing*, 31, 16. doi:10.1007/s11222-020-09982-2
#'   (for `moment_match = TRUE`).
#'
#' @seealso The \pkg{loo} package website with
#'   [documentation](https://mc-stan.org/loo/reference/index.html) and
#'   [vignettes](https://mc-stan.org/loo/articles/).
#'
NULL

fit_loo <- function(
  variables = "log_lik",
  r_eff = FALSE,
  moment_match = FALSE,
  ...
) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The `loo` package is required!", call. = FALSE)
  }
  moment_match <- .stanr_flag(moment_match, "moment_match")
  if (length(variables) != 1) {
    stop(
      "Only a single variable name is allowed for the 'variables' argument.",
      call. = FALSE
    )
  }
  LLarray <- self$draws(variables, format = "draws_array")
  if (is.logical(r_eff)) {
    if (isTRUE(r_eff)) {
      r_eff_cores <- getOption("mc.cores", 1)
      r_eff <- loo::relative_eff(
        exp(sweep(LLarray, 3, apply(LLarray, 3, max), FUN = "-")),
        cores = r_eff_cores
      )
    } else {
      r_eff <- NULL
    }
  }

  if (moment_match) {
    loo_result <- suppressWarnings(loo::loo.array(LLarray, r_eff = r_eff, ...))

    log_lik_i <- function(x, i, parameter_name = "log_lik", ...) {
      ll_array <- x$draws(variables = parameter_name, format = "draws_array")[,,
        i
      ]
      attr(ll_array, "dim") <- attributes(ll_array)$dim[1:2]
      ll_array
    }

    # One batched native call per observation instead of a per-draw
    # `constrain_variables()` loop (which re-lists the full variable
    # structure for every draw).
    log_lik_i_upars <- function(x, upars, i, parameter_name = "log_lik", ...) {
      private$ensure_native()
      constrained <- private$model_$native_function("model_constrain_matrix")(
        private$model_ptr_,
        private$rng_ptr_,
        upars,
        TRUE,
        TRUE
      )
      colnames(constrained) <- .stanr_bracket_names(colnames(constrained))
      target <- paste0(parameter_name, "[", i, "]")
      # A scalar log-lik variable has no bracketed element name.
      if (!target %in% colnames(constrained)) {
        target <- parameter_name
      }
      constrained[, target]
    }

    loo::loo_moment_match.default(
      x = self,
      loo = loo_result,
      post_draws = function(x, ...) {
        x$draws(format = "draws_matrix")
      },
      log_lik_i = log_lik_i,
      unconstrain_pars = function(x, pars, ...) {
        x$unconstrain_draws(format = "draws_matrix")
      },
      log_prob_upars = function(x, upars, ...) {
        apply(upars, 1, x$log_prob)
      },
      log_lik_i_upars = log_lik_i_upars,
      ...
    )
  } else {
    loo::loo.array(LLarray, r_eff = r_eff, ...)
  }
}
StanMCMC$set("public", "loo", fit_loo)

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
        par <- par[!(names(par) %in% c("lp__", "converged__"))]
        names(par) <- .stanr_bracket_names(names(par))
      }
      payload$par <- par
      if (!is.null(payload$iterations)) {
        # Full optimization path (one row per saved iteration): expose it via
        # $draws() instead of the single synthesized row below.
        iterations <- payload$iterations
        iterations <- iterations[,
          colnames(iterations) != "converged__",
          drop = FALSE
        ]
        colnames(iterations) <- .stanr_bracket_names(colnames(iterations))
        payload$draws <- iterations
      } else if (length(par)) {
        payload$draws <- matrix(
          c(payload$value %||% NA_real_, unname(par)),
          nrow = 1L,
          dimnames = list(NULL, c("lp__", names(par)))
        )
      }
      # Every other fit type's draws arrive already `posterior`-classed;
      # this one is assembled by hand above, so `$draws(variables = ...)`
      # (via `subset_draws()`) needs it classed too.
      if (!is.null(payload$draws)) {
        payload$draws <- posterior::as_draws_matrix(payload$draws)
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
#' @return A named numeric vector of parameter estimates. Always reflects the
#'   final optimization iteration, regardless of whether
#'   [`$optimize()`][model-method-optimize] was run with `save_iterations =
#'   TRUE`.
#'
#' @seealso [`$draws()`][fit-method-draws], which under
#'   `save_iterations = TRUE` returns the full optimization path (one row per
#'   saved iteration) instead of a single row for the final estimate.
#'
NULL

mle_mle <- function(variables = NULL) {
  value <- private$par_ %||% numeric()
  if (!is.null(variables)) {
    missing <- setdiff(variables, names(value))
    if (length(missing)) {
      stop(
        "Unknown variable(s): ",
        paste(missing, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
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

# `mle_summary()` is O(n) and its variable domain excludes `lp__` (present in
# `draws_`), so the base print's draws-derived variable subsetting neither
# applies nor helps here.
mle_print <- function(variables = NULL, ..., digits = 2, max_rows = 20) {
  out <- self$summary(variables = variables, ...)
  print(utils::head(out, max_rows), digits = digits)
  invisible(self)
}
StanMLE$set("public", "print", mle_print)

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
#'  [`$lp_approx()`][fit-method-lp] | Extract the log density approximation draws. |
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
      payload$draws <- .stanr_rename_draw_columns(payload$draws)
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
StanLaplace$set("public", "lp_approx", fit_lp_approx)

# StanVB class -----------------------------------------------------------------

#' StanVB objects
#'
#' @name StanVB
#' @description A `StanVB` object is returned by
#'   [`$variational()`][model-method-variational] and contains approximate
#'   posterior draws from Automatic Differentiation Variational Inference (ADVI).
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanVB` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$lp_approx()`][fit-method-lp] | Extract the log density approximation draws. |
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
      draws <- payload$draws
      draws <- .stanr_rename_draw_columns(draws)
      requested <- payload$args$output_samples
      # ADVI writes the posterior mean as the first row before the requested
      # draws; drop it when present so draw counts match what was requested.
      if (
        !is.null(draws) && !is.null(requested) && nrow(draws) == requested + 1L
      ) {
        draws <- draws[-1L, , drop = FALSE]
      }
      # The ADVI writer emits an `lp__` column of placeholder zeros/NAs
      # (variational inference doesn't track a per-draw log density); drop it
      # rather than presenting a meaningless constant column.
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
StanVB$set("public", "lp_approx", fit_lp_approx)

# StanPathfinder class ---------------------------------------------------------

#' StanPathfinder objects
#'
#' @name StanPathfinder
#' @description A `StanPathfinder` object is returned by
#'   [`$pathfinder()`][model-method-pathfinder] and contains approximate
#'   posterior draws from the Pathfinder algorithm.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanPathfinder` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$lp_approx()`][fit-method-lp] | Extract the log density approximation draws. |
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
      payload$draws <- .stanr_rename_draw_columns(payload$draws)
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
StanPathfinder$set("public", "lp_approx", fit_lp_approx)

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
#'  [`$lp()`][fit-method-diagnose] | Return the log probability evaluated at the
#'   initial parameter values. |
#'  [`$num_failed()`][fit-method-diagnose] | Return the number of failed gradient checks. |
#'
NULL

StanDiagnose <- R6Class(
  "StanDiagnose",
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
      private$gradients_ <- payload$gradients %||% data.frame()
      private$lp_ <- payload$lp %||% NA_real_
      private$num_failed_ <- as.integer(payload$num_failed %||% NA_integer_)
      metadata$num_failed <- private$num_failed_
      super$initialize(
        list(
          return_code = payload$return_code %||% 0L,
          output = payload$output %||% character(),
          model_ptr = payload$model_ptr
        ),
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
  private = list(
    gradients_ = NULL,
    lp_ = NULL,
    num_failed_ = NULL
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
#'   lp()
#'   num_failed()
#'   ```
#'
#' @return
#' * `$gradients()` returns a data frame with gradient check results for each
#'   parameter, including columns `param_idx`, `value`, `model`, `finite_diff`,
#'   and `error`.
#' * `$lp()` returns the log probability evaluated at the initial parameter
#'   values.
#' * `$num_failed()` returns the number of parameters that failed the gradient
#'   check.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

diagnose_gradients <- function() private$gradients_
StanDiagnose$set("public", "gradients", diagnose_gradients)

diagnose_lp <- function() as.numeric(private$lp_)
StanDiagnose$set("public", "lp", diagnose_lp)

diagnose_num_failed <- function() private$num_failed_
StanDiagnose$set("public", "num_failed", diagnose_num_failed)
