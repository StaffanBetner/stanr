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
    },

    draws = function(variables = NULL, inc_warmup = FALSE, format = NULL) {
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
    },

    summary = function(variables = NULL, ...) {
      posterior::summarise_draws(
        self$draws(variables = variables, format = private$default_format_),
        ...
      )
    },

    print = function(variables = NULL, ..., digits = 2, max_rows = 20) {
      if (is.null(variables) && !is.null(private$draws_)) {
        variables <- utils::head(posterior::variables(private$draws_), max_rows)
      }
      out <- self$summary(variables = variables, ...)
      print(utils::head(out, max_rows), digits = digits)
      invisible(self)
    },

    return_codes = function() private$return_codes_,

    metadata = function() {
      seed_pos <- which(names(private$metadata_) == "seed")
      after <- if (length(seed_pos)) seed_pos[[1]] else 0L
      append(private$metadata_, list(data = private$data_), after = after)
    },

    output = function() private$output_,

    time = function() list(total = private$elapsed_),

    init = function() private$init_,

    code = function() {
      if (!inherits(private$model_, "StanModel")) {
        character()
      } else {
        private$model_$code()
      }
    },

    lp = function() {
      as.numeric(self$draws(variables = "lp__", format = "draws_matrix"))
    },

    save_object = function(file, ...) {
      saveRDS(self, file = file, ...)
      invisible(normalizePath(file, mustWork = TRUE))
    },

    log_prob = function(unconstrained_variables, jacobian = TRUE) {
      private$check_jacobian(jacobian)
      as.numeric(private$native_call(
        "model_log_prob",
        as.double(unconstrained_variables),
        jacobian
      ))
    },

    grad_log_prob = function(unconstrained_variables, jacobian = TRUE) {
      private$check_jacobian(jacobian)
      private$native_call(
        "model_grad_log_prob",
        as.double(unconstrained_variables),
        jacobian
      )
    },

    hessian = function(unconstrained_variables, jacobian = TRUE) {
      private$check_jacobian(jacobian)
      private$native_call(
        "model_hessian",
        as.double(unconstrained_variables),
        jacobian
      )
    },

    constrain_variables = function(
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
    },

    unconstrain_variables = function(variables) {
      if (!is.list(variables) || is.null(names(variables))) {
        stop(
          "`variables` must be a named list of constrained values.",
          call. = FALSE
        )
      }
      if (!inherits(private$model_, "StanModel")) {
        stop("This fit does not retain a model binding.", call. = FALSE)
      }
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
    },

    unconstrain_draws = function(
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
    },

    variable_skeleton = function(
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
    },

    expose_stan_functions = function(global = FALSE, verbose = FALSE) {
      if (!inherits(private$model_, "StanModel")) {
        stop("This fit does not retain a model binding.", call. = FALSE)
      }
      private$model_$expose_stan_functions(global, verbose)
    },

    expose_functions = function(global = FALSE, verbose = FALSE) {
      self$expose_stan_functions(global, verbose)
    }
  ),
  active = list(
    functions = function(value) {
      if (!missing(value)) {
        stop("`$functions` is read-only.", call. = FALSE)
      }
      if (!inherits(private$model_, "StanModel")) {
        stop("This fit does not retain a model binding.", call. = FALSE)
      }
      private$model_$functions
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
      if (
        !is.na(private$native_generation_) &&
          private$native_generation_ == private$model_$compile_generation() &&
          !.stanr_xptr_is_null(private$model_ptr_)
      ) {
        return(invisible(NULL))
      }
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

# StanFit method documentation -------------------------------------------------

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
#' @template param-variables
#' @template param-inc_warmup
#' @template param-format
#'
#' @return An object from the \pkg{posterior} package in the requested format.
#'
#' @seealso [`$summary()`][fit-method-summary]
#'
NULL

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
#' @template param-format
#' @template param-inc_warmup
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
