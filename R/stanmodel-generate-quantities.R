# StanModel $generate_quantities() method --------------------------------------

#' Run generated quantities
#'
#' @name model-method-generate-quantities
#' @aliases generate_quantities
#' @family StanModel methods
#'
#' @description The `$generate_quantities()` method of a [`StanModel`] object
#'   runs Stan's `"generate quantities"` method, which evaluates the generated
#'   quantities block of the Stan program for a set of parameter values.
#'   Returns a [`StanGQ`] object.
#'
#' @param fitted_params A [`StanFit`] object, or anything accepted by
#'   [posterior::as_draws_matrix()], containing draws with every model
#'   parameter present by name (e.g. `beta[1]`, not a positional column).
#' @template param-data
#' @template param-seed
#' @template param-num_threads
#' @template param-show_messages
#' @template param-show_exceptions
#' @template param-opencl_ids
#'
#' @return A [`StanGQ`] object containing the generated quantities.
#'
#' @seealso [`$sample()`][model-method-sample]
#'
NULL

stan_model_generate_quantities_impl <- function(
  fitted_params,
  data = list(),
  seed = NULL,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  common <- .stanr_common_service_flags(
    show_messages,
    show_exceptions,
    opencl_ids,
    private,
    num_threads
  )
  show_messages <- common$show_messages
  show_exceptions <- common$show_exceptions
  num_threads <- common$num_threads

  input <- if (inherits(fitted_params, "StanFit")) {
    fitted_params$draws(format = "draws_matrix")
  } else {
    posterior::as_draws_matrix(fitted_params)
  }
  nchains_input <- posterior::nchains(input)

  native_args_fn <- function(seed, resolved_init, model) {
    pars <- .stanr_bracket_names(self$constrained_param_names(model))
    draws_matrix <- posterior::as_draws_matrix(
      posterior::subset_draws(input, variable = pars)
    )

    list(
      method = "generate_quantities",
      seed = seed,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      nchains = nchains_input,
      draws = draws_matrix
    )
  }

  payload_fn <- function(result) {
    list(draws = result$samples)
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = NULL,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanGQ$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = NULL,
    elapsed = res$elapsed,
    metadata = list(
      method = "generate_quantities",
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
