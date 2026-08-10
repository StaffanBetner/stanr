# StanModel $diagnose() method
#' Run gradient diagnostics
#'
#' @name model-method-diagnose
#' @aliases diagnose
#' @family StanModel methods
#'
#' @description The `$diagnose()` method of a [`StanModel`] object runs Stan's
#'   `"diagnose"` method to check the correctness of gradients computed by
#'   Stan. Returns a [`StanDiagnose`] object.
#'
#' @template param-data
#' @template param-seed
#' @template param-init
#' @param epsilon (number) The finite difference step size.
#' @param error (number) The maximum allowed relative error.
#' @template param-show_messages
#' @template param-show_exceptions
#'
#' @return A [`StanDiagnose`] object containing gradient check results.
#'
#' @seealso [`$sample()`][model-method-sample]
#'
NULL

stan_model_diagnose_impl <- function(
  data = list(),
  seed = NULL,
  init = 2,
  epsilon = 1e-6,
  error = 1e-6,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")

  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0) {
    stop("`epsilon` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(error) || length(error) != 1L || error <= 0) {
    stop("`error` must be a positive number.", call. = FALSE)
  }

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "diagnose",
      epsilon = as.double(epsilon),
      error = as.double(error),
      seed = as.integer(seed),
      id = 1L,
      init_radius = resolved_init$radius,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = 1L,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    list(
      num_failed = as.integer(result$num_failed),
      gradients = data.frame(
        param_idx = seq_along(result$value) - 1L,
        value = result$value,
        model = result$model,
        finite_diff = result$finite_diff,
        error = result$error,
        check.names = FALSE
      ),
      lp = result$lp
    )
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  if (res$payload$num_failed == 0L) {
    message("[stanr] All gradient tests passed.")
  } else {
    message(sprintf(
      "[stanr] %d parameter(s) failed the gradient test.",
      res$payload$num_failed
    ))
  }

  StanDiagnose$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "diagnose",
      epsilon = epsilon,
      error = error
    )
  )
}
