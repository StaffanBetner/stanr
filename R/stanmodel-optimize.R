# StanModel $optimize() method -------------------------------------------------

#' Run optimization
#'
#' @name model-method-optimize
#' @aliases optimize
#' @family StanModel methods
#'
#' @description The `$optimize()` method of a [`StanModel`] object runs Stan's
#'   `"optimize"` method to find the maximum a posteriori (MAP) estimate or
#'   maximum likelihood estimate (MLE). Returns a [`StanMLE`] object.
#'
#' @template param-data
#' @template param-seed
#' @template param-init
#' @param algorithm (string) The optimization algorithm: `"lbfgs"`, `"bfgs"`,
#'   or `"newton"`.
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacobian of the inverse transformation?
#' @param init_alpha (number) Initial step size for LBFGS.
#' @param iter (integer) Maximum number of optimization iterations.
#' @param tol_obj (number) Absolute tolerance for changes in objective value.
#' @param tol_rel_obj (number) Relative tolerance for changes in objective value.
#' @param tol_grad (number) Absolute tolerance for the norm of the gradient.
#' @param tol_rel_grad (number) Relative tolerance for the norm of the gradient.
#' @param tol_param (number) Absolute tolerance for changes in parameter values.
#' @param history_size (integer) Number of corrections in LBFGS approximation.
#' @param save_iterations (logical) Should optimization iterations be saved?
#'   When `TRUE`, [`$draws()`][fit-method-draws] on the returned [`StanMLE`]
#'   exposes the full optimization path (one row per saved iteration,
#'   including the initial point) instead of a single row for the final
#'   estimate. [`$mle()`][fit-method-mle] is unaffected either way and always
#'   reflects the final iteration.
#' @template param-num_threads
#' @template param-show_messages
#' @template param-show_exceptions
#' @template param-opencl_ids
#'
#' @return A [`StanMLE`] object containing the point estimate.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$laplace()`][model-method-laplace]
#'
NULL

stan_model_optimize_impl <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  algorithm = "lbfgs",
  jacobian = FALSE,
  init_alpha = 0.001,
  iter = 2000L,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  history_size = 5L,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_iterations = FALSE
) {
  jacobian <- .stanr_flag(jacobian, "jacobian")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  save_iterations <- .stanr_flag(save_iterations, "save_iterations")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  if (!algorithm %in% c("lbfgs", "bfgs", "newton")) {
    stop(
      "`algorithm` must be one of \"lbfgs\", \"bfgs\", \"newton\".",
      call. = FALSE
    )
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  refresh <- .stanr_int(refresh, "refresh")
  iter <- .stanr_int(iter, "iter")
  history_size <- .stanr_int(history_size, "history_size", min = 1L)

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "optimize",
      algorithm = algorithm,
      seed = seed,
      id = 1L,
      init_radius = resolved_init$radius,
      iter = iter,
      init_alpha = as.double(init_alpha),
      tol_obj = as.double(tol_obj),
      tol_rel_obj = as.double(tol_rel_obj),
      tol_grad = as.double(tol_grad),
      tol_rel_grad = as.double(tol_rel_grad),
      tol_param = as.double(tol_param),
      history_size = history_size,
      save_iterations = save_iterations,
      jacobian = jacobian,
      refresh = refresh,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    par_mat <- result$par
    par_vec <- if (is.matrix(par_mat) && nrow(par_mat) > 0) {
      par_mat[nrow(par_mat), , drop = TRUE]
    } else {
      numeric(0)
    }
    payload <- list(par = par_vec, value = result$value)
    if (save_iterations) {
      payload$iterations <- par_mat
    }
    payload
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanMLE$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "optimize",
      jacobian = jacobian,
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
