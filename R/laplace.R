# Internal helper for Laplace approximation.
# Accepts pre-normalized arguments from .newstan_normalize_laplace().
# The `mode` value (constrained parameter vector) is passed separately
# since it may come from an optimization result or user input.

#' @noRd
.newstan_run_laplace <- function(stanmod, args, mode) {
  # Extract constrained parameter vector from mode result if needed
  if (is.list(mode) && !is.null(mode$par)) {
    mode <- mode$par
  }
  if (!is.numeric(mode) || is.null(names(mode))) {
    stop("mode must be a named numeric vector or an optimization result.",
         call. = FALSE)
  }

  model_instance <- new_model_instance(stanmod, args$data, args$seed)
  pars <- stanmod$constrained_param_names(model_instance$model)
  mode <- mode[pars]
  if (anyNA(mode)) {
    stop("mode must contain every constrained model parameter.", call. = FALSE)
  }

  native_args <- list(
    method = "laplace",
    mode = as.double(mode),
    jacobian = as.logical(args$jacobian),
    draws = as.integer(args$draws),
    calculate_lp = as.logical(args$calculate_lp),
    seed = as.integer(args$seed),
    refresh = as.integer(args$refresh),
    verbose = as.logical(args$show_messages),
    num_threads = as.integer(args$threads)
  )

  withr::with_envvar(
    c(STAN_NUM_THREADS = args$threads),
    result <- stanmod$run_model(model_instance$model, native_args)
  )

  structure(list(
    draws = posterior::as_draws_df(result$draws),
    return_code = result$return_code,
    args = service_args(native_args)
  ), class = c("StanLaplace", "StanService", "list"))
}
