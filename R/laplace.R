# The `mode` value (constrained parameter vector) is passed in already
# resolved by stan_model_laplace(), since it may come from an internal
# optimization run, a user-supplied StanMLE, or a raw numeric vector.

#' @noRd
.newstan_run_laplace <- function(
  stanmod,
  mode,
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  threads = NULL,
  jacobian = TRUE,
  draws = NULL,
  calculate_lp = TRUE,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  common <- .newstan_normalize_common(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  def <- .newstan_defaults$laplace
  threads <- as.integer(threads %||% 1L)

  # Extract constrained parameter vector from mode result if needed
  if (is.list(mode) && !is.null(mode$par)) {
    mode <- mode$par
  }
  if (!is.numeric(mode) || is.null(names(mode))) {
    stop(
      "mode must be a named numeric vector or an optimization result.",
      call. = FALSE
    )
  }

  model <- stanmod$new_model(common$data, common$seed)
  pars <- stanmod$constrained_param_names(model)
  mode <- mode[pars]
  if (anyNA(mode)) {
    stop("mode must contain every constrained model parameter.", call. = FALSE)
  }

  native_args <- list(
    method = "laplace",
    mode = as.double(mode),
    jacobian = as.logical(jacobian),
    draws = as.integer(draws %||% def$draws),
    calculate_lp = as.logical(calculate_lp),
    seed = as.integer(common$seed),
    refresh = as.integer(common$refresh),
    verbose = as.logical(common$show_messages),
    num_threads = threads
  )

  withr::with_envvar(
    c(STAN_NUM_THREADS = threads),
    result <- stanmod$run_model(model, native_args)
  )

  structure(
    list(
      draws = posterior::as_draws_df(result$draws),
      return_code = result$return_code,
      args = service_args(native_args),
      output = result$output %||% character(),
      model_ptr = model
    ),
    class = c("StanLaplace", "StanService", "list")
  )
}
