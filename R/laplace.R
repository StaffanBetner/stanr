#' Draw from a Laplace approximation
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param mode A named numeric vector of constrained parameter values, or an
#'   `optimizing()` result.
#' @param jacobian Include the change-of-variables adjustment (default: TRUE).
#' @param draws Number of draws from the approximation (default: 1000).
#' @param calculate_lp Calculate the log probability at each draw (default: TRUE).
#' @param seed Random seed (NA = random).
#' @param refresh Output refresh frequency (default: 100).
#' @param verbose Print progress (default: TRUE).
#' @param num_threads Number of threads, or `-1` for all available threads.
#' @param ... Unused.
#'
#' @return A list containing Laplace draws, a return code, and the arguments.
#'
#' @export
laplace <- function(
  stanmod,
  data,
  mode,
  jacobian = TRUE,
  draws = 1000,
  calculate_lp = TRUE,
  seed = NA,
  refresh = 100,
  verbose = TRUE,
  num_threads = -1,
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(stats::runif(1, 1, 2^31 - 1))
  }
  if (is.list(mode) && !is.null(mode$par)) {
    mode <- mode$par
  }
  if (!is.numeric(mode) || is.null(names(mode))) {
    stop("mode must be a named numeric vector or an optimizing() result.",
         call. = FALSE)
  }

  model_instance <- new_model_instance(stanmod, data, seed)
  pars <- stanmod$constrained_param_names(model_instance$model)
  mode <- mode[pars]
  if (anyNA(mode)) {
    stop("mode must contain every constrained model parameter.", call. = FALSE)
  }

  args <- list(
    method = "laplace",
    mode = as.double(mode),
    jacobian = as.logical(jacobian),
    draws = as.integer(draws),
    calculate_lp = as.logical(calculate_lp),
    seed = as.integer(seed),
    refresh = as.integer(refresh),
    verbose = as.logical(verbose),
    num_threads = as.integer(num_threads)
  )

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, args)
  )

  structure(list(
    draws = posterior::as_draws_df(result$draws),
    return_code = result$return_code,
    args = service_args(args)
  ), class = c("StanLaplace", "StanService", "list"))
}
