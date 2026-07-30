#' Check model gradients via finite differences
#'
#' Verifies that Stan's generated gradients match finite-difference approximations.
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param epsilon Finite difference step size (default: 1e-6).
#' @param error Error threshold for comparison (default: 1e-6).
#' @param seed Random seed (NA = random).
#' @param id Chain ID for RNG advancement (default: 1).
#' @param init Initialization radius, or named constrained initial values
#'   (default: 2).
#' @param verbose Print progress (default: TRUE).
#' @param num_threads Number of threads, or `-1` for all available threads.
#' @param ... Unused.
#'
#' @return An integer: number of parameters that failed the gradient test
#'   (0 = all pass).
#'
#' @export
gradient_check <- function(
  stanmod,
  data,
  epsilon = 1e-6,
  error = 1e-6,
  seed = NA,
  id = 1,
  init = 2,
  verbose = TRUE,
  num_threads = -1,
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(stats::runif(1, 1, 2^31 - 1))
  }

  args <- list(
    method = "diagnose",
    epsilon = as.double(epsilon),
    error = as.double(error),
    seed = as.integer(seed),
    id = as.integer(id),
    init_radius = init_radius(init),
    verbose = as.logical(verbose),
    data = data,
    init = normalize_init(init)
  )

  model_instance <- new_model_instance(stanmod, data, seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, args)
  )

  n_failed <- as.integer(result$num_failed)

  if (n_failed == 0L) {
    message("[newstan] All gradient tests passed.")
  } else {
    message(sprintf(
      "[newstan] %d parameter(s) failed the gradient test.",
      n_failed
    ))
  }

  return(n_failed)
}
