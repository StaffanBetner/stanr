#' Check model gradients via finite differences
#'
#' Verifies that Stan's generated gradients match finite-difference approximations.
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param epsilon Finite difference step size (default: 1e-6).
#' @param error Error threshold for comparison (default: 1e-6).
#' @param seed Random seed (NA = random).
#' @param init Initial values (numeric vector, or `"random"`).
#' @param init_radius Initialization radius (default: 2).
#' @param verbose Print progress (default: TRUE).
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
  init = 0,
  init_radius = 2,
  verbose = TRUE,
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(runif(1, 1, 2^31 - 1))
  }

  args <- list(
    method = "diagnose",
    epsilon = as.double(epsilon),
    error = as.double(error),
    seed = as.integer(seed),
    chain_id = 1L,
    init_radius = as.double(init_radius),
    verbose = as.logical(verbose),
    data = data,
    init = if (is.list(init)) init else list()
  )

  dat_ptr <- .Call(`r_data_context`, data)
  mod_ptr <- stanmod$new_model(dat_ptr, seed)
  withr::with_envvar(
    c(STAN_NUM_THREADS = 4),
    result <- .Call(`newstan_run`, mod_ptr, args)
  )

  n_failed <- as.integer(result$return_code)

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
