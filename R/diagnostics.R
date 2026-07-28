#' Check model gradients via finite differences
#'
#' Verifies that Stan's generated gradients match finite-difference approximations.
#'
#' @param object A `newstan_fit` object
#' @param epsilon Finite difference step size (default: 1e-6)
#' @param error Error threshold for comparison (default: 1e-6)
#' @param seed Random seed
#' @param init Initial values
#' @param init_radius Initialization radius (default: 2)
#' @param verbose Print progress (default: TRUE)
#' @param ... Unused
#'
#' @return An integer: number of parameters that failed the gradient test (0 = all pass).
#'
#' @export
gradient_check <- function(object,
                           epsilon = 1e-6,
                           error = 1e-6,
                           seed = NA,
                           init = 0,
                           init_radius = 2,
                           verbose = TRUE,
                           ...) {
  if (!inherits(object, "newstan_fit")) {
    stop("'object' must be a newstan_fit object.")
  }

  if (is.na(seed)) seed <- as.integer(runif(1, 1, 2^31 - 1))

  args <- list(
    method = "diagnose",
    epsilon = as.double(epsilon),
    error = as.double(error),
    seed = as.integer(seed),
    chain_id = 1L,
    init_radius = as.double(init_radius),
    verbose = as.logical(verbose),
    data = .prepare_data(object$data),
    init = .prepare_init(init, object)
  )

  dll_handle <- object$dll
  result <- .Call(dll_handle[["newstan_run"]], object$dll_ptr, args)

  n_failed <- as.integer(result$return_code)

  if (n_failed == 0L) {
    message("[newstan] All gradient tests passed.")
  } else {
    message(sprintf("[newstan] %d parameter(s) failed the gradient test.", n_failed))
  }

  return(n_failed)
}
