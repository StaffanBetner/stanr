#' Laplace approximation sampling
#'
#' Approximates the posterior via Laplace approximation (Gaussian centered at the mode)
#' and draws from it.
#'
#' @param object A `newstan_fit` object
#' @param fit An `newstan_optim` result providing the mode (from [optimizing()])
#' @param draws Number of draws from the Laplace approximation (default: 2000)
#' @param seed Random seed
#' @param verbose Print progress (default: FALSE)
#' @param ... Unused
#'
#' @return An S3 object of class `"newstan_laplace"` with draws from the approximation.
#'
#' @export
laplace <- function(
  object,
  fit,
  draws = 2000,
  seed = NA,
  verbose = FALSE,
  ...
) {
  if (!inherits(object, "newstan_fit")) {
    stop("'object' must be a newstan_fit object.")
  }
  if (!inherits(fit, "newstan_optim")) {
    stop("'fit' must be a newstan_optim result from optimizing().")
  }

  if (is.na(seed)) {
    seed <- as.integer(runif(1, 1, 2^31 - 1))
  }

  # TODO: Implement Laplace approximation
  # This requires the mode from optimization and Hessian computation
  stop("Laplace approximation not yet implemented.")
}
