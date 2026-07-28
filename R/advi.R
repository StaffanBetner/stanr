#' Run ADVI (Automatic Differentiable Variational Inference)
#'
#' @param object A `newstan_fit` object
#' @param algorithm Variational family: `"fullrank"` or `"meanfield"` (default: `"fullrank"`)
#' @param iter Maximum iterations (default: 10000)
#' @param grad_samples MC samples for gradient estimate (default: 1)
#' @param elbo_samples MC samples for ELBO estimate (default: 100)
#' @param tol_rel_obj Convergence tolerance (default: 0.01)
#' @param eta Stepping parameter (default: 1.0)
#' @param adapt_engaged Enable eta adaptation (default: TRUE)
#' @param adapt_iter Adaptation iterations (default: 50)
#' @param eval_elbo Evaluate ELBO every Nth iteration (default: 100)
#' @param output_samples Posterior samples to draw (default: 1000)
#' @param seed Random seed
#' @param init Initial values
#' @param init_radius Initialization radius (default: 2)
#' @param verbose Print progress (default: TRUE)
#' @param ... Unused
#'
#' @return An S3 object of class `"newstan_advi"` with variational approximation results.
#'
#' @export
advi <- function(object,
                 algorithm = "fullrank",
                 iter = 10000,
                 grad_samples = 1,
                 elbo_samples = 100,
                 tol_rel_obj = 0.01,
                 eta = 1.0,
                 adapt_engaged = TRUE,
                 adapt_iter = 50,
                 eval_elbo = 100,
                 output_samples = 1000,
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
    method = "advi",
    algorithm = algorithm,
    seed = as.integer(seed),
    chain_id = 1L,
    init_radius = as.double(init_radius),
    iter = as.integer(iter),
    grad_samples = as.integer(grad_samples),
    elbo_samples = as.integer(elbo_samples),
    tol_rel_obj = as.double(tol_rel_obj),
    eta = as.double(eta),
    adapt_engaged = as.logical(adapt_engaged),
    adapt_iter = as.integer(adapt_iter),
    eval_elbo = as.integer(eval_elbo),
    output_samples = as.integer(output_samples),
    verbose = as.logical(verbose),
    data = .prepare_data(object$data),
    init = .prepare_init(init, object)
  )

  dll_handle <- object$dll
  result <- .Call(dll_handle[["newstan_run"]], object$dll_ptr, args)

  structure(
    list(
      return_code = result$return_code,
      method = "advi",
      algorithm = algorithm,
      args = args
    ),
    class = "newstan_advi"
  )
}

#' @export
print.newstan_advi <- function(x, ...) {
  cat(sprintf("newstan ADVI: %s, return code %d\n", x$algorithm, x$return_code))
  invisible(x)
}
