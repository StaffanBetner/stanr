#' Run optimization on a fitted Stan model
#'
#' @param object A `newstan_fit` object
#' @param algorithm Optimization algorithm: `"newton"`, `"bfgs"`, or `"lbfgs"` (default: `"bfgs"`)
#' @param iter Maximum iterations (default: 2000)
#' @param init_alpha Initial step size (default: 0.001)
#' @param tol_obj Tolerance on absolute objective changes (default: 1e-12)
#' @param tol_rel_obj Tolerance on relative objective changes (default: 10000)
#' @param tol_grad Tolerance on gradient norm (default: 1e-8)
#' @param tol_rel_grad Tolerance on relative gradient norm (default: 1e7)
#' @param tol_param Tolerance on parameter changes (default: 1e-8)
#' @param history_size L-BFGS history size (default: 5)
#' @param seed Random seed
#' @param init Initial values
#' @param init_radius Initialization radius (default: 2)
#' @param save_iterations Save all iterations (default: FALSE)
#' @param refresh Output refresh frequency (default: 100)
#' @param verbose Print progress (default: TRUE)
#' @param ... Unused
#'
#' @return An S3 object of class `"newstan_optim"` with `par` (parameters), `value` (log prob), and metadata.
#'
#' @export
optimizing <- function(object,
                       algorithm = "bfgs",
                       iter = 2000,
                       init_alpha = 0.001,
                       tol_obj = 1e-12,
                       tol_rel_obj = 10000,
                       tol_grad = 1e-8,
                       tol_rel_grad = 1e7,
                       tol_param = 1e-8,
                       history_size = 5,
                       seed = NA,
                       init = 0,
                       init_radius = 2,
                       save_iterations = FALSE,
                       refresh = 100,
                       verbose = TRUE,
                       ...) {
  if (!inherits(object, "newstan_fit")) {
    stop("'object' must be a newstan_fit object.")
  }

  if (is.na(seed)) seed <- as.integer(runif(1, 1, 2^31 - 1))

  args <- list(
    method = "optimizing",
    algorithm = algorithm,
    seed = as.integer(seed),
    chain_id = 1L,
    init_radius = as.double(init_radius),
    iter = as.integer(iter),
    init_alpha = as.double(init_alpha),
    tol_obj = as.double(tol_obj),
    tol_rel_obj = as.double(tol_rel_obj),
    tol_grad = as.double(tol_grad),
    tol_rel_grad = as.double(tol_rel_grad),
    tol_param = as.double(tol_param),
    history_size = as.integer(history_size),
    save_iterations = as.logical(save_iterations),
    refresh = as.integer(refresh),
    verbose = as.logical(verbose),
    data = .prepare_data(object$data),
    init = .prepare_init(init, object)
  )

  dll_handle <- object$dll
  result <- .Call(dll_handle[["newstan_run"]], object$dll_ptr, args)

  structure(
    list(
      par = result$par,
      value = result$value,
      return_code = result$return_code,
      algorithm = algorithm,
      args = args
    ),
    class = "newstan_optim"
  )
}

#' @export
print.newstan_optim <- function(x, ...) {
  cat(sprintf("newstan optimization: %s\n", x$algorithm))
  if (!is.null(x$value)) cat(sprintf("  log prob: %.4f\n", x$value))
  if (!is.null(x$par)) cat(sprintf("  parameters: %d\n", length(x$par)))
  invisible(x)
}
