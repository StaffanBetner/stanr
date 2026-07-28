#' Run Pathfinder approximation
#'
#' @param object A `newstan_fit` object
#' @param iter Maximum L-BFGS iterations (default: 500)
#' @param history_size L-BFGS history size (default: 5)
#' @param num_elbo_draws MC draws for ELBO evaluation (default: 64)
#' @param num_draws Approximate posterior draws per path (default: 300)
#' @param seed Random seed
#' @param init Initial values
#' @param init_radius Initialization radius (default: 2)
#' @param verbose Print progress (default: TRUE)
#' @param ... Unused
#'
#' @return An S3 object of class `"newstan_pathfinder"` with posterior draws.
#'
#' @export
pathfinder <- function(object,
                       iter = 500,
                       history_size = 5,
                       num_elbo_draws = 64,
                       num_draws = 300,
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
    method = "pathfinder",
    seed = as.integer(seed),
    chain_id = 1L,
    init_radius = as.double(init_radius),
    iter = as.integer(iter),
    history_size = as.integer(history_size),
    num_elbo_draws = as.integer(num_elbo_draws),
    num_draws = as.integer(num_draws),
    verbose = as.logical(verbose),
    data = .prepare_data(object$data),
    init = .prepare_init(init, object)
  )

  dll_handle <- object$dll
  result <- .Call(dll_handle[["newstan_run"]], object$dll_ptr, args)

  structure(
    list(
      return_code = result$return_code,
      method = "pathfinder",
      args = args
    ),
    class = "newstan_pathfinder"
  )
}

#' @export
print.newstan_pathfinder <- function(x, ...) {
  cat(sprintf("newstan pathfinder: return code %d\n", x$return_code))
  invisible(x)
}
