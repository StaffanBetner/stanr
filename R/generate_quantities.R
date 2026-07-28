#' Generate quantities from posterior draws
#'
#' Takes pre-computed parameter draws and generates quantities of interest.
#'
#' @param object A `newstan_fit` object (the model with generated quantities block)
#' @param fit_draws An S3 object containing posterior draws (from [sampling()])
#' @param seed Random seed
#' @param chain_id Chain ID for RNG advancement (default: 1)
#' @param verbose Print progress (default: FALSE)
#' @param ... Unused
#'
#' @return An S3 object of class `"newstan_gqs"` with generated quantity draws.
#'
#' @export
generated_quantities <- function(object,
                                 fit_draws,
                                 seed = NA,
                                 chain_id = 1,
                                 verbose = FALSE,
                                 ...) {
  if (!inherits(object, "newstan_fit")) {
    stop("'object' must be a newstan_fit object.")
  }
  if (!inherits(fit_draws, "newstan_sampler")) {
    stop("'fit_draws' must be a newstan_sampler object from sampling().")
  }

  if (is.na(seed)) seed <- as.integer(runif(1, 1, 2^31 - 1))

  # Convert samples data.frame to Eigen::MatrixXd (rows=samples, columns=params)
  draws_df <- fit_draws$samples
  draws_matrix <- as.matrix(draws_df)

  args <- list(
    method = "standalone_gqs",
    seed = as.integer(seed),
    chain_id = as.integer(chain_id),
    verbose = as.logical(verbose),
    draws = draws_matrix
  )

  dll_handle <- object$dll
  result <- .Call(dll_handle[["newstan_run"]], object$dll_ptr, args)

  structure(
    list(
      samples = result$samples,
      return_code = result$return_code,
      method = "standalone_gqs",
      args = args
    ),
    class = "newstan_gqs"
  )
}

#' @export
print.newstan_gqs <- function(x, ...) {
  n_samples <- if (!is.null(x$samples)) nrow(x$samples) else 0L
  cat(sprintf("newstan generated quantities: %d samples, return code %d\n",
              n_samples, x$return_code))
  invisible(x)
}
