#' Generate quantities from posterior draws
#'
#' Takes pre-computed parameter draws and generates quantities of interest.
#'
#' @param stanmod A model environment returned by [stan_model()], compiled from
#'   a Stan model with a generated quantities block.
#' @param data Named list of data variables to pass to the model.
#' @param fitted_params An object containing posterior draws of constrained parameters
#'   (e.g., the `draws` element from a [sampling()] result).
#' @param seed Random seed (NA = random).
#' @param verbose Print progress (default: FALSE).
#' @param num_threads Number of threads, or `-1` for all available threads.
#' @param ... Unused.
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with generated quantity draws.
#'   - `return_code`: integer status code.
#'   - `args`: named list of generated quantities configuration arguments.
#'     Large inputs are omitted.
#'
#' @export
generated_quantities <- function(
  stanmod,
  data,
  fitted_params,
  seed = NA,
  verbose = FALSE,
  num_threads = -1,
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(stats::runif(1, 1, 2^31 - 1))
  }

  model_instance <- new_model_instance(stanmod, data, seed)
  pars <- stanmod$constrained_param_names(model_instance$model)

  # Convert draws to matrix (rows=samples, columns=parameters)
  draws_matrix <- if (inherits(fitted_params, "draws")) {
    posterior::as_draws_matrix(posterior::subset_draws(fitted_params, variable = pars))
  } else {
    as.matrix(fitted_params)
  }

  args <- list(
    method = "generate_quantities",
    seed = as.integer(seed),
    verbose = as.logical(verbose),
    num_threads = as.integer(num_threads),
    draws = draws_matrix
  )

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, args)
  )

  gqs_draws <- posterior::as_draws_df(result$samples)

  structure(list(
    draws = gqs_draws,
    return_code = result$return_code,
    args = service_args(args)
  ), class = c("StanGeneratedQuantities", "StanService", "list"))
}
