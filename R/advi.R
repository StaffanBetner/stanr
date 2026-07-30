#' Run ADVI (Automatic Differentiation Variational Inference)
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param algorithm Variational family: `"fullrank"` or `"meanfield"`
#'   (default: `"meanfield"`).
#' @param iter Maximum iterations (default: 10000).
#' @param grad_samples MC samples for gradient estimate (default: 1).
#' @param elbo_samples MC samples for ELBO estimate (default: 100).
#' @param tol_rel_obj Convergence tolerance (default: 0.01).
#' @param eta Stepping parameter (default: 1.0).
#' @param adapt_engaged Enable eta adaptation (default: TRUE).
#' @param adapt_iter Adaptation iterations (default: 50).
#' @param eval_elbo Evaluate ELBO every Nth iteration (default: 100).
#' @param output_samples Posterior samples to draw (default: 1000).
#' @param seed Random seed (NA = random).
#' @param id Chain ID for RNG advancement (default: 1).
#' @param init Initialization radius, or named constrained initial values
#'   (default: 2).
#' @param verbose Print progress (default: TRUE).
#' @param num_threads Number of threads, or `-1` for all available threads.
#' @param ... Unused.
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with variational draws.
#'   - `return_code`: integer status code.
#'   - `args`: named list of ADVI arguments.
#'
#' @export
variational <- function(
  stanmod,
  data,
  algorithm = "meanfield",
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
    method = "variational",
    algorithm = algorithm,
    seed = as.integer(seed),
    id = as.integer(id),
    init_radius = init_radius(init),
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
    data = data,
    init = normalize_init(init)
  )

  model_instance <- new_model_instance(stanmod, data, seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, args)
  )

  if (result$return_code != 0) {
    return(list(draws = NULL, return_code = result$return_code, args = args))
  }

  list(
    draws = posterior::as_draws_df(result$draws),
    return_code = result$return_code,
    args = args
  )
}
