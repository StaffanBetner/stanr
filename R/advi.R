#' Run ADVI (Automatic Differentiation Variational Inference)
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param algorithm Variational family: `"fullrank"` or `"meanfield"`
#'   (default: `"fullrank"`).
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
#' @param init Initial values (numeric vector, or `"random"`).
#' @param init_radius Initialization radius (default: 2).
#' @param verbose Print progress (default: TRUE).
#' @param ... Unused.
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with variational draws.
#'   - `return_code`: integer status code.
#'   - `args`: named list of ADVI arguments.
#'
#' @export
advi <- function(
  stanmod,
  data,
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
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(runif(1, 1, 2^31 - 1))
  }

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
    data = data,
    init = normalize_init(init)
  )

  dat_ptr <- .Call(`r_data_context`, data)
  mod_ptr <- stanmod$new_model(dat_ptr, seed)
  withr::with_envvar(
    c(STAN_NUM_THREADS = 4),
    result <- .Call(`newstan_run`, mod_ptr, args)
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
