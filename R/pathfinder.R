#' Run Pathfinder approximation
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param iter Maximum L-BFGS iterations (default: 500).
#' @param history_size L-BFGS history size (default: 5).
#' @param num_elbo_draws MC draws for ELBO evaluation (default: 64).
#' @param num_draws Approximate posterior draws per path (default: 300).
#' @param seed Random seed (NA = random).
#' @param init Initial values (numeric vector, or `"random"`).
#' @param init_radius Initialization radius (default: 2).
#' @param verbose Print progress (default: TRUE).
#' @param ... Unused.
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with approximate posterior draws.
#'   - `return_code`: integer status code.
#'   - `args`: named list of Pathfinder arguments.
#'
#' @export
pathfinder <- function(
  stanmod,
  data,
  iter = 500,
  history_size = 5,
  num_elbo_draws = 64,
  num_draws = 300,
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
    method = "pathfinder",
    seed = as.integer(seed),
    chain_id = 1L,
    init_radius = as.double(init_radius),
    iter = as.integer(iter),
    history_size = as.integer(history_size),
    num_elbo_draws = as.integer(num_elbo_draws),
    num_draws = as.integer(num_draws),
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

  list(
    return_code = result$return_code,
    args = args
  )
}
