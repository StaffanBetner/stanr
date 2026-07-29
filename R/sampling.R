#' Run MCMC sampling on a Stan model
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param iter_warmup Number of warmup iterations (default: 1000).
#' @param iter_sampling Number of post-warmup samples (default: 1000).
#' @param thin Thinning interval (default: 1).
#' @param save_warmup Save warmup samples (default: FALSE).
#' @param chains Number of parallel chains (default: 1). Chains are run in
#'   parallel via TBB `parallel_for` inside the Stan services.
#' @param chain_id Starting chain ID for RNG advancement (default: 1).
#' @param seed Random seed (NA = random).
#' @param init Initial values (numeric vector, or `"random"`).
#' @param init_radius Radius for random initialization (default: 2).
#' @param algorithm Sampler algorithm: `"NUTS"`, `"NUTS_FIXED"`, `"HMC"`, or
#'   `"Fixed_param"` (default: `"NUTS"`).
#' @param metric Euclidean metric: `"unit_e"`, `"diag_e"`, or `"dense_e"`
#'   (default: `"diag_e"`).
#' @param stepsize Initial stepsize (NA = adapt).
#' @param stepsize_jitter Uniform jitter for stepsize (default: 0).
#' @param max_depth Maximum tree depth for NUTS (default: 10).
#' @param delta Target acceptance rate (default: 0.8).
#' @param gamma Adaptation gamma (default: 0.05).
#' @param kappa Adaptation kappa (default: 0.75).
#' @param t0 Adaptation t0 (default: 10).
#' @param init_buffer Warmup buffer width (default: 75).
#' @param term_buffer Warmup terminal buffer (default: 50).
#' @param window Adaptation window size (default: 25).
#' @param refresh Output refresh frequency (iterations between log messages,
#'   default: 100).
#' @param verbose Print progress messages (default: TRUE).
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with parameter draws.
#'   - `diagnostics`: a `posterior::as_draws_df` object with sampler diagnostics.
#'   - `return_code`: integer status code.
#'   - `args`: named list of sampling arguments.
#'
#' @export
sampling <- function(
  stanmod,
  data,
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  save_warmup = FALSE,
  chains = 1,
  chain_id = 1,
  seed = NA,
  init = 0,
  init_radius = 2,
  algorithm = "NUTS",
  metric = "diag_e",
  stepsize = 1,
  stepsize_jitter = 0,
  max_depth = 10,
  delta = 0.8,
  gamma = 0.05,
  kappa = 0.75,
  t0 = 10,
  init_buffer = 75,
  term_buffer = 50,
  window = 25,
  refresh = 100,
  verbose = TRUE,
  ...
) {
  # Handle seed
  if (is.na(seed)) {
    seed <- as.integer(runif(1, 1, 2^31 - 1))
  }

  # Build args list for .Call
  args <- list(
    method = "sampling",
    algorithm = algorithm,
    metric = metric,
    seed = as.integer(seed),
    chain_id = as.integer(chain_id),
    chains = as.integer(chains),
    init_radius = as.double(init_radius),
    num_warmup = as.integer(iter_warmup),
    num_samples = as.integer(iter_sampling),
    thin = as.integer(thin),
    save_warmup = as.logical(save_warmup),
    refresh = as.integer(refresh),
    stepsize = as.double(stepsize),
    stepsize_jitter = as.double(stepsize_jitter),
    max_depth = as.integer(max_depth),
    delta = as.double(delta),
    gamma = as.double(gamma),
    kappa = as.double(kappa),
    t0 = as.double(t0),
    init_buffer = as.integer(init_buffer),
    term_buffer = as.integer(term_buffer),
    window = as.integer(window),
    init = if (is.list(init)) init else list(),
    verbose = as.logical(verbose)
  )

  dat_ptr <- .Call(`r_data_context`, data)
  mod_ptr <- stanmod$new_model(dat_ptr, seed)
  withr::with_envvar(
    c(STAN_NUM_THREADS = 4),
    result <- .Call(`newstan_run`, mod_ptr, args)
  )

  # Build result object
  draw_names <- colnames(result$samples)
  draws <- posterior::as_draws_df(result$samples)
  diagnostic_vars <- c(
    "accept_stat__",
    "stepsize__",
    "treedepth__",
    "n_leapfrog__",
    "divergent__",
    "energy__"
  )
  par_vars <- draw_names[
    !(draw_names %in% diagnostic_vars) & draw_names != ".chain"
  ]
  if (algorithm == "Fixed_param") {
    diagnostics <- posterior::draws_df(
      "stepsize__" = NA,
      "treedepth__" = NA,
      "n_leapfrog__" = NA,
      "divergent__" = NA,
      "energy__" = NA
    )
  } else {
    diagnostics <- posterior::subset_draws(draws, variable = diagnostic_vars)
  }

  list(
    draws = posterior::subset_draws(draws, par_vars),
    diagnostics = diagnostics,
    return_code = result$return_code,
    args = args
  )
}
