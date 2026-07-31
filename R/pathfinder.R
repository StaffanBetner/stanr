#' Run Pathfinder approximation
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param max_lbfgs_iters Maximum L-BFGS iterations (default: 1000).
#' @param history_size L-BFGS history size (default: 5).
#' @param num_elbo_draws MC draws for ELBO evaluation (default: 25).
#' @param num_draws Approximate posterior draws per path (default: 1000).
#' @param num_paths Number of single pathfinders to run (default: 4).
#'   When `num_paths > 1`, runs multi-path pathfinder with PSIS resampling.
#' @param num_psis_draws Number of draws to return from PSIS resampling
#'   (default: 1000). Only used when `num_paths > 1`.
#' @param seed Random seed (NA = random).
#' @param id Chain ID for RNG advancement (default: 1).
#' @param init Initialization radius, or named constrained initial values
#'   (default: 2).
#' @param init_alpha Initial line-search step size (default: 0.001).
#' @param tol_obj Tolerance on absolute objective changes (default: 1e-12).
#' @param tol_rel_obj Tolerance on relative objective changes (default: 1e4).
#' @param tol_grad Tolerance on gradient norm (default: 1e-8).
#' @param tol_rel_grad Tolerance on relative gradient norm (default: 1e7).
#' @param tol_param Tolerance on parameter changes (default: 1e-8).
#' @param save_single_paths Save individual path draws (default: FALSE).
#' @param psis_resample Use PSIS resampling for multiple paths (default: TRUE).
#' @param calculate_lp Calculate log probabilities (default: TRUE).
#' @param refresh Output refresh frequency (default: 100).
#' @param verbose Print progress (default: TRUE).
#' @param num_threads Number of threads, or `-1` for all available threads.
#' @param ... Unused.
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with approximate posterior draws.
#'   - `diagnostics`: a `posterior::as_draws_df` object with pathfinder diagnostics
#'     (`lp_approx__`, `lp__`, `path__`).
#'   - `return_code`: integer status code.
#'   - `args`: named list of Pathfinder configuration arguments. Large inputs
#'     are omitted.
#'
#' @noRd
pathfinder <- function(
  stanmod,
  data,
  max_lbfgs_iters = 1000,
  history_size = 5,
  num_elbo_draws = 25,
  num_draws = 1000,
  num_paths = 4,
  num_psis_draws = 1000,
  seed = NA,
  id = 1,
  init = 2,
  init_alpha = 0.001,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  save_single_paths = FALSE,
  psis_resample = TRUE,
  calculate_lp = TRUE,
  refresh = 100,
  verbose = TRUE,
  num_threads = -1,
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(stats::runif(1, 1, 2^31 - 1))
  }

  args <- list(
    method = "pathfinder",
    seed = as.integer(seed),
    id = as.integer(id),
    init_radius = init_radius(init),
    max_lbfgs_iters = as.integer(max_lbfgs_iters),
    history_size = as.integer(history_size),
    num_elbo_draws = as.integer(num_elbo_draws),
    num_draws = as.integer(num_draws),
    num_paths = as.integer(num_paths),
    num_psis_draws = as.integer(num_psis_draws),
    init_alpha = as.double(init_alpha),
    tol_obj = as.double(tol_obj),
    tol_rel_obj = as.double(tol_rel_obj),
    tol_grad = as.double(tol_grad),
    tol_rel_grad = as.double(tol_rel_grad),
    tol_param = as.double(tol_param),
    save_single_paths = as.logical(save_single_paths),
    psis_resample = as.logical(psis_resample),
    calculate_lp = as.logical(calculate_lp),
    refresh = as.integer(refresh),
    verbose = as.logical(verbose),
    num_threads = as.integer(num_threads),
    init = normalize_init(init)
  )

  model_instance <- new_model_instance(stanmod, data, seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, args)
  )

  # Handle non-zero return codes
  if (result$return_code != 0) {
    return(
      structure(list(
        draws = NULL,
        return_code = result$return_code,
        args = service_args(args)
      ), class = c("StanPathfinder", "StanService", "list"))
    )
  }

  # Convert matrix to draws_df
  draws <- posterior::as_draws_df(result$draws)

  # Separate special columns from parameters
  # Multi-path pathfinder includes lp_approx__, lp__, path__
  special_vars <- c("lp_approx__", "lp__", "path__")
  present_special <- special_vars[special_vars %in% colnames(result$draws)]

  if (length(present_special) > 0) {
    diagnostics <- posterior::subset_draws(draws, variable = present_special)
    draws <- posterior::subset_draws(
      draws,
      variable = setdiff(colnames(result$draws), present_special)
    )
  } else {
    diagnostics <- NULL
  }

  structure(list(
    draws = draws,
    diagnostics = diagnostics,
    return_code = result$return_code,
    args = service_args(args)
  ), class = c("StanPathfinder", "StanService", "list"))
}
