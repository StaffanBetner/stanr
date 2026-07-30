#' Run optimization on a Stan model
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param algorithm Optimization algorithm: `"newton"`, `"bfgs"`, or `"lbfgs"`
#'   (default: `"lbfgs"`).
#' @param iter Maximum iterations (default: 2000).
#' @param init_alpha Initial step size (default: 0.001).
#' @param tol_obj Tolerance on absolute objective changes (default: 1e-12).
#' @param tol_rel_obj Tolerance on relative objective changes (default: 1e4).
#' @param tol_grad Tolerance on gradient norm (default: 1e-8).
#' @param tol_rel_grad Tolerance on relative gradient norm (default: 1e7).
#' @param tol_param Tolerance on parameter changes (default: 1e-8).
#' @param history_size L-BFGS history size (default: 5).
#' @param seed Random seed (NA = random).
#' @param id Chain ID for RNG advancement (default: 1).
#' @param init Initialization radius, or named constrained initial values
#'   (default: 2).
#' @param save_iterations Save all iterations (default: FALSE).
#' @param refresh Output refresh frequency (default: 100).
#' @param verbose Print progress (default: TRUE).
#' @param num_threads Number of threads, or `-1` for all available threads.
#' @param ... Unused.
#'
#' @return A list containing:
#'   - `par`: named numeric vector of parameter values at the mode.
#'   - `value`: log probability at the mode.
#'   - `return_code`: integer status code.
#'   - `args`: named list of optimization arguments.
#'
#' @export
optimizing <- function(
  stanmod,
  data,
  algorithm = "lbfgs",
  iter = 2000,
  init_alpha = 0.001,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  history_size = 5,
  seed = NA,
  id = 1,
  init = 2,
  save_iterations = FALSE,
  refresh = 100,
  verbose = TRUE,
  num_threads = -1,
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(stats::runif(1, 1, 2^31 - 1))
  }

  args <- list(
    method = "optimize",
    algorithm = algorithm,
    seed = as.integer(seed),
    id = as.integer(id),
    init_radius = init_radius(init),
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
    data = data,
    init = normalize_init(init)
  )

  model_instance <- new_model_instance(stanmod, data, seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, args)
  )

  # Extract parameter values from last row of par matrix
  par_mat <- result$par
  par_vec <- if (is.matrix(par_mat) && nrow(par_mat) > 0) {
    par_mat[nrow(par_mat), , drop = TRUE]
  } else {
    numeric(0)
  }

  list(
    par = par_vec,
    value = result$value,
    return_code = result$return_code,
    args = args
  )
}
