# StanModel $pathfinder() method -----------------------------------------------

#' Run Pathfinder
#'
#' @name model-method-pathfinder
#' @aliases pathfinder
#' @family StanModel methods
#'
#' @description The `$pathfinder()` method of a [`StanModel`] object runs
#'   Stan's `"pathfinder"` method, which uses a parallel iterative optimization
#'   approach to approximate the posterior distribution. Returns a
#'   [`StanPathfinder`] object.
#'
#' @template param-data
#' @template param-seed
#' @template param-init
#' @param num_paths (integer) The number of paths to use.
#' @param single_path_draws (integer) Number of draws per path.
#' @param draws (integer) Total number of draws from the approximation.
#' @param max_lbfgs_iters (integer) Maximum LBFGS iterations per path.
#' @param num_elbo_draws (integer) Number of draws for ELBO estimation.
#' @param save_single_paths (logical) Should single path results be saved?
#' @param psis_resample (logical) Should Pareto smoothed importance sampling
#'   resampling be used?
#' @param calculate_lp (logical) Should the log density be calculated?
#' @template param-num_threads
#' @template param-show_messages
#' @template param-show_exceptions
#' @template param-opencl_ids
#'
#' @return A [`StanPathfinder`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_pathfinder_impl <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  init_alpha = 0.001,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  history_size = 5L,
  single_path_draws = 1000L,
  draws = 1000L,
  num_paths = 4,
  max_lbfgs_iters = 1000L,
  num_elbo_draws = 25L,
  save_single_paths = FALSE,
  psis_resample = TRUE,
  calculate_lp = TRUE,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  save_single_paths <- .stanr_flag(save_single_paths, "save_single_paths")
  psis_resample <- .stanr_flag(psis_resample, "psis_resample")
  calculate_lp <- .stanr_flag(calculate_lp, "calculate_lp")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  refresh <- .stanr_int(refresh, "refresh")
  num_paths <- .stanr_int(num_paths, "num_paths", min = 1L)
  single_path_draws <- .stanr_int(
    single_path_draws,
    "single_path_draws",
    min = 1L
  )
  draws <- .stanr_int(draws, "draws", min = 1L)
  max_lbfgs_iters <- .stanr_int(max_lbfgs_iters, "max_lbfgs_iters")
  num_elbo_draws <- .stanr_int(num_elbo_draws, "num_elbo_draws")
  history_size <- .stanr_int(history_size, "history_size", min = 1L)

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "pathfinder",
      seed = seed,
      id = 1L,
      init_radius = resolved_init$radius,
      max_lbfgs_iters = max_lbfgs_iters,
      history_size = history_size,
      num_elbo_draws = num_elbo_draws,
      num_draws = single_path_draws,
      num_paths = num_paths,
      num_psis_draws = draws,
      init_alpha = as.double(init_alpha),
      tol_obj = as.double(tol_obj),
      tol_rel_obj = as.double(tol_rel_obj),
      tol_grad = as.double(tol_grad),
      tol_rel_grad = as.double(tol_rel_grad),
      tol_param = as.double(tol_param),
      save_single_paths = save_single_paths,
      psis_resample = psis_resample,
      calculate_lp = calculate_lp,
      refresh = refresh,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    if (result$return_code != 0) {
      return(list(draws = NULL))
    }
    draws <- posterior::as_draws_matrix(result$draws)
    special_vars <- c("lp_approx__", "lp__", "path__")
    present <- intersect(special_vars, colnames(result$draws))
    diagnostics <- if (length(present)) {
      posterior::subset_draws(draws, variable = present)
    }
    list(draws = draws, diagnostics = diagnostics)
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanPathfinder$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "pathfinder",
      num_paths = num_paths,
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
