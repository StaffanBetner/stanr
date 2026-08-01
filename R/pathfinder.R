# Internal helper for Pathfinder approximation.
# Accepts pre-normalized arguments from .newstan_normalize_pathfinder().

#' @noRd
.newstan_run_pathfinder <- function(stanmod, args) {
  native_args <- list(
    method = "pathfinder",
    seed = as.integer(args$seed),
    id = 1L,
    init_radius = init_radius(args$init),
    max_lbfgs_iters = as.integer(args$max_lbfgs_iters),
    history_size = as.integer(args$history_size),
    num_elbo_draws = as.integer(args$num_elbo_draws),
    num_draws = as.integer(args$single_path_draws),
    num_paths = as.integer(args$num_paths),
    num_psis_draws = as.integer(args$draws),
    init_alpha = as.double(args$init_alpha),
    tol_obj = as.double(args$tol_obj),
    tol_rel_obj = as.double(args$tol_rel_obj),
    tol_grad = as.double(args$tol_grad),
    tol_rel_grad = as.double(args$tol_rel_grad),
    tol_param = as.double(args$tol_param),
    save_single_paths = as.logical(args$save_single_paths),
    psis_resample = as.logical(args$psis_resample),
    calculate_lp = as.logical(args$calculate_lp),
    refresh = as.integer(args$refresh),
    verbose = as.logical(args$show_messages),
    num_threads = as.integer(args$threads),
    init = normalize_init(args$init)
  )

  model_instance <- new_model_instance(stanmod, args$data, args$seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = args$threads),
    result <- stanmod$run_model(model_instance$model, native_args)
  )

  if (result$return_code != 0) {
    return(
      structure(list(
        draws = NULL,
        return_code = result$return_code,
        args = service_args(native_args)
      ), class = c("StanPathfinder", "StanService", "list"))
    )
  }

  draws <- posterior::as_draws_df(result$draws)

  # Separate special columns from parameters
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
    args = service_args(native_args)
  ), class = c("StanPathfinder", "StanService", "list"))
}
