#' @noRd
.newstan_run_pathfinder <- function(
  stanmod,
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  threads = NULL,
  init_alpha = NULL,
  tol_obj = NULL,
  tol_rel_obj = NULL,
  tol_grad = NULL,
  tol_rel_grad = NULL,
  tol_param = NULL,
  history_size = NULL,
  single_path_draws = NULL,
  draws = NULL,
  num_paths = 4,
  max_lbfgs_iters = NULL,
  num_elbo_draws = NULL,
  save_single_paths = NULL,
  psis_resample = NULL,
  calculate_lp = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  common <- .newstan_normalize_common(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  def <- .newstan_defaults$pathfinder
  threads <- as.integer(threads %||% 1L)

  native_args <- list(
    method = "pathfinder",
    seed = as.integer(common$seed),
    id = 1L,
    init_radius = init_radius(common$init),
    max_lbfgs_iters = as.integer(max_lbfgs_iters %||% def$max_lbfgs_iters),
    history_size = as.integer(history_size %||% def$history_size),
    num_elbo_draws = as.integer(num_elbo_draws %||% def$num_elbo_draws),
    num_draws = as.integer(single_path_draws %||% def$single_path_draws),
    num_paths = as.integer(num_paths),
    num_psis_draws = as.integer(draws %||% def$draws),
    init_alpha = as.double(init_alpha %||% def$init_alpha),
    tol_obj = as.double(tol_obj %||% def$tol_obj),
    tol_rel_obj = as.double(tol_rel_obj %||% def$tol_rel_obj),
    tol_grad = as.double(tol_grad %||% def$tol_grad),
    tol_rel_grad = as.double(tol_rel_grad %||% def$tol_rel_grad),
    tol_param = as.double(tol_param %||% def$tol_param),
    save_single_paths = as.logical(save_single_paths %||% def$save_single_paths),
    psis_resample = as.logical(psis_resample %||% def$psis_resample),
    calculate_lp = as.logical(calculate_lp %||% def$calculate_lp),
    refresh = as.integer(common$refresh),
    verbose = as.logical(common$show_messages),
    num_threads = threads,
    init = normalize_init(common$init)
  )

  model <- stanmod$new_model(common$data, common$seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = threads),
    result <- stanmod$run_model(model, native_args)
  )

  if (result$return_code != 0) {
    return(
      structure(
        list(
          draws = NULL,
          return_code = result$return_code,
          args = service_args(native_args),
          output = result$output %||% character(),
          model_ptr = model
        ),
        class = c("StanPathfinder", "StanService", "list")
      )
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

  structure(
    list(
      draws = draws,
      diagnostics = diagnostics,
      return_code = result$return_code,
      args = service_args(native_args),
      output = result$output %||% character(),
      model_ptr = model
    ),
    class = c("StanPathfinder", "StanService", "list")
  )
}
