# Internal helper for optimization.
# Accepts pre-normalized arguments from .newstan_normalize_optimize().

#' @noRd
.newstan_run_optimize <- function(stanmod, args) {
  native_args <- list(
    method = "optimize",
    algorithm = args$algorithm,
    seed = as.integer(args$seed),
    id = 1L,
    init_radius = init_radius(args$init),
    iter = as.integer(args$iter),
    init_alpha = as.double(args$init_alpha),
    tol_obj = as.double(args$tol_obj),
    tol_rel_obj = as.double(args$tol_rel_obj),
    tol_grad = as.double(args$tol_grad),
    tol_rel_grad = as.double(args$tol_rel_grad),
    tol_param = as.double(args$tol_param),
    history_size = as.integer(args$history_size),
    save_iterations = FALSE,
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

  # Extract parameter values from last row of par matrix
  par_mat <- result$par
  par_vec <- if (is.matrix(par_mat) && nrow(par_mat) > 0) {
    par_mat[nrow(par_mat), , drop = TRUE]
  } else {
    numeric(0)
  }

  structure(list(
    par = par_vec,
    value = result$value,
    return_code = result$return_code,
    args = service_args(native_args)
  ), class = c("StanOptimize", "StanService", "list"))
}
