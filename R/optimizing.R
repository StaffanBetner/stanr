#' @noRd
.newstan_run_optimize <- function(
  stanmod,
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  threads = NULL,
  algorithm = NULL,
  jacobian = FALSE,
  init_alpha = NULL,
  iter = NULL,
  tol_obj = NULL,
  tol_rel_obj = NULL,
  tol_grad = NULL,
  tol_rel_grad = NULL,
  tol_param = NULL,
  history_size = NULL,
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
  def <- .newstan_defaults$optimize
  threads <- as.integer(threads %||% 1L)

  native_args <- list(
    method = "optimize",
    algorithm = algorithm %||% def$algorithm,
    seed = as.integer(common$seed),
    id = 1L,
    init_radius = init_radius(common$init),
    iter = as.integer(iter %||% def$iter),
    init_alpha = as.double(init_alpha %||% def$init_alpha),
    tol_obj = as.double(tol_obj %||% def$tol_obj),
    tol_rel_obj = as.double(tol_rel_obj %||% def$tol_rel_obj),
    tol_grad = as.double(tol_grad %||% def$tol_grad),
    tol_rel_grad = as.double(tol_rel_grad %||% def$tol_rel_grad),
    tol_param = as.double(tol_param %||% def$tol_param),
    history_size = as.integer(history_size %||% def$history_size),
    save_iterations = FALSE,
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

  # Extract parameter values from last row of par matrix
  par_mat <- result$par
  par_vec <- if (is.matrix(par_mat) && nrow(par_mat) > 0) {
    par_mat[nrow(par_mat), , drop = TRUE]
  } else {
    numeric(0)
  }

  structure(
    list(
      par = par_vec,
      value = result$value,
      return_code = result$return_code,
      args = service_args(native_args),
      output = result$output %||% character(),
      model_ptr = model
    ),
    class = c("StanOptimize", "StanService", "list")
  )
}
