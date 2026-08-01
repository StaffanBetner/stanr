# Internal helper for gradient diagnostics.
# Accepts pre-normalized arguments from .newstan_normalize_diagnose().

#' @noRd
.newstan_run_diagnose <- function(stanmod, args) {
  native_args <- list(
    method = "diagnose",
    epsilon = as.double(args$epsilon),
    error = as.double(args$error),
    seed = as.integer(args$seed),
    id = 1L,
    init_radius = init_radius(args$init),
    verbose = TRUE,
    num_threads = 1L,
    init = normalize_init(args$init)
  )

  model_instance <- new_model_instance(stanmod, args$data, args$seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = 1),
    result <- stanmod$run_model(model_instance$model, native_args)
  )

  n_failed <- as.integer(result$num_failed)

  if (n_failed == 0L) {
    message("[newstan] All gradient tests passed.")
  } else {
    message(sprintf(
      "[newstan] %d parameter(s) failed the gradient test.",
      n_failed
    ))
  }

  structure(n_failed, class = c("StanDiagnose", class(n_failed)))
}
