# Internal helper for ADVI (Variational Inference).
# Accepts pre-normalized arguments from .newstan_normalize_variational().

#' @noRd
.newstan_run_variational <- function(stanmod, args) {
  native_args <- list(
    method = "variational",
    algorithm = args$algorithm,
    seed = as.integer(args$seed),
    id = 1L,
    init_radius = init_radius(args$init),
    iter = as.integer(args$iter),
    grad_samples = as.integer(args$grad_samples),
    elbo_samples = as.integer(args$elbo_samples),
    tol_rel_obj = as.double(args$tol_rel_obj),
    eta = as.double(args$eta),
    adapt_engaged = as.logical(args$adapt_engaged),
    adapt_iter = as.integer(args$adapt_iter),
    eval_elbo = as.integer(args$eval_elbo),
    output_samples = as.integer(args$draws),
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
      structure(
        list(
          draws = NULL,
          return_code = result$return_code,
          args = service_args(native_args)
        ),
        class = c("StanVariational", "StanService", "list")
      )
    )
  }

  structure(
    list(
      draws = posterior::as_draws_df(result$draws),
      return_code = result$return_code,
      args = service_args(native_args)
    ),
    class = c("StanVariational", "StanService", "list")
  )
}
