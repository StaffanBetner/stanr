# Internal helper for generated quantities.
# Accepts pre-normalized common arguments and a fitted_params input.

#' @noRd
.newstan_run_generate_quantities <- function(
  stanmod,
  args,
  fitted_params,
  parallel_chains,
  threads_per_chain
) {
  model_instance <- new_model_instance(stanmod, args$data, args$seed)
  pars <- stanmod$constrained_param_names(model_instance$model)

  # Convert draws to matrix (rows=samples, columns=parameters)
  draws_matrix <- if (inherits(fitted_params, "draws")) {
    posterior::as_draws_matrix(posterior::subset_draws(
      fitted_params,
      variable = pars
    ))
  } else {
    as.matrix(fitted_params)
  }

  native_args <- list(
    method = "generate_quantities",
    seed = as.integer(args$seed),
    verbose = as.logical(args$show_messages),
    num_threads = as.integer(parallel_chains * threads_per_chain),
    draws = draws_matrix
  )

  withr::with_envvar(
    c(STAN_NUM_THREADS = parallel_chains * threads_per_chain),
    result <- stanmod$run_model(model_instance$model, native_args)
  )

  gqs_draws <- posterior::as_draws_df(result$samples)

  structure(
    list(
      draws = gqs_draws,
      return_code = result$return_code,
      args = service_args(native_args)
    ),
    class = c("StanGeneratedQuantities", "StanService", "list")
  )
}
