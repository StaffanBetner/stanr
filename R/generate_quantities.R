#' @noRd
.newstan_run_generate_quantities <- function(
  stanmod,
  fitted_params,
  data = NULL,
  seed = NULL,
  num_threads = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  common <- .newstan_normalize_common(
    data = data,
    seed = seed,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  num_threads <- as.integer(num_threads %||% 1L)

  input <- if (inherits(fitted_params, "StanFit")) {
    fitted_params$draws(format = "draws_matrix")
  } else {
    fitted_params
  }

  model <- stanmod$new_model(common$data, common$seed)
  pars <- stanmod$constrained_param_names(model)

  # Convert draws to matrix (rows=samples, columns=parameters)
  draws_matrix <- if (inherits(input, "draws")) {
    posterior::as_draws_matrix(posterior::subset_draws(
      input,
      variable = pars
    ))
  } else {
    as.matrix(input)
  }

  native_args <- list(
    method = "generate_quantities",
    seed = as.integer(common$seed),
    verbose = as.logical(common$show_messages),
    num_threads = num_threads,
    draws = draws_matrix
  )

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model, native_args)
  )

  gqs_draws <- posterior::as_draws_df(result$samples)

  structure(
    list(
      draws = gqs_draws,
      return_code = result$return_code,
      args = service_args(native_args),
      output = result$output %||% character(),
      model_ptr = model
    ),
    class = c("StanGeneratedQuantities", "StanService", "list")
  )
}
