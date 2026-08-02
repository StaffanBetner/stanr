#' @noRd
.newstan_run_variational <- function(
  stanmod,
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  threads = NULL,
  algorithm = NULL,
  iter = NULL,
  grad_samples = NULL,
  elbo_samples = NULL,
  tol_rel_obj = NULL,
  eta = NULL,
  adapt_engaged = NULL,
  adapt_iter = NULL,
  eval_elbo = NULL,
  draws = NULL,
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
  def <- .newstan_defaults$variational
  threads <- as.integer(threads %||% 1L)

  native_args <- list(
    method = "variational",
    algorithm = algorithm %||% def$algorithm,
    seed = as.integer(common$seed),
    id = 1L,
    init_radius = init_radius(common$init),
    iter = as.integer(iter %||% def$iter),
    grad_samples = as.integer(grad_samples %||% def$grad_samples),
    elbo_samples = as.integer(elbo_samples %||% def$elbo_samples),
    tol_rel_obj = as.double(tol_rel_obj %||% def$tol_rel_obj),
    eta = as.double(eta %||% def$eta),
    adapt_engaged = as.logical(adapt_engaged %||% def$adapt_engaged),
    adapt_iter = as.integer(adapt_iter %||% def$adapt_iter),
    eval_elbo = as.integer(eval_elbo %||% def$eval_elbo),
    output_samples = as.integer(draws %||% def$draws),
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
        class = c("StanVariational", "StanService", "list")
      )
    )
  }

  structure(
    list(
      draws = posterior::as_draws_df(result$draws),
      return_code = result$return_code,
      args = service_args(native_args),
      output = result$output %||% character(),
      model_ptr = model
    ),
    class = c("StanVariational", "StanService", "list")
  )
}
