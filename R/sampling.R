# Internal helper for MCMC sampling.
# Accepts pre-normalized arguments from .newstan_normalize_sample().
# Builds the native args list, calls the model, and returns the raw result.

#' @noRd
.newstan_run_sampling <- function(stanmod, args) {
  # Build args list for native call
  native_args <- list(
    method = "sample",
    algorithm = if (args$fixed_param) "fixed_param" else "hmc",
    engine = if (args$fixed_param) "nuts" else args$engine,
    metric = args$metric,
    adapt_engaged = as.logical(args$adapt_engaged),
    seed = as.integer(args$seed),
    id = as.integer(args$chain_ids[[1]]),
    num_chains = as.integer(args$chains),
    init_radius = init_radius(args$init),
    num_warmup = as.integer(args$iter_warmup),
    num_samples = as.integer(args$iter_sampling),
    thin = as.integer(args$thin),
    save_warmup = as.logical(args$save_warmup),
    refresh = as.integer(args$refresh),
    stepsize = as.double(args$step_size),
    stepsize_jitter = as.double(args$step_size_jitter),
    max_depth = as.integer(args$max_treedepth),
    int_time = as.double(args$int_time),
    delta = as.double(args$adapt_delta),
    gamma = as.double(args$adapt_gamma),
    kappa = as.double(args$adapt_kappa),
    t0 = as.double(args$adapt_t0),
    init_buffer = as.integer(args$init_buffer),
    term_buffer = as.integer(args$term_buffer),
    window = as.integer(args$window),
    init = normalize_init(args$init),
    inv_metric = args$inv_metric,
    inv_metric_na = is.null(args$inv_metric),
    verbose = as.logical(args$show_messages),
    num_threads = as.integer(args$parallel_chains * args$threads_per_chain)
  )

  model_instance <- new_model_instance(stanmod, args$data, args$seed)

  num_threads <- native_args$num_threads
  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, native_args)
  )

  # Handle non-zero return codes
  if (result$return_code != 0) {
    return(
      structure(
        list(
          draws = NULL,
          diagnostics = NULL,
          return_code = result$return_code,
          args = service_args(native_args)
        ),
        class = c("StanSample", "StanService", "list")
      )
    )
  }

  # Build result object
  draw_names <- colnames(result$samples)

  # Diagnostic variables depend on engine
  if (args$fixed_param == FALSE && args$engine == "static") {
    diagnostic_vars <- c("accept_stat__", "stepsize__")
  } else {
    diagnostic_vars <- c(
      "accept_stat__",
      "stepsize__",
      "treedepth__",
      "n_leapfrog__",
      "divergent__",
      "energy__"
    )
  }
  par_vars <- draw_names[
    !(draw_names %in% diagnostic_vars) & draw_names != ".chain"
  ]

  if (args$fixed_param) {
    diagnostics <- posterior::draws_df(
      "stepsize__" = NA,
      "treedepth__" = NA,
      "n_leapfrog__" = NA,
      "divergent__" = NA,
      "energy__" = NA
    )
  } else {
    draws <- posterior::as_draws_df(result$samples)
    diagnostics <- posterior::subset_draws(draws, variable = diagnostic_vars)
  }

  structure(
    list(
      draws = if (args$fixed_param) {
        posterior::as_draws_df(result$samples[, par_vars, drop = FALSE])
      } else {
        posterior::subset_draws(draws, par_vars)
      },
      diagnostics = diagnostics,
      return_code = result$return_code,
      args = service_args(native_args)
    ),
    class = c("StanSample", "StanService", "list")
  )
}
