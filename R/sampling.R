#' @noRd
.newstan_run_sampling <- function(
  stanmod,
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  chains = 4,
  chain_ids = seq_len(chains),
  num_threads = getOption("mc.cores", 1),
  iter_warmup = NULL,
  iter_sampling = NULL,
  save_warmup = FALSE,
  thin = NULL,
  max_treedepth = NULL,
  adapt_engaged = TRUE,
  adapt_delta = NULL,
  step_size = NULL,
  metric = NULL,
  inv_metric = NULL,
  init_buffer = NULL,
  term_buffer = NULL,
  window = NULL,
  fixed_param = FALSE,
  show_messages = TRUE,
  show_exceptions = TRUE,
  engine = NULL,
  int_time = NULL,
  step_size_jitter = NULL,
  adapt_gamma = NULL,
  adapt_kappa = NULL,
  adapt_t0 = NULL
) {
  common <- .newstan_normalize_common(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  def <- .newstan_defaults$sample

  ids <- .newstan_validate_chains(chains, chain_ids)
  chains <- ids$chains
  chain_ids <- ids$chain_ids

  num_threads <- as.integer(num_threads %||% 1L)
  if (num_threads < 1L) {
    stop(
      "`num_threads` must be positive.",
      call. = FALSE
    )
  }

  metric <- metric %||% def$metric
  adapt_engaged <- isTRUE(adapt_engaged)
  fixed_param <- isTRUE(fixed_param)
  engine <- engine %||% def$engine
  if (!engine %in% c("nuts", "static")) {
    stop("`engine` must be one of \"nuts\", \"static\".", call. = FALSE)
  }
  iter_warmup <- as.integer(iter_warmup %||% def$iter_warmup)
  iter_sampling <- as.integer(iter_sampling %||% def$iter_sampling)
  thin <- as.integer(thin %||% def$thin)
  save_warmup <- isTRUE(save_warmup)

  if (iter_warmup < 0 || iter_sampling < 0) {
    stop("num_warmup and num_samples must be non-negative.", call. = FALSE)
  }
  if (thin < 1L) {
    stop("thin must be at least 1.", call. = FALSE)
  }
  # Calculate in double precision so adding two valid integer iteration counts
  # cannot overflow to NA before the explicit R-size guard below.
  num_saved_draws <- ceiling(as.double(iter_sampling) / as.double(thin))
  if (save_warmup) {
    num_saved_draws <- num_saved_draws +
      ceiling(as.double(iter_warmup) / as.double(thin))
  }
  if (num_saved_draws > .Machine$integer.max) {
    stop("Requested number of saved draws is too large.", call. = FALSE)
  }

  if (!fixed_param && engine == "static" && chains > 1L) {
    stop(
      "Static HMC only supports a single chain. Set `chains` = 1.",
      call. = FALSE
    )
  }

  # Normalize inv_metric: validate and wrap for native consumption
  inv_metric <- .newstan_normalize_inv_metric(
    inv_metric = inv_metric,
    metric = metric,
    adapt_engaged = adapt_engaged,
    chains = chains
  )

  native_args <- list(
    method = "sample",
    algorithm = if (fixed_param) "fixed_param" else "hmc",
    engine = if (fixed_param) "nuts" else engine,
    metric = metric,
    adapt_engaged = as.logical(adapt_engaged),
    seed = as.integer(common$seed),
    id = as.integer(chain_ids[[1]]),
    num_chains = as.integer(chains),
    init_radius = init_radius(common$init),
    num_warmup = iter_warmup,
    num_samples = iter_sampling,
    thin = thin,
    save_warmup = as.logical(save_warmup),
    refresh = as.integer(common$refresh),
    stepsize = as.double(step_size %||% def$step_size),
    stepsize_jitter = as.double(step_size_jitter %||% def$step_size_jitter),
    max_depth = as.integer(max_treedepth %||% def$max_treedepth),
    int_time = as.double(int_time %||% def$int_time),
    delta = as.double(adapt_delta %||% def$adapt_delta),
    gamma = as.double(adapt_gamma %||% def$adapt_gamma),
    kappa = as.double(adapt_kappa %||% def$adapt_kappa),
    t0 = as.double(adapt_t0 %||% def$adapt_t0),
    init_buffer = as.integer(init_buffer %||% def$init_buffer),
    term_buffer = as.integer(term_buffer %||% def$term_buffer),
    window = as.integer(window %||% def$window),
    init = normalize_init(common$init),
    inv_metric = inv_metric,
    inv_metric_na = is.null(inv_metric),
    verbose = as.logical(common$show_messages),
    num_threads = num_threads
  )

  model <- stanmod$new_model(common$data, common$seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model, native_args)
  )

  # Handle non-zero return codes
  if (result$return_code != 0) {
    return(
      structure(
        list(
          draws = NULL,
          diagnostics = NULL,
          return_code = result$return_code,
          args = service_args(native_args),
          output = result$output %||% character(),
          model_ptr = model
        ),
        class = c("StanSample", "StanService", "list")
      )
    )
  }

  # Build result object
  draw_names <- colnames(result$samples)

  # Diagnostic variables depend on engine
  if (fixed_param == FALSE && engine == "static") {
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

  if (fixed_param) {
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
      draws = if (fixed_param) {
        posterior::as_draws_df(result$samples[, par_vars, drop = FALSE])
      } else {
        posterior::subset_draws(draws, par_vars)
      },
      diagnostics = diagnostics,
      return_code = result$return_code,
      args = service_args(native_args),
      inv_metric = result$inv_metric,
      step_size = result$step_size,
      output = result$output %||% character(),
      model_ptr = model
    ),
    class = c("StanSample", "StanService", "list")
  )
}
