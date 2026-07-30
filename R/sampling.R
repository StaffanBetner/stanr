#' Run MCMC sampling on a Stan model
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param num_warmup Number of warmup iterations (default: 1000).
#' @param num_samples Number of post-warmup samples (default: 1000).
#' @param thin Thinning interval (default: 1).
#' @param save_warmup Save warmup samples (default: FALSE).
#' @param num_chains Number of parallel chains (default: 1). Chains are run in
#'   parallel via TBB `parallel_for` inside the Stan services.
#' @param id Starting chain ID for RNG advancement (default: 1).
#' @param seed Random seed (NA = random).
#' @param init Initialization radius, or named constrained initial values
#'   (default: 2).
#' @param algorithm Sampler algorithm: `"hmc"` or `"fixed_param"` (default: `"hmc"`).
#' @param engine HMC engine: `"nuts"` or `"static"` (default: `"nuts"`). Only
#'   used when `algorithm = "hmc"`.
#' @param metric Euclidean metric: `"unit_e"`, `"diag_e"`, or `"dense_e"`
#'   (default: `"diag_e"`).
#' @param inv_metric Precomputed inverse metric. For `"diag_e"`, a numeric vector
#'   of length equal to the number of unconstrained parameters. For `"dense_e"`,
#'   a square matrix. Can be a single metric (recycled across chains) or a list
#'   of metrics (one per chain). Corresponds to CmdStan's `metric_file` argument.
#'   Default: `NULL` (identity metric).
#' @param stepsize Initial stepsize (default: 1).
#' @param stepsize_jitter Uniform jitter for stepsize (default: 0).
#' @param max_depth Maximum tree depth for NUTS (default: 10). Not used
#'   for static HMC.
#' @param int_time Integration time for static HMC (default: `2 * pi`). Not used
#'   for NUTS.
#' @param delta Target acceptance rate (default: 0.8).
#' @param gamma Adaptation gamma (default: 0.05).
#' @param kappa Adaptation kappa (default: 0.75).
#' @param t0 Adaptation t0 (default: 10).
#' @param init_buffer Warmup buffer width (default: 75).
#' @param term_buffer Warmup terminal buffer (default: 50).
#' @param window Adaptation window size (default: 25).
#' @param adapt_engaged Enable stepsize/metric adaptation during warmup
#'   (default: TRUE). When FALSE, uses fixed stepsize throughout.
#' @param refresh Output refresh frequency (iterations between log messages,
#'   default: 100).
#' @param verbose Print progress messages (default: TRUE).
#' @param num_threads Number of threads, or `-1` for all available threads.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with parameter draws.
#'   - `diagnostics`: a `posterior::as_draws_df` object with sampler diagnostics.
#'   - `return_code`: integer status code.
#'   - `args`: named list of sampling arguments.
#'
#' @export
sampling <- function(
  stanmod,
  data,
  num_warmup = 1000,
  num_samples = 1000,
  thin = 1,
  save_warmup = FALSE,
  num_chains = 1,
  id = 1,
  seed = NA,
  init = 2,
  algorithm = "hmc",
  engine = "nuts",
  metric = "diag_e",
  inv_metric = NULL,
  stepsize = 1,
  stepsize_jitter = 0,
  max_depth = 10,
  int_time = 2 * pi,
  delta = 0.8,
  gamma = 0.05,
  kappa = 0.75,
  t0 = 10,
  init_buffer = 75,
  term_buffer = 50,
  window = 25,
  adapt_engaged = TRUE,
  refresh = 100,
  verbose = TRUE,
  num_threads = -1,
  ...
) {
  # Handle seed
  if (is.na(seed)) {
    seed <- as.integer(stats::runif(1, 1, 2^31 - 1))
  }

  init_radius_value <- init_radius(init)

  # Validate configuration
  if (num_chains < 1) {
    stop("chains must be at least 1.", call. = FALSE)
  }
  if (num_warmup < 0 || num_samples < 0) {
    stop("num_warmup and num_samples must be non-negative.", call. = FALSE)
  }
  if (thin < 1) {
    stop("thin must be at least 1.", call. = FALSE)
  }
  num_saved_draws <- num_samples %/% thin
  if (save_warmup) {
    num_saved_draws <- num_saved_draws + num_warmup %/% thin
  }
  if (num_saved_draws > .Machine$integer.max) {
    stop("Requested number of saved draws is too large.", call. = FALSE)
  }
  if (refresh < 0) {
    stop("refresh must be non-negative.", call. = FALSE)
  }
  if (!is.finite(init_radius_value) || init_radius_value < 0) {
    stop("init_radius must be finite and non-negative.", call. = FALSE)
  }
  if (!is.finite(stepsize) || stepsize <= 0 ||
      !is.finite(stepsize_jitter) || stepsize_jitter < 0 ||
      stepsize_jitter > 1) {
    stop("stepsize must be positive and stepsize_jitter must be in [0, 1].",
         call. = FALSE)
  }
  if (max_depth < 1 || !is.finite(int_time) || int_time <= 0) {
    stop("max_depth must be at least 1 and int_time must be positive.",
         call. = FALSE)
  }
  if (!is.finite(delta) || delta <= 0 || delta >= 1 ||
      !is.finite(gamma) || gamma <= 0 ||
      !is.finite(kappa) || kappa <= 0 || kappa > 1 ||
      !is.finite(t0) || t0 <= 0) {
    stop("Invalid adaptation parameters.", call. = FALSE)
  }
  if (init_buffer < 0 || term_buffer < 0 || window < 0) {
    stop("Adaptation window parameters must be non-negative.", call. = FALSE)
  }
  if (!algorithm %in% c("hmc", "fixed_param")) {
    stop("Unknown sampling algorithm: ", algorithm, call. = FALSE)
  }
  if (algorithm == "hmc" && !engine %in% c("nuts", "static")) {
    stop("Unknown HMC engine: ", engine, call. = FALSE)
  }
  if (!metric %in% c("unit_e", "diag_e", "dense_e")) {
    stop("Unknown metric: ", metric, call. = FALSE)
  }
  if (algorithm == "hmc" && adapt_engaged && num_warmup == 0) {
    stop("num_warmup must be > 0 when adapt_engaged is TRUE.",
         call. = FALSE)
  }
  if (algorithm == "hmc" && engine == "static" && num_chains > 1) {
    stop("Static HMC only supports a single chain. Set chains = 1.",
         call. = FALSE)
  }

  # Warn if inv_metric provided with unit_e
  if (!is.null(inv_metric) && metric == "unit_e") {
    warning("inv_metric is ignored when metric = 'unit_e'", call. = FALSE)
  }

  # Prepare inv_metric for C++: wrap in list if needed
  inv_metric_na <- is.null(inv_metric)
  if (!inv_metric_na) {
    # If not already a list, wrap as single metric recycled across chains
    if (!is.list(inv_metric)) {
      inv_metric <- list(inv_metric)
    }
    # Validate length if per-chain
    if (length(inv_metric) > 1 && length(inv_metric) != num_chains) {
      stop(
        "inv_metric must be a single metric or a list of length ",
        num_chains,
        " (one per chain)",
        call. = FALSE
      )
    }
    if (algorithm == "hmc" && engine == "nuts" && !adapt_engaged &&
        metric %in% c("diag_e", "dense_e") && num_chains > 1) {
      stop(
        "inv_metric with non-adaptive ",
        metric,
        " is only supported for a single chain. ",
        "Set adapt_engaged = TRUE for multi-chain with custom metric.",
        call. = FALSE
      )
    }
  }

  # Build args list for .Call
  args <- list(
    method = "sample",
    algorithm = algorithm,
    engine = if (algorithm == "hmc") engine else "nuts",
    metric = metric,
    adapt_engaged = as.logical(adapt_engaged),
    seed = as.integer(seed),
    id = as.integer(id),
    num_chains = as.integer(num_chains),
    init_radius = init_radius_value,
    num_warmup = as.integer(num_warmup),
    num_samples = as.integer(num_samples),
    thin = as.integer(thin),
    save_warmup = as.logical(save_warmup),
    refresh = as.integer(refresh),
    stepsize = as.double(stepsize),
    stepsize_jitter = as.double(stepsize_jitter),
    max_depth = as.integer(max_depth),
    int_time = as.double(int_time),
    delta = as.double(delta),
    gamma = as.double(gamma),
    kappa = as.double(kappa),
    t0 = as.double(t0),
    init_buffer = as.integer(init_buffer),
    term_buffer = as.integer(term_buffer),
    window = as.integer(window),
    init = normalize_init(init),
    inv_metric = inv_metric,
    inv_metric_na = inv_metric_na,
    verbose = as.logical(verbose),
    num_threads = as.integer(num_threads)
  )

  model_instance <- new_model_instance(stanmod, data, seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = num_threads),
    result <- stanmod$run_model(model_instance$model, args)
  )

  # Handle non-zero return codes (CONFIG, SOFTWARE errors)
  if (result$return_code != 0) {
    return(
      structure(list(
        draws = NULL,
        diagnostics = NULL,
        return_code = result$return_code,
        args = args
      ), class = c("StanSample", "StanService", "list"))
    )
  }

  # Build result object
  draw_names <- colnames(result$samples)
  draws <- posterior::as_draws_df(result$samples)

  # Diagnostic variables depend on engine
  if (algorithm == "hmc" && engine == "static") {
    # Static HMC only produces accept_stat__ and stepsize__
    diagnostic_vars <- c("accept_stat__", "stepsize__")
  } else {
    # NUTS produces full set of diagnostics
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
  if (algorithm == "fixed_param") {
    diagnostics <- posterior::draws_df(
      "stepsize__" = NA,
      "treedepth__" = NA,
      "n_leapfrog__" = NA,
      "divergent__" = NA,
      "energy__" = NA
    )
  } else {
    diagnostics <- posterior::subset_draws(draws, variable = diagnostic_vars)
  }

  structure(list(
    draws = posterior::subset_draws(draws, par_vars),
    diagnostics = diagnostics,
    return_code = result$return_code,
    args = args
  ), class = c("StanSample", "StanService", "list"))
}
