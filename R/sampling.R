#' Run MCMC sampling on a Stan model
#'
#' @param stanmod A model environment returned by [stan_model()].
#' @param data Named list of data variables to pass to the model.
#' @param iter_warmup Number of warmup iterations (default: 1000).
#' @param iter_sampling Number of post-warmup samples (default: 1000).
#' @param thin Thinning interval (default: 1).
#' @param save_warmup Save warmup samples (default: FALSE).
#' @param chains Number of parallel chains (default: 1). Chains are run in
#'   parallel via TBB `parallel_for` inside the Stan services.
#' @param chain_id Starting chain ID for RNG advancement (default: 1).
#' @param seed Random seed (NA = random).
#' @param init Initial values (numeric vector, or `"random"`).
#' @param init_radius Radius for random initialization (default: 2).
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
#' @param int_time Integration time for static HMC (default: 10). Not used
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
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  save_warmup = FALSE,
  chains = 1,
  chain_id = 1,
  seed = NA,
  init = 0,
  init_radius = 2,
  algorithm = "hmc",
  engine = "nuts",
  metric = "diag_e",
  inv_metric = NULL,
  stepsize = 1,
  stepsize_jitter = 0,
  max_depth = 10,
  int_time = 10,
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
  ...
) {
  # Handle seed
  if (is.na(seed)) {
    seed <- as.integer(runif(1, 1, 2^31 - 1))
  }

  # Validate algorithm
  if (!algorithm %in% c("hmc", "fixed_param")) {
    stop("algorithm must be 'hmc' or 'fixed_param'", call. = FALSE)
  }

  # Validate engine (only relevant for HMC)
  if (algorithm == "hmc" && !engine %in% c("nuts", "static")) {
    stop(
      "engine must be 'nuts' or 'static' when algorithm is 'hmc'",
      call. = FALSE
    )
  }

  # Validate metric
  if (!metric %in% c("unit_e", "diag_e", "dense_e")) {
    stop("metric must be 'unit_e', 'diag_e', or 'dense_e'", call. = FALSE)
  }

  # Warn if inv_metric provided with unit_e
  if (!is.null(inv_metric) && metric == "unit_e") {
    warning("inv_metric is ignored when metric = 'unit_e'", call. = FALSE)
  }

  # Static HMC only supports a single chain
  if (algorithm == "hmc" && engine == "static" && chains > 1) {
    stop(
      "Static HMC only supports a single chain. Set chains = 1.",
      call. = FALSE
    )
  }

  # Prepare inv_metric for C++: wrap in list if needed
  inv_metric_na <- is.null(inv_metric)
  if (!inv_metric_na) {
    # If not already a list, wrap as single metric recycled across chains
    if (!is.list(inv_metric)) {
      inv_metric <- list(inv_metric)
    }
    # Validate length if per-chain
    if (length(inv_metric) > 1 && length(inv_metric) != chains) {
      stop(
        "inv_metric must be a single metric or a list of length ",
        chains,
        " (one per chain)",
        call. = FALSE
      )
    }
  }

  # Build args list for .Call
  args <- list(
    method = "sampling",
    algorithm = algorithm,
    engine = if (algorithm == "hmc") engine else "nuts",
    metric = metric,
    adapt_engaged = as.logical(adapt_engaged),
    seed = as.integer(seed),
    chain_id = as.integer(chain_id),
    chains = as.integer(chains),
    init_radius = as.double(init_radius),
    num_warmup = as.integer(iter_warmup),
    num_samples = as.integer(iter_sampling),
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
    verbose = as.logical(verbose)
  )

  dat_ptr <- .Call(`r_data_context`, data)
  mod_ptr <- stanmod$new_model(dat_ptr, seed)
  withr::with_envvar(
    c(STAN_NUM_THREADS = 4),
    result <- .Call(`newstan_run`, mod_ptr, args)
  )

  # Handle non-zero return codes (CONFIG, SOFTWARE errors)
  if (result$return_code != 0) {
    return(
      list(
        draws = NULL,
        diagnostics = NULL,
        return_code = result$return_code,
        args = args
      )
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

  list(
    draws = posterior::subset_draws(draws, par_vars),
    diagnostics = diagnostics,
    return_code = result$return_code,
    args = args
  )
}
