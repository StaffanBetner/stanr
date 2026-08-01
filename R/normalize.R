# Shared normalization and defaults layer for CmdStanR-aligned API.
#
# This module is the single source of truth for bundled Stan defaults and the
# normalized internal argument schema. Public signatures use NULL where the
# bundled default should be applied; the normalization functions resolve those
# NULLs to concrete values here.
#
# The native run-result schema is also defined here as a documented standard.
# See .newstan_run_result_schema() for the expected shape.

#' Bundled Stan defaults for each service
#'
#' These values match Stan 2.39 defaults where applicable. They are the
#' authoritative source; do not duplicate them in StanModel methods.
#'
#' @noRd
.newstan_defaults <- list(
  sample = list(
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    chains = 4L,
    save_warmup = FALSE,
    thin = 1L,
    max_treedepth = 10L,
    adapt_engaged = TRUE,
    adapt_delta = 0.8,
    step_size = 1,
    metric = "diag_e",
    init_buffer = 75L,
    term_buffer = 50L,
    window = 25L,
    fixed_param = FALSE,
    refresh = 100L,
    # newstan extensions
    engine = "nuts",
    int_time = 2 * pi,
    step_size_jitter = 0,
    adapt_gamma = 0.05,
    adapt_kappa = 0.75,
    adapt_t0 = 10
  ),
  optimize = list(
    algorithm = "lbfgs",
    iter = 2000L,
    init_alpha = 0.001,
    tol_obj = 1e-12,
    tol_rel_obj = 1e4,
    tol_grad = 1e-8,
    tol_rel_grad = 1e7,
    tol_param = 1e-8,
    history_size = 5L,
    jacobian = FALSE,
    refresh = 100L
  ),
  laplace = list(
    jacobian = TRUE,
    draws = 1000L,
    calculate_lp = TRUE,
    refresh = 100L
  ),
  variational = list(
    algorithm = "meanfield",
    iter = 10000L,
    grad_samples = 1L,
    elbo_samples = 100L,
    tol_rel_obj = 0.01,
    eta = 1,
    adapt_engaged = TRUE,
    adapt_iter = 50L,
    eval_elbo = 100L,
    draws = 1000L,
    refresh = 100L
  ),
  pathfinder = list(
    max_lbfgs_iters = 1000L,
    history_size = 5L,
    num_elbo_draws = 25L,
    single_path_draws = 1000L,
    num_paths = 4L,
    draws = 1000L,
    init_alpha = 0.001,
    tol_obj = 1e-12,
    tol_rel_obj = 1e4,
    tol_grad = 1e-8,
    tol_rel_grad = 1e7,
    tol_param = 1e-8,
    save_single_paths = FALSE,
    psis_resample = TRUE,
    calculate_lp = TRUE,
    refresh = 100L
  ),
  diagnose = list(
    epsilon = 1e-6,
    error = 1e-6
  )
)


#' Resolve NULL to the bundled default for a service argument
#'
#' @noRd
.resolve_default <- function(value, defaults, name) {
  if (is.null(value)) defaults[[name]] else value
}


#' Validate and resolve common arguments shared across services
#'
#' Returns a list of normalized values. Callers should use `dplyl::list_modify`
#' or inline `c()` to merge with service-specific arguments.
#'
#' @noRd
.newstan_normalize_common <- function(data = NULL, seed = NULL,
                                       refresh = NULL, init = NULL,
                                       show_messages = TRUE,
                                       show_exceptions = TRUE) {
  data <- data %||% list()
  seed <- .newstan_seed(seed)
  refresh <- as.integer(refresh %||% 100L)
  init_value <- init %||% 2

  list(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init_value,
    show_messages = isTRUE(show_messages),
    show_exceptions = isTRUE(show_exceptions)
  )
}


#' Validate and resolve sampling arguments to internal schema
#'
#' @noRd
.newstan_normalize_sample <- function(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  chains = 4, chain_ids = seq_len(chains),
  parallel_chains = getOption("mc.cores", 1),
  threads_per_chain = NULL, iter_warmup = NULL, iter_sampling = NULL,
  save_warmup = FALSE, thin = NULL, max_treedepth = NULL,
  adapt_engaged = TRUE, adapt_delta = NULL, step_size = NULL,
  metric = NULL, inv_metric = NULL,
  init_buffer = NULL, term_buffer = NULL, window = NULL,
  fixed_param = FALSE, show_messages = TRUE, show_exceptions = TRUE,
  engine = "nuts", int_time = 2 * pi, step_size_jitter = 0,
  adapt_gamma = 0.05, adapt_kappa = 0.75, adapt_t0 = 10
) {
  def <- .newstan_defaults$sample

  chains <- as.integer(chains)
  if (chains < 1L) stop("`chains` must be a positive integer.", call. = FALSE)

  chain_ids <- as.integer(chain_ids)
  if (length(chain_ids) != chains || anyNA(chain_ids) ||
      anyDuplicated(chain_ids) || any(diff(chain_ids) != 1L)) {
    stop("The current backend requires `chain_ids` to be unique consecutive integers.",
         call. = FALSE)
  }

  parallel_chains <- as.integer(parallel_chains %||% 1L)
  threads_per_chain <- as.integer(threads_per_chain %||% 1L)
  if (parallel_chains < 1L || threads_per_chain < 1L) {
    stop("`parallel_chains` and `threads_per_chain` must be positive.",
         call. = FALSE)
  }

  resolved_metric <- .resolve_default(metric, def, "metric")
  resolved_adapt_engaged <- isTRUE(adapt_engaged)
  resolved_fixed_param <- isTRUE(fixed_param)
  resolved_engine <- .resolve_default(engine, def, "engine")
  resolved_iter_warmup <- as.integer(.resolve_default(iter_warmup, def, "iter_warmup"))
  resolved_iter_sampling <- as.integer(.resolve_default(iter_sampling, def, "iter_sampling"))
  resolved_thin <- as.integer(.resolve_default(thin, def, "thin"))
  resolved_save_warmup <- isTRUE(save_warmup)

  if (resolved_iter_warmup < 0 || resolved_iter_sampling < 0) {
    stop("num_warmup and num_samples must be non-negative.", call. = FALSE)
  }
  if (resolved_thin < 1L) {
    stop("thin must be at least 1.", call. = FALSE)
  }
  # Calculate in double precision so adding two valid integer iteration counts
  # cannot overflow to NA before the explicit R-size guard below.
  num_saved_draws <- ceiling(as.double(resolved_iter_sampling) / as.double(resolved_thin))
  if (resolved_save_warmup) {
    num_saved_draws <- num_saved_draws +
      ceiling(as.double(resolved_iter_warmup) / as.double(resolved_thin))
  }
  if (num_saved_draws > .Machine$integer.max) {
    stop("Requested number of saved draws is too large.", call. = FALSE)
  }

  if (!resolved_adapt_engaged && !resolved_fixed_param &&
      resolved_iter_warmup == 0L) {
    stop("`iter_warmup` must be > 0 when `adapt_engaged` is TRUE.",
         call. = FALSE)
  }

  if (!resolved_fixed_param && resolved_engine == "static" && chains > 1L) {
    stop("Static HMC only supports a single chain. Set `chains` = 1.",
         call. = FALSE)
  }

  # Normalize inv_metric: validate and wrap for native consumption
  inv_metric <- .newstan_normalize_inv_metric(
    inv_metric = inv_metric,
    metric = resolved_metric,
    adapt_engaged = resolved_adapt_engaged,
    chains = chains
  )

  list(
    data = data %||% list(),
    seed = .newstan_seed(seed),
    refresh = as.integer(.resolve_default(refresh, def, "refresh")),
    init = init %||% 2,
    chains = chains,
    chain_ids = chain_ids,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain,
    iter_warmup = resolved_iter_warmup,
    iter_sampling = resolved_iter_sampling,
    save_warmup = resolved_save_warmup,
    thin = resolved_thin,
    max_treedepth = as.integer(.resolve_default(max_treedepth, def, "max_treedepth")),
    adapt_engaged = resolved_adapt_engaged,
    adapt_delta = .resolve_default(adapt_delta, def, "adapt_delta"),
    step_size = .resolve_default(step_size, def, "step_size"),
    metric = resolved_metric,
    inv_metric = inv_metric,
    init_buffer = as.integer(.resolve_default(init_buffer, def, "init_buffer")),
    term_buffer = as.integer(.resolve_default(term_buffer, def, "term_buffer")),
    window = as.integer(.resolve_default(window, def, "window")),
    fixed_param = resolved_fixed_param,
    show_messages = isTRUE(show_messages),
    show_exceptions = isTRUE(show_exceptions),
    engine = resolved_engine,
    int_time = .resolve_default(int_time, def, "int_time"),
    step_size_jitter = .resolve_default(step_size_jitter, def, "step_size_jitter"),
    adapt_gamma = .resolve_default(adapt_gamma, def, "adapt_gamma"),
    adapt_kappa = .resolve_default(adapt_kappa, def, "adapt_kappa"),
    adapt_t0 = .resolve_default(adapt_t0, def, "adapt_t0")
  )
}


#' Validate and normalize inv_metric for sampling
#'
#' Wraps a single metric in a list (recycled across chains) or validates
#' a per-chain list. Issues a warning if inv_metric is supplied with unit_e.
#'
#' @noRd
.newstan_normalize_inv_metric <- function(inv_metric, metric, adapt_engaged, chains) {
  if (is.null(inv_metric)) return(NULL)

  if (metric == "unit_e") {
    warning("inv_metric is ignored when metric = 'unit_e'", call. = FALSE)
    return(NULL)
  }

  # Wrap single metric in list for recycling across chains
  if (!is.list(inv_metric)) {
    inv_metric <- list(inv_metric)
  }

  # Validate per-chain length
  if (length(inv_metric) > 1L && length(inv_metric) != chains) {
    stop(
      "inv_metric must be a single metric or a list of length ",
      chains, " (one per chain).",
      call. = FALSE
    )
  }

  inv_metric
}


#' Validate and resolve optimization arguments to internal schema
#'
#' @noRd
.newstan_normalize_optimize <- function(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  threads = NULL, algorithm = NULL, jacobian = FALSE,
  init_alpha = NULL, iter = NULL, tol_obj = NULL,
  tol_rel_obj = NULL, tol_grad = NULL, tol_rel_grad = NULL,
  tol_param = NULL, history_size = NULL,
  show_messages = TRUE, show_exceptions = TRUE
) {
  def <- .newstan_defaults$optimize

  list(
    data = data %||% list(),
    seed = .newstan_seed(seed),
    refresh = as.integer(.resolve_default(refresh, def, "refresh")),
    init = init %||% 2,
    threads = as.integer(threads %||% 1L),
    algorithm = .resolve_default(algorithm, def, "algorithm"),
    jacobian = isTRUE(jacobian),
    init_alpha = .resolve_default(init_alpha, def, "init_alpha"),
    iter = as.integer(.resolve_default(iter, def, "iter")),
    tol_obj = .resolve_default(tol_obj, def, "tol_obj"),
    tol_rel_obj = .resolve_default(tol_rel_obj, def, "tol_rel_obj"),
    tol_grad = .resolve_default(tol_grad, def, "tol_grad"),
    tol_rel_grad = .resolve_default(tol_rel_grad, def, "tol_rel_grad"),
    tol_param = .resolve_default(tol_param, def, "tol_param"),
    history_size = as.integer(.resolve_default(history_size, def, "history_size")),
    show_messages = isTRUE(show_messages),
    show_exceptions = isTRUE(show_exceptions)
  )
}


#' Validate and resolve Laplace arguments to internal schema
#'
#' @noRd
.newstan_normalize_laplace <- function(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  threads = NULL, mode = NULL, opt_args = NULL,
  jacobian = TRUE, draws = NULL, calculate_lp = TRUE,
  show_messages = TRUE, show_exceptions = TRUE
) {
  def <- .newstan_defaults$laplace

  if (!is.null(mode) && !is.null(opt_args)) {
    stop("`mode` and `opt_args` cannot both be supplied.", call. = FALSE)
  }

  list(
    data = data %||% list(),
    seed = .newstan_seed(seed),
    refresh = as.integer(.resolve_default(refresh, def, "refresh")),
    init = init %||% 2,
    threads = as.integer(threads %||% 1L),
    mode = mode,
    opt_args = opt_args %||% list(),
    jacobian = isTRUE(jacobian),
    draws = as.integer(.resolve_default(draws, def, "draws")),
    calculate_lp = isTRUE(calculate_lp),
    show_messages = isTRUE(show_messages),
    show_exceptions = isTRUE(show_exceptions)
  )
}


#' Validate and resolve ADVI arguments to internal schema
#'
#' @noRd
.newstan_normalize_variational <- function(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  threads = NULL, algorithm = NULL, iter = NULL,
  grad_samples = NULL, elbo_samples = NULL, tol_rel_obj = NULL,
  eta = NULL, adapt_engaged = NULL, adapt_iter = NULL,
  eval_elbo = NULL, draws = NULL,
  show_messages = TRUE, show_exceptions = TRUE
) {
  def <- .newstan_defaults$variational

  list(
    data = data %||% list(),
    seed = .newstan_seed(seed),
    refresh = as.integer(.resolve_default(refresh, def, "refresh")),
    init = init %||% 2,
    threads = as.integer(threads %||% 1L),
    algorithm = .resolve_default(algorithm, def, "algorithm"),
    iter = as.integer(.resolve_default(iter, def, "iter")),
    grad_samples = as.integer(.resolve_default(grad_samples, def, "grad_samples")),
    elbo_samples = as.integer(.resolve_default(elbo_samples, def, "elbo_samples")),
    tol_rel_obj = .resolve_default(tol_rel_obj, def, "tol_rel_obj"),
    eta = .resolve_default(eta, def, "eta"),
    adapt_engaged = .resolve_default(adapt_engaged, def, "adapt_engaged"),
    adapt_iter = as.integer(.resolve_default(adapt_iter, def, "adapt_iter")),
    eval_elbo = as.integer(.resolve_default(eval_elbo, def, "eval_elbo")),
    draws = as.integer(.resolve_default(draws, def, "draws")),
    show_messages = isTRUE(show_messages),
    show_exceptions = isTRUE(show_exceptions)
  )
}


#' Validate and resolve Pathfinder arguments to internal schema
#'
#' @noRd
.newstan_normalize_pathfinder <- function(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  threads = NULL, init_alpha = NULL, tol_obj = NULL,
  tol_rel_obj = NULL, tol_grad = NULL, tol_rel_grad = NULL,
  tol_param = NULL, history_size = NULL,
  single_path_draws = NULL, draws = NULL, num_paths = 4,
  max_lbfgs_iters = NULL, num_elbo_draws = NULL,
  save_single_paths = NULL, psis_resample = NULL,
  calculate_lp = NULL,
  show_messages = TRUE, show_exceptions = TRUE
) {
  def <- .newstan_defaults$pathfinder

  list(
    data = data %||% list(),
    seed = .newstan_seed(seed),
    refresh = as.integer(.resolve_default(refresh, def, "refresh")),
    init = init %||% 2,
    threads = as.integer(threads %||% 1L),
    init_alpha = .resolve_default(init_alpha, def, "init_alpha"),
    tol_obj = .resolve_default(tol_obj, def, "tol_obj"),
    tol_rel_obj = .resolve_default(tol_rel_obj, def, "tol_rel_obj"),
    tol_grad = .resolve_default(tol_grad, def, "tol_grad"),
    tol_rel_grad = .resolve_default(tol_rel_grad, def, "tol_rel_grad"),
    tol_param = .resolve_default(tol_param, def, "tol_param"),
    history_size = as.integer(.resolve_default(history_size, def, "history_size")),
    single_path_draws = as.integer(.resolve_default(single_path_draws, def, "single_path_draws")),
    draws = as.integer(.resolve_default(draws, def, "draws")),
    num_paths = as.integer(num_paths),
    max_lbfgs_iters = as.integer(.resolve_default(max_lbfgs_iters, def, "max_lbfgs_iters")),
    num_elbo_draws = as.integer(.resolve_default(num_elbo_draws, def, "num_elbo_draws")),
    save_single_paths = .resolve_default(save_single_paths, def, "save_single_paths"),
    psis_resample = .resolve_default(psis_resample, def, "psis_resample"),
    calculate_lp = .resolve_default(calculate_lp, def, "calculate_lp"),
    show_messages = isTRUE(show_messages),
    show_exceptions = isTRUE(show_exceptions)
  )
}


#' Validate and resolve diagnose arguments to internal schema
#'
#' @noRd
.newstan_normalize_diagnose <- function(
  data = NULL, seed = NULL, init = NULL,
  epsilon = NULL, error = NULL
) {
  def <- .newstan_defaults$diagnose

  list(
    data = data %||% list(),
    seed = .newstan_seed(seed),
    init = init %||% 2,
    epsilon = .resolve_default(epsilon, def, "epsilon"),
    error = .resolve_default(error, def, "error")
  )
}


#' Native run-result schema
#'
#' All services should return a list matching this schema. Non-applicable
#' entries are left as empty lists/vectors. This keeps R6 construction
#' and failure handling uniform across services.
#'
#' ```r
#' list(
#'   method = "sample",                    # service name
#'   draws = list(post_warmup = ...,       # post-warmup draws (data frame or matrix)
#'                warmup = ...),            # warmup draws (NULL if not saved)
#'   sampler_diagnostics = list(           # sampler diagnostics (MCMC only)
#'     post_warmup = ...,
#'     warmup = ...),
#'   return_codes = integer(),             # per-chain/per-run return codes
#'   chain_ids = integer(),                # chain IDs used
#'   init = list(user = ...,               # user-specified init
#'               actual = ...),            # actual init values from native
#'   metric = list(),                      # adapted metrics (MCMC)
#'   step_size = numeric(),                # adapted step sizes (MCMC)
#'   timing = list(total = ...,            # wall-clock seconds
#'                 chains = ...),           # per-chain timing (MCMC)
#'   messages = character(),               # retained output messages
#'   profiles = list(),                    # profiling data (P2)
#'   structured = list(),                  # service-specific structured output
#'   metadata = list()                     # additional metadata
#' )
#' ```
#'
#' @noRd
.newstan_run_result_schema <- function(method = "sample") {
  list(
    method = method,
    draws = list(post_warmup = NULL, warmup = NULL),
    sampler_diagnostics = list(post_warmup = NULL, warmup = NULL),
    return_codes = integer(),
    chain_ids = integer(),
    init = list(user = NULL, actual = NULL),
    metric = list(),
    step_size = numeric(),
    timing = list(total = NA_real_, chains = list()),
    messages = character(),
    profiles = list(),
    structured = list(),
    metadata = list()
  )
}
