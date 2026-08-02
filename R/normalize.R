# Shared normalization and defaults layer for CmdStanR-aligned API.
#
# This module is the single source of truth for bundled Stan defaults and the
# normalized internal argument schema. Public signatures use NULL where the
# bundled default should be applied; the normalization functions resolve those
# NULLs to concrete values here.

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
    thin = 1L,
    max_treedepth = 10L,
    adapt_delta = 0.8,
    step_size = 1,
    metric = "diag_e",
    init_buffer = 75L,
    term_buffer = 50L,
    window = 25L,
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


#' Validate and resolve common arguments shared across services
#'
#' @noRd
.newstan_normalize_common <- function(
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
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


#' Validate and resolve chain count/IDs for sampling
#'
#' @noRd
.newstan_validate_chains <- function(chains, chain_ids) {
  chains <- as.integer(chains)
  if (chains < 1L) {
    stop("`chains` must be a positive integer.", call. = FALSE)
  }

  chain_ids <- as.integer(chain_ids)
  if (
    length(chain_ids) != chains ||
      anyNA(chain_ids) ||
      anyDuplicated(chain_ids) ||
      any(diff(chain_ids) != 1L)
  ) {
    stop(
      "The current backend requires `chain_ids` to be unique consecutive integers.",
      call. = FALSE
    )
  }

  list(chains = chains, chain_ids = chain_ids)
}


#' Validate and normalize inv_metric for sampling
#'
#' Wraps a single metric in a list (recycled across chains) or validates
#' a per-chain list. Issues a warning if inv_metric is supplied with unit_e.
#'
#' @noRd
.newstan_normalize_inv_metric <- function(
  inv_metric,
  metric,
  adapt_engaged,
  chains
) {
  if (is.null(inv_metric)) {
    return(NULL)
  }

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
      chains,
      " (one per chain).",
      call. = FALSE
    )
  }

  inv_metric
}


