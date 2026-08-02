# Shared normalization layer for CmdStanR-aligned API.
#
# This module holds the normalized internal argument schema shared across
# StanModel service methods. Bundled Stan defaults live directly in each
# service method's own signature.

#' Validate and resolve common arguments shared across services
#'
#' @noRd
.newstan_normalize_common <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  seed <- .newstan_seed(seed)

  list(
    data = data,
    seed = seed,
    refresh = as.integer(refresh),
    init = init,
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


