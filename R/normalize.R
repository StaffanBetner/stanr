# Shared normalization layer for CmdStanR-aligned API.
#
# This module holds normalization helpers shared across StanModel service
# methods that aren't part of the single shared execution path in
# `.newstan_run_service()` (see R/classes-model.R). Bundled Stan defaults
# live directly in each service method's own signature.

#' Validate and resolve chain count/IDs for sampling
#'
#' @noRd
.newstan_validate_chains <- function(chains, chain_ids) {
  chains <- .newstan_int(chains, "chains", min = 1L)
  if (
    !is.numeric(chain_ids) ||
      length(chain_ids) != chains ||
      anyNA(chain_ids) ||
      any(chain_ids != floor(chain_ids))
  ) {
    stop(
      "`chain_ids` must be ",
      chains,
      " integer value(s).",
      call. = FALSE
    )
  }
  chain_ids <- as.integer(chain_ids)
  if (anyDuplicated(chain_ids) || any(diff(chain_ids) != 1L)) {
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

  if (!is.list(inv_metric)) {
    inv_metric <- list(inv_metric)
  }

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
