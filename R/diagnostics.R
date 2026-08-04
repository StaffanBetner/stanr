#' Warn about divergent transitions after MCMC sampling
#'
#' Mirrors cmdstanr's `check_divergences()`.
#'
#' @param num_divergent (integer vector) Divergences per chain, or all-`NA` if
#'   not collected.
#' @param num_draws (integer) Total post-warmup draws across all chains.
#'
#' @noRd
.stanr_check_divergences <- function(num_divergent, num_draws) {
  if (anyNA(num_divergent)) {
    return(invisible(NULL))
  }
  total <- sum(num_divergent)
  if (total == 0) {
    return(invisible(NULL))
  }
  message(
    "[stanr] Warning: ",
    total,
    " of ",
    num_draws,
    " (",
    base::format(round(100 * total / num_draws, 0), nsmall = 1),
    "%) transitions ended with a divergence.\n",
    "See https://mc-stan.org/misc/warnings for details."
  )
}

#' Warn about transitions that hit the maximum treedepth
#'
#' Mirrors cmdstanr's `check_max_treedepth()`.
#'
#' @noRd
.stanr_check_max_treedepth <- function(
  num_max_treedepth,
  num_draws,
  max_depth
) {
  if (anyNA(num_max_treedepth)) {
    return(invisible(NULL))
  }
  total <- sum(num_max_treedepth)
  if (total == 0) {
    return(invisible(NULL))
  }
  message(
    "[stanr] Warning: ",
    total,
    " of ",
    num_draws,
    " (",
    base::format(round(100 * total / num_draws, 0), nsmall = 1),
    "%) transitions hit the maximum treedepth limit of ",
    max_depth,
    ".\n",
    "See https://mc-stan.org/misc/warnings for details."
  )
}

#' Compute E-BFMI per chain and warn about problems
#'
#' Mirrors cmdstanr's `ebfmi()`/`check_ebfmi()`. Unlike the divergence/
#' treedepth checks, a chain that can't be computed at all (too few
#' iterations, or missing/NA energy) is reported via `warning()` -- not
#' suppressed by `quiet` -- since that's a different kind of problem than
#' "computed fine but the value is bad".
#'
#' @param draws Sampler diagnostics as a `draws_array` (iteration x chain x
#'   variable), already known to contain `energy__`.
#' @param n_chains (integer) Number of chains.
#' @param quiet (logical) Suppress the below-threshold/NaN message?
#' @param threshold (number) E-BFMI values below this trigger a warning.
#'
#' @noRd
.stanr_check_ebfmi <- function(draws, n_chains, quiet, threshold = 0.3) {
  n_iter <- posterior::niterations(draws)
  if (n_iter < 3) {
    warning(
      "E-BFMI not computed because it is undefined for posterior chains ",
      "of length less than 3.",
      call. = FALSE
    )
    return(rep(NA_real_, n_chains))
  }
  energy <- vapply(
    seq_len(n_chains),
    function(i) as.numeric(draws[, i, "energy__"]),
    numeric(n_iter)
  )
  if (anyNA(energy)) {
    warning(
      "E-BFMI not computed because 'energy__' contains NAs.",
      call. = FALSE
    )
    return(rep(NA_real_, n_chains))
  }
  ebfmi <- apply(energy, 2, function(x) {
    (sum(diff(x)^2) / length(x)) / stats::var(x)
  })
  if (!quiet) {
    num_nan <- sum(is.nan(ebfmi))
    if (num_nan > 0) {
      message(
        "[stanr] Warning: ",
        num_nan,
        " of ",
        n_chains,
        " chains have a NaN E-BFMI.\n",
        "See https://mc-stan.org/misc/warnings for details."
      )
    } else if (sum(ebfmi < threshold) > 0) {
      message(
        "[stanr] Warning: ",
        sum(ebfmi < threshold),
        " of ",
        n_chains,
        " chains had an E-BFMI less than ",
        threshold,
        ".\n",
        "See https://mc-stan.org/misc/warnings for details."
      )
    }
  }
  unname(ebfmi)
}
