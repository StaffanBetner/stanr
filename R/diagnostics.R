# Warns about a count of problem transitions after MCMC sampling. Shared by
# `.stanr_check_divergences()` and `.stanr_check_max_treedepth()`, which
# differ only in `description` (completes "... transitions `description`.").
# `count` is per-chain, all-`NA` if not collected.
.stanr_check_transitions <- function(count, num_draws, description) {
  if (anyNA(count)) {
    return(invisible(NULL))
  }
  total <- sum(count)
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
    "%) transitions ",
    description,
    ".\n",
    "See https://mc-stan.org/misc/warnings for details."
  )
}

.stanr_check_divergences <- function(num_divergent, num_draws) {
  .stanr_check_transitions(
    num_divergent,
    num_draws,
    "ended with a divergence"
  )
}

.stanr_check_max_treedepth <- function(
  num_max_treedepth,
  num_draws,
  max_depth
) {
  .stanr_check_transitions(
    num_max_treedepth,
    num_draws,
    paste0("hit the maximum treedepth limit of ", max_depth)
  )
}

# Computes E-BFMI per chain and warns about problems. Unlike the
# divergence/treedepth checks, a chain that can't be computed at all (too
# few iterations, or missing/NA energy) is reported via `warning()` -- not
# suppressed by `quiet` -- since that's a different kind of problem than
# "computed fine but the value is bad". `draws` must already contain
# `energy__`.
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
