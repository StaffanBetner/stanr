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

#' Parse diagnose output messages into gradients data frame and lp value
#'
#' Stan's test_gradients() writes formatted strings to the parameter writer.
#' This function parses those strings to extract the log probability and
#' gradient check results.
#'
#' @noRd
.stanr_parse_diagnose_output <- function(lines) {
  lp <- NA_real_
  empty_gradients <- data.frame(
    param_idx = integer(0),
    value = double(0),
    model = double(0),
    finite_diff = double(0),
    error = double(0),
    check.names = FALSE
  )

  if (!is.character(lines) || length(lines) == 0) {
    return(list(gradients = empty_gradients, lp = lp))
  }

  # Accumulate matched gradient rows into plain vectors during the loop
  # (cheap append), and build the data.frame once at the end -- growing a
  # data.frame with per-row rbind() is O(n^2) for large gradient-check
  # output.
  n <- length(lines)
  param_idx <- integer(n)
  value <- double(n)
  model <- double(n)
  finite_diff <- double(n)
  error <- double(n)
  n_rows <- 0L

  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0) {
      next
    }

    if (startsWith(line, "Log probability=")) {
      lp_str <- sub("^Log probability=", "", line)
      lp_val <- suppressWarnings(as.numeric(lp_str))
      if (!is.na(lp_val)) {
        lp <- lp_val
      }
      next
    }

    if (grepl("param", line, fixed = TRUE)) {
      next
    }

    # Columns: param index, value, analytic ("model") gradient,
    # finite-difference gradient, and the absolute error between them.
    parts <- strsplit(line, "\\s+")[[1]]
    parts <- parts[nzchar(parts)]
    numeric_parts <- suppressWarnings(as.numeric(parts))

    if (length(numeric_parts) >= 5L && !any(is.na(numeric_parts))) {
      n_rows <- n_rows + 1L
      param_idx[n_rows] <- as.integer(numeric_parts[1L])
      value[n_rows] <- numeric_parts[2L]
      model[n_rows] <- numeric_parts[3L]
      finite_diff[n_rows] <- numeric_parts[4L]
      error[n_rows] <- numeric_parts[5L]
    }
  }

  gradients <- if (n_rows == 0L) {
    empty_gradients
  } else {
    data.frame(
      param_idx = param_idx[seq_len(n_rows)],
      value = value[seq_len(n_rows)],
      model = model[seq_len(n_rows)],
      finite_diff = finite_diff[seq_len(n_rows)],
      error = error[seq_len(n_rows)],
      check.names = FALSE
    )
  }

  list(gradients = gradients, lp = lp)
}
