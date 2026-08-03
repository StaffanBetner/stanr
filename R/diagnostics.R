#' Parse diagnose output messages into gradients data frame and lp value
#'
#' Stan's test_gradients() writes formatted strings to the parameter writer.
#' This function parses those strings to extract the log probability and
#' gradient check results.
#'
#' @noRd
.newstan_parse_diagnose_output <- function(lines) {
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

    # Extract log probability
    if (startsWith(line, "Log probability=")) {
      lp_str <- sub("^Log probability=", "", line)
      lp_val <- suppressWarnings(as.numeric(lp_str))
      if (!is.na(lp_val)) {
        lp <- lp_val
      }
      next
    }

    # Skip header line
    if (grepl("param", line, fixed = TRUE)) {
      next
    }

    # Parse gradient row: space-separated numeric values
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
