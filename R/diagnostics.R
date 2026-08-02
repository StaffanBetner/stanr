#' Parse diagnose output messages into gradients data frame and lp value
#'
#' Stan's test_gradients() writes formatted strings to the parameter writer.
#' This function parses those strings to extract the log probability and
#' gradient check results.
#'
#' @noRd
.newstan_parse_diagnose_output <- function(lines) {
  lp <- NA_real_
  gradients <- data.frame(
    param_idx = integer(0),
    value = double(0),
    model = double(0),
    finite_diff = double(0),
    error = double(0),
    check.names = FALSE
  )

  if (!is.character(lines) || length(lines) == 0) {
    return(list(gradients = gradients, lp = lp))
  }

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
      gradients <- rbind(
        gradients,
        data.frame(
          param_idx = as.integer(numeric_parts[1L]),
          value = numeric_parts[2L],
          model = numeric_parts[3L],
          finite_diff = numeric_parts[4L],
          error = numeric_parts[5L],
          check.names = FALSE
        )
      )
    }
  }

  list(gradients = gradients, lp = lp)
}
