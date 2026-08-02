#' @noRd
.newstan_run_diagnose <- function(
  stanmod,
  data = NULL,
  seed = NULL,
  init = NULL,
  epsilon = NULL,
  error = NULL
) {
  common <- .newstan_normalize_common(data = data, seed = seed, init = init)
  def <- .newstan_defaults$diagnose
  epsilon <- epsilon %||% def$epsilon
  error <- error %||% def$error

  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0) {
    stop("`epsilon` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(error) || length(error) != 1L || error <= 0) {
    stop("`error` must be a positive number.", call. = FALSE)
  }

  native_args <- list(
    method = "diagnose",
    epsilon = as.double(epsilon),
    error = as.double(error),
    seed = as.integer(common$seed),
    id = 1L,
    init_radius = init_radius(common$init),
    verbose = TRUE,
    num_threads = 1L,
    init = normalize_init(common$init)
  )

  model <- stanmod$new_model(common$data, common$seed)

  withr::with_envvar(
    c(STAN_NUM_THREADS = 1),
    result <- stanmod$run_model(model, native_args)
  )

  n_failed <- as.integer(result$num_failed)
  output_lines <- result$output

  # Parse output messages from Stan's test_gradients()
  parsed <- .newstan_parse_diagnose_output(output_lines)

  if (n_failed == 0L) {
    message("[newstan] All gradient tests passed.")
  } else {
    message(sprintf(
      "[newstan] %d parameter(s) failed the gradient test.",
      n_failed
    ))
  }

  list(
    num_failed = n_failed,
    return_code = if (n_failed == 0L) 0L else 1L,
    gradients = parsed$gradients,
    lp = parsed$lp,
    output = output_lines,
    args = service_args(native_args),
    model_ptr = model
  )
}


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
