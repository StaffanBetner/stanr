# Tests for internal helpers in R/classes-fit.R

test_that(".newstan_bracket_names converts dotted Stan names to bracket form", {
  expect_equal(newstan:::.newstan_bracket_names("alpha"), "alpha")
  expect_equal(newstan:::.newstan_bracket_names("beta.1"), "beta[1]")
  expect_equal(newstan:::.newstan_bracket_names("theta.2.3"), "theta[2,3]")
  expect_equal(newstan:::.newstan_bracket_names("x[1]"), "x[1]")
})


test_that(".newstan_bracket_names handles a vector mixing dotted and plain names", {
  input <- c("alpha", "beta.1", "theta.2.3", "x[1]")
  expected <- c("alpha", "beta[1]", "theta[2,3]", "x[1]")
  expect_equal(newstan:::.newstan_bracket_names(input), expected)
})


test_that(".newstan_bracket_names matches the old per-element implementation", {
  # Reference implementation: the pre-vectorization vapply/strsplit loop that
  # .newstan_bracket_names() used to be. Kept here only to check the
  # vectorized replacement produces identical output.
  old_bracket_names <- function(names) {
    vapply(
      names,
      function(name) {
        pieces <- strsplit(name, ".", fixed = TRUE)[[1]]
        if (length(pieces) == 1L) {
          return(name)
        }
        paste0(pieces[[1]], "[", paste(pieces[-1L], collapse = ","), "]")
      },
      character(1),
      USE.NAMES = FALSE
    )
  }

  set.seed(42)
  n <- 50000L
  bases <- paste0("par", seq_len(n))
  n_dims <- sample(1:3, n, replace = TRUE)
  dotted_suffix <- vapply(
    n_dims,
    function(k) paste(sample(1:20, k, replace = TRUE), collapse = "."),
    character(1)
  )
  names <- paste0(bases, ".", dotted_suffix)
  # Sprinkle in some already-plain (no-dot) names too.
  plain_idx <- sample(seq_len(n), size = n %/% 10)
  names[plain_idx] <- bases[plain_idx]

  new_result <- newstan:::.newstan_bracket_names(names)

  # Full correctness check against a hand-rolled reference implementation of
  # the OLD logic (not a timing comparison).
  expect_equal(new_result, old_bracket_names(names))

  # Spot-check a handful of entries against manually computed expectations.
  spot <- sample(seq_len(n), 20)
  for (i in spot) {
    nm <- names[i]
    pieces <- strsplit(nm, ".", fixed = TRUE)[[1]]
    expected <- if (length(pieces) == 1L) {
      nm
    } else {
      paste0(pieces[[1]], "[", paste(pieces[-1L], collapse = ","), "]")
    }
    expect_equal(new_result[i], expected)
  }
})


test_that("save_object() does not serialize the fit data twice", {
  # Regression test for B2a: `private$data_` and `private$metadata_$data`
  # used to both hold a reference to the same data list, and R serialization
  # does not deduplicate identical objects reachable via two different
  # fields, so `saveRDS()` wrote the data twice.
  set.seed(1)
  data <- list(x = stats::rnorm(1e6)) # ~8MB as doubles

  fit <- newstan:::StanFit$new(
    payload = list(),
    model = NULL,
    data = data,
    seed = 1L,
    elapsed = 0,
    metadata = list()
  )

  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  fit$save_object(file)

  fit_size <- file.info(file)$size
  one_copy_size <- length(serialize(data, connection = NULL))

  # If `data` were still stored twice, the saved file would be at least
  # ~2x one copy of `data`. With the fix it should be close to one copy
  # plus a small, fixed overhead for the rest of the (mostly empty) fit.
  expect_lt(fit_size, 1.5 * one_copy_size)

  # The public `$metadata()` contract is unchanged: `data` still round-trips
  # and the full field set is the same as before the fix.
  meta <- fit$metadata()
  expect_identical(meta$data, data)
  expect_identical(
    names(meta),
    c("seed", "data", "arguments", "model_name")
  )
})
