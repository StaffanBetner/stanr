# Tests for internal helpers in R/classes-fit.R

test_that(".newstan_bracket_names converts dotted Stan names to bracket form", {
  expect_equal(newstan:::.newstan_bracket_names("alpha"), "alpha")
  expect_equal(newstan:::.newstan_bracket_names("beta.1"), "beta[1]")
  expect_equal(newstan:::.newstan_bracket_names("theta.2.3"), "theta[2,3]")
  expect_equal(newstan:::.newstan_bracket_names("x[1]"), "x[1]")
})


test_that(".newstan_bracket_names handles a vector mixing dotted and plain names", {
  input <- c("alpha", "beta.1", "theta.2.3", "x[1]", "par.12.3", "sigma123")
  expected <- c(
    "alpha",
    "beta[1]",
    "theta[2,3]",
    "x[1]",
    "par[12,3]",
    "sigma123"
  )
  expect_equal(newstan:::.newstan_bracket_names(input), expected)
})


test_that("optimize() fit errors on draws(inc_warmup = TRUE)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(data = data, seed = 42, show_messages = FALSE)

  expect_error(
    result$draws(inc_warmup = TRUE),
    "warmup draws were not saved"
  )
})


test_that("save_object() does not serialize the fit data twice", {
  # Regression test: `private$data_` and `private$metadata_$data` used to
  # both hold a reference to the same data list, and R serialization does
  # not deduplicate identical objects reachable via two different fields,
  # so `saveRDS()` wrote the data twice.
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
