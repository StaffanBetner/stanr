test_that("optimizing returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$optimize(data = data, seed = 42, show_messages = FALSE)

  expect_s3_class(result, "StanMLE")
  expect_s3_class(result, "StanFit")
  expect_type(result$mle(), "double")
  expect_named(result$summary(), c("variable", "estimate"))
  expect_equal(result$return_codes(), 0L)
})

test_that("optimizing with lbfgs algorithm works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$optimize(
    data = data,
    algorithm = "lbfgs",
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("optimizing finds reasonable theta for bernoulli", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  # 5 successes out of 10 -> MLE theta = 0.5
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$optimize(data = data, seed = 42, show_messages = FALSE)

  expect_true(result$mle("theta") > 0.3)
  expect_true(result$mle("theta") < 0.7)
})

test_that("optimizing output() returns non-empty Stan log messages", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$optimize(data = data, seed = 42, show_messages = FALSE)

  expect_type(result$output(), "character")
  expect_true(length(result$output()) > 0)
})

test_that("optimizing with jacobian = TRUE vs FALSE gives different mle() for constrained parameters", {
  path <- test_path("test-models/sigma_normal.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 5, y = c(0.8, -1.2, 0.5, 1.7, -0.3))

  result_no_jacobian <- mod$optimize(
    data = data,
    seed = 42,
    jacobian = FALSE,
    show_messages = FALSE
  )
  result_jacobian <- mod$optimize(
    data = data,
    seed = 42,
    jacobian = TRUE,
    show_messages = FALSE
  )

  expect_equal(result_no_jacobian$return_codes(), 0L)
  expect_equal(result_jacobian$return_codes(), 0L)
  expect_false(isTRUE(all.equal(
    result_no_jacobian$mle(),
    result_jacobian$mle()
  )))
})

test_that("optimizing with save_iterations = TRUE exposes the full optimization path via draws()", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$optimize(
    data = data,
    seed = 42,
    save_iterations = TRUE,
    show_messages = FALSE
  )
  result_default <- mod$optimize(data = data, seed = 42, show_messages = FALSE)

  expect_equal(result$return_codes(), 0L)
  draws <- unclass(as.matrix(result$draws()))
  expect_true(nrow(draws) > 1)

  last_row <- draws[nrow(draws), ]
  expect_equal(
    unname(last_row[names(result$mle())]),
    unname(result$mle())
  )
  # Saving iterations should not change the converged estimate: the last row
  # of the full path should match the single-row output of a default run
  # with the same seed/data.
  default_row <- unclass(as.matrix(result_default$draws()))[1L, ]
  expect_equal(unname(last_row[names(default_row)]), unname(default_row))
})

test_that("optimizing default save_iterations yields a single row in draws()", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$optimize(data = data, seed = 42, show_messages = FALSE)

  draws <- unclass(as.matrix(result$draws()))
  expect_equal(nrow(draws), 1L)
})
