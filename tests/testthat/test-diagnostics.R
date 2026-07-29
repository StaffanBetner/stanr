test_that("gradient_check returns integer", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- suppressMessages(
    gradient_check(mod, data, seed = 42, verbose = FALSE)
  )

  expect_type(result, "integer")
})

test_that("gradient_check passes for bernoulli model", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- suppressMessages(
    gradient_check(mod, data, seed = 42, verbose = FALSE)
  )

  expect_equal(result, 0L)
})
