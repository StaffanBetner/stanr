test_that("gradient_check returns integer", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- suppressMessages(mod$diagnose(data = data, seed = 42))

  expect_s3_class(result, "StanDiagnose")
  expect_s3_class(result, "StanFit")
  expect_type(result$num_failed(), "integer")
  expect_equal(result$return_codes(), 0L)
})

test_that("gradient_check passes for bernoulli model", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- suppressMessages(mod$diagnose(data = data, seed = 42))

  expect_equal(result$num_failed(), 0L)
})
