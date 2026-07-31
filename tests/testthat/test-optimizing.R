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
