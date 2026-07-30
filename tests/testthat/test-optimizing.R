test_that("optimizing returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- optimizing(mod, data, seed = 42, verbose = FALSE)

  expect_type(result, "list")
  expect_s3_class(result, "StanOptimize")
  expect_named(result, c("par", "value", "return_code", "args"))
  expect_type(result$par, "double")
  expect_true(is.numeric(result$value))
  expect_named(summary(result), c("variable", "estimate"))
  expect_equal(result$return_code, 0L)
})

test_that("optimizing with lbfgs algorithm works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- optimizing(
    mod,
    data,
    algorithm = "lbfgs",
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
})

test_that("optimizing finds reasonable theta for bernoulli", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  # 5 successes out of 10 -> MLE theta = 0.5
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- optimizing(mod, data, seed = 42, verbose = FALSE)

  expect_true(result$par["theta"] > 0.3)
  expect_true(result$par["theta"] < 0.7)
})
