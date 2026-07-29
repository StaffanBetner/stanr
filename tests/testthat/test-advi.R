test_that("advi returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- advi(
    mod,
    data,
    iter = 1000,
    output_samples = 100,
    seed = 42,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("draws", "return_code", "args"))
  expect_s3_class(result$draws, "draws_df")
  expect_equal(result$return_code, 0L)
})

test_that("advi with meanfield algorithm works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- advi(
    mod,
    data,
    algorithm = "meanfield",
    iter = 1000,
    output_samples = 100,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
})
