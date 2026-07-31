test_that("advi returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$variational(
    data = data,
    iter = 1000,
    draws = 100,
    seed = 42,
    show_messages = FALSE
  )

  expect_s3_class(result, "StanVB")
  expect_s3_class(result, "StanFit")
  expect_s3_class(result$draws(), "draws_matrix")
  expect_s3_class(result$summary(), "draws_summary")
  expect_equal(result$return_codes(), 0L)
})

test_that("advi with meanfield algorithm works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$variational(
    data = data,
    algorithm = "meanfield",
    iter = 1000,
    draws = 100,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
})
