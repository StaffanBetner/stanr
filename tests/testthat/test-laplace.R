test_that("laplace evaluates model-side gradients in the generated model library", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- laplace(
    mod,
    data,
    mode = c(theta = 0.5),
    draws = 2,
    calculate_lp = TRUE,
    refresh = 0,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_s3_class(result, "StanLaplace")
  expect_s3_class(summary(result), "draws_summary")
  expect_equal(nrow(result$draws), 2L)
})
