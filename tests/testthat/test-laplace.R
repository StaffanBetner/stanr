test_that("laplace evaluates model-side gradients in the generated model library", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$laplace(
    data = data,
    mode = c(theta = 0.5),
    draws = 2,
    calculate_lp = TRUE,
    refresh = 0,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result, "StanLaplace")
  expect_s3_class(result$summary(), "draws_summary")
  expect_equal(posterior::ndraws(result$draws()), 2L)
})
