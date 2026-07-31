test_that("generated_quantities returns expected structure", {
  path <- test_path("test-models/bernoulli_gqs.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # First get some posterior draws via sampling
  samp <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  # Extract numeric matrix from draws (drop posterior dim columns)
  draws_mat <- posterior::as_draws_matrix(samp$draws())

  result <- mod$generate_quantities(
    fitted_params = draws_mat,
    data = data,
    seed = 42,
    show_messages = FALSE
  )

  expect_s3_class(result, "StanGQ")
  expect_s3_class(result, "StanFit")
  expect_s3_class(result$draws(), "draws_array")
  expect_s3_class(result$summary(), "draws_summary")
  expect_equal(result$return_codes(), 0L)
  expect_false("fitted_params" %in% names(result$metadata()$arguments))
})

test_that("generated_quantities produces log_lik column", {
  path <- test_path("test-models/bernoulli_gqs.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  samp <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  draws_mat <- posterior::as_draws_matrix(samp$draws())

  result <- mod$generate_quantities(
    fitted_params = draws_mat,
    data = data,
    seed = 42,
    show_messages = FALSE
  )

  expect_true("log_lik" %in% posterior::variables(result$draws()))
})
