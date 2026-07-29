skip()

test_that("generated_quantities returns expected structure", {
  path <- test_path("test-models/bernoulli_gqs.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # First get some posterior draws via sampling
  samp <- sampling(
    mod,
    data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    seed = 42,
    verbose = FALSE
  )

  # Extract numeric matrix from draws (drop posterior dim columns)
  draws_mat <- as.matrix(posterior::as_draws_matrix(samp$draws))

  result <- generated_quantities(
    mod,
    data,
    draws_mat,
    seed = 42,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("draws", "return_code", "args"))
  expect_s3_class(result$draws, "draws_df")
  expect_equal(result$return_code, 0L)
})

test_that("generated_quantities produces log_lik column", {
  path <- test_path("test-models/bernoulli_gqs.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  samp <- sampling(
    mod,
    data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    seed = 42,
    verbose = FALSE
  )

  draws_mat <- as.matrix(posterior::as_draws_matrix(samp$draws))

  result <- generated_quantities(
    mod,
    data,
    draws_mat,
    seed = 42,
    verbose = FALSE
  )

  expect_true("log_lik" %in% colnames(result$draws))
})
