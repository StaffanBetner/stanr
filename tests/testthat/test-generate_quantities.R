test_that("generated_quantities returns expected structure", {
  mod <- test_model("bernoulli_gqs")
  data <- bernoulli_data

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
  mod <- test_model("bernoulli_gqs")
  data <- bernoulli_data

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

test_that("generated_quantities matches bracket-normalized names for vector parameters", {
  mod <- test_model("vector_gqs")
  data <- list(N = 5, y = c(0.5, -0.2, 0.1, 0.3, -0.4))

  samp <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  result <- mod$generate_quantities(
    fitted_params = samp,
    data = data,
    seed = 42,
    show_messages = FALSE
  )

  expect_true("mu_sum" %in% posterior::variables(result$draws()))
  expect_equal(result$return_codes(), 0L)
})

test_that("generated_quantities errors on an unnamed draws matrix", {
  mod <- test_model("vector_gqs")
  data <- list(N = 5, y = c(0.5, -0.2, 0.1, 0.3, -0.4))

  samp <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  draws_mat <- samp$draws(format = "draws_matrix")
  plain <- unclass(unname(draws_mat))

  expect_error(
    mod$generate_quantities(
      fitted_params = plain,
      data = data,
      seed = 42,
      show_messages = FALSE
    ),
    "mu\\[1\\]"
  )
})
