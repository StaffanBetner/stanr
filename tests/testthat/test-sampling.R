test_that("sampling returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    iter_warmup = 100,
    iter_sampling = 100,
    chains = 1,
    seed = 42,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("draws", "diagnostics", "return_code", "args"))
  expect_s3_class(result$draws, "draws_df")
  expect_s3_class(result$diagnostics, "draws_df")
  expect_equal(result$return_code, 0L)
})

test_that("sampling with multiple chains works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 2,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with Fixed_param algorithm works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    iter_warmup = 10,
    iter_sampling = 20,
    chains = 1,
    algorithm = "Fixed_param",
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
})

test_that("sampling with save_warmup increases draws", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result_no_warmup <- sampling(
    mod,
    data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = FALSE,
    chains = 1,
    seed = 42,
    verbose = FALSE
  )

  result_warmup <- sampling(
    mod,
    data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = TRUE,
    chains = 1,
    seed = 42,
    verbose = FALSE
  )

  expect_true(nrow(result_warmup$draws) > nrow(result_no_warmup$draws))
})
