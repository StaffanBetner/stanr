test_that("sampling returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 100,
    num_samples = 100,
    num_chains = 1,
    seed = 42,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_s3_class(result, "StanSample")
  expect_named(result, c("draws", "diagnostics", "return_code", "args"))
  expect_s3_class(result$draws, "draws_df")
  expect_s3_class(result$diagnostics, "draws_df")
  expect_s3_class(summary(result), "draws_summary")
  expect_equal(result$return_code, 0L)
  expect_false(any(c("init", "inv_metric") %in% names(result$args)))
})

test_that("sampling with multiple chains works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 2,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with fixed_param algorithm works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 10,
    num_samples = 20,
    num_chains = 1,
    algorithm = "fixed_param",
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
})

test_that("sampling with adapt_engaged = FALSE works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "nuts",
    adapt_engaged = FALSE,
    stepsize = 1,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
})

test_that("sampling with inv_metric (diag_e) works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # Bernoulli model has 1 unconstrained parameter (theta)
  # Identity inverse metric is just [1]
  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "nuts",
    metric = "diag_e",
    inv_metric = c(1.0),
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with save_warmup increases draws", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result_no_warmup <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    save_warmup = FALSE,
    num_chains = 1,
    seed = 42,
    verbose = FALSE
  )

  result_warmup <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    save_warmup = TRUE,
    num_chains = 1,
    seed = 42,
    verbose = FALSE
  )

  expect_true(nrow(result_warmup$draws) > nrow(result_no_warmup$draws))
})

test_that("sampling with static HMC engine works (unit_e, adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "static",
    metric = "unit_e",
    adapt_engaged = TRUE,
    stepsize = 1,
    int_time = 10,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with static HMC engine works (unit_e, no adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "static",
    metric = "unit_e",
    adapt_engaged = FALSE,
    stepsize = 1,
    int_time = 10,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with static HMC engine works (diag_e, adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "static",
    metric = "diag_e",
    adapt_engaged = TRUE,
    stepsize = 1,
    int_time = 10,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with static HMC engine works (diag_e, no adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "static",
    metric = "diag_e",
    adapt_engaged = FALSE,
    stepsize = 1,
    int_time = 10,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with static HMC engine works (dense_e, adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # Bernoulli model has 1 unconstrained parameter
  # Dense identity metric is just a 1x1 matrix
  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "static",
    metric = "dense_e",
    adapt_engaged = TRUE,
    stepsize = 1,
    int_time = 10,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with static HMC engine works (dense_e, no adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "static",
    metric = "dense_e",
    adapt_engaged = FALSE,
    stepsize = 1,
    int_time = 10,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("sampling with static HMC + inv_metric (diag_e) works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # Bernoulli model has 1 unconstrained parameter
  result <- sampling(
    mod,
    data,
    num_warmup = 50,
    num_samples = 50,
    num_chains = 1,
    algorithm = "hmc",
    engine = "static",
    metric = "diag_e",
    inv_metric = c(1.0),
    adapt_engaged = TRUE,
    stepsize = 1,
    int_time = 10,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(nrow(result$draws) > 0)
})

test_that("static HMC with multiple chains throws a configuration error", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  expect_error(
    sampling(
      mod,
      data,
      num_warmup = 50,
      num_samples = 50,
      num_chains = 2,
      algorithm = "hmc",
      engine = "static",
      seed = 42,
      verbose = FALSE
    ),
    "Static HMC only supports a single chain"
  )
})
