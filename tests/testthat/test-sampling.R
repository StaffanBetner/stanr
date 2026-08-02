test_that("sampling returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  expect_s3_class(result, "StanMCMC")
  expect_s3_class(result, "StanFit")
  expect_s3_class(result$draws(), "draws_array")
  expect_s3_class(result$sampler_diagnostics(), "draws_array")
  expect_s3_class(suppressWarnings(result$summary()), "draws_summary")
  expect_true(all(result$return_codes() == 0L))
  expect_false(any(
    c("init", "inv_metric") %in%
      names(result$metadata()$arguments)
  ))
})

test_that("sampling with multiple chains works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 2,
    seed = 42,
    show_messages = FALSE
  )

  expect_true(all(result$return_codes() == 0L))
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with fixed_param algorithm works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 10,
    iter_sampling = 20,
    chains = 1,
    fixed_param = TRUE,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("sampling with adapt_engaged = FALSE works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    adapt_engaged = FALSE,
    step_size = 1,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("sampling with adapt_engaged = FALSE and iter_warmup = 0 works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 0,
    iter_sampling = 100,
    chains = 1,
    adapt_engaged = FALSE,
    step_size = 1,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("sampling with adapt_engaged = TRUE and iter_warmup = 0 fails with a clear message", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 0,
    iter_sampling = 100,
    chains = 1,
    adapt_engaged = TRUE,
    seed = 42,
    show_messages = FALSE
  )

  expect_true(all(result$return_codes() != 0L))
})

test_that("sampling with inv_metric (diag_e) works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # Bernoulli model has 1 unconstrained parameter (theta)
  # Identity inverse metric is just [1]
  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    metric = "diag_e",
    inv_metric = c(1.0),
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with save_warmup increases draws", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result_no_warmup <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = FALSE,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  result_warmup <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = TRUE,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(
    posterior::ndraws(result_no_warmup$draws(inc_warmup = TRUE)),
    posterior::ndraws(result_no_warmup$draws())
  )
  expect_gt(
    posterior::ndraws(result_warmup$draws(inc_warmup = TRUE)),
    posterior::ndraws(result_warmup$draws())
  )
})

test_that("sampling with static HMC engine works (unit_e, adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "unit_e",
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (unit_e, no adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "unit_e",
    adapt_engaged = FALSE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (diag_e, adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "diag_e",
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (diag_e, no adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "diag_e",
    adapt_engaged = FALSE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (dense_e, adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # Bernoulli model has 1 unconstrained parameter
  # Dense identity metric is just a 1x1 matrix
  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "dense_e",
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (dense_e, no adapt)", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "dense_e",
    adapt_engaged = FALSE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC + inv_metric (diag_e) works", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  # Bernoulli model has 1 unconstrained parameter
  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "diag_e",
    inv_metric = c(1.0),
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("static HMC with multiple chains throws a configuration error", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 50,
      iter_sampling = 50,
      chains = 2,
      engine = "static",
      seed = 42,
      show_messages = FALSE
    ),
    "Static HMC only supports a single chain"
  )
})

test_that("sampling with an invalid engine errors before reaching C++", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 50,
      iter_sampling = 50,
      chains = 1,
      engine = "typo",
      seed = 42,
      show_messages = FALSE
    ),
    "`engine` must be one of"
  )
})
