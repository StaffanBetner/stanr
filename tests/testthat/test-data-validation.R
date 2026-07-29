test_that("numeric values are accepted for Stan integer data only when integral", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)

  result <- sampling(
    mod,
    data = list(N = 4, y = c(1, 0, 1, 0)),
    num_warmup = 5,
    num_samples = 5,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)

  expect_error(
    sampling(
      mod,
      data = list(N = 4, y = c(1, 0.5, 1, 0)),
      num_warmup = 5,
      num_samples = 5,
      seed = 42,
      verbose = FALSE
    ),
    "int variable contained non-int values"
  )
})

test_that("NA integer data is rejected before Stan services run", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)

  expect_error(
    sampling(
      mod,
      data = list(N = 4L, y = c(1L, NA_integer_, 1L, 0L)),
      num_warmup = 5,
      num_samples = 5,
      seed = 42,
      verbose = FALSE
    ),
    "Integer variable 'y' contains NA"
  )
})

test_that("invalid sampling counts return a configuration failure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 4L, y = c(1L, 0L, 1L, 0L))

  zero_chains <- sampling(
    mod, data, num_chains = 0, num_warmup = 5, num_samples = 5,
    seed = 42, verbose = FALSE
  )
  zero_thin <- sampling(
    mod, data, thin = 0, num_warmup = 5, num_samples = 5,
    seed = 42, verbose = FALSE
  )

  expect_false(zero_chains$return_code == 0L)
  expect_false(zero_thin$return_code == 0L)
})

test_that("numeric initialization radius is accepted", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  expect_silent(
    sampling(mod, list(N = 4L, y = c(1L, 0L, 1L, 0L)), init = c(0.1),
               num_warmup = 2, num_samples = 2, seed = 42,
               verbose = FALSE)
  )
})
