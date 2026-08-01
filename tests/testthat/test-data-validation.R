test_that("numeric values are accepted for Stan integer data only when integral", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)

  result <- mod$sample(
    data = list(N = 4, y = c(1, 0, 1, 0)),
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)

  expect_error(
    mod$sample(
      data = list(N = 4, y = c(1, 0.5, 1, 0)),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE
    ),
    "int variable contained non-int values"
  )
})

test_that("NA integer data is rejected before Stan services run", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)

  expect_error(
    mod$sample(
      data = list(N = 4L, y = c(1L, NA_integer_, 1L, 0L)),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE
    ),
    "Integer variable 'y' contains NA"
  )
})

test_that("invalid sampling counts throw a configuration error", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 4L, y = c(1L, 0L, 1L, 0L))

  expect_error(
    mod$sample(
      data = data,
      chains = 0,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 42,
      show_messages = FALSE
    ),
    "`chains` must be a positive integer"
  )
  expect_error(
    mod$sample(
      data = data,
      thin = 0,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 42,
      show_messages = FALSE
    ),
    "thin must be at least 1"
  )
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = .Machine$integer.max,
      iter_sampling = .Machine$integer.max,
      save_warmup = TRUE,
      chains = 1,
      seed = 42,
      show_messages = FALSE
    ),
    "Requested number of saved draws is too large"
  )
})

test_that("numeric initialization radius is accepted", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  expect_silent(
    mod$sample(
      data = list(N = 4L, y = c(1L, 0L, 1L, 0L)),
      init = c(0.1),
      iter_warmup = 2,
      iter_sampling = 2,
      chains = 1,
      seed = 42,
      show_messages = FALSE
    )
  )
})
