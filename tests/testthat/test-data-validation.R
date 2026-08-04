local_test_context()

init_test_cache("data-validation")

test_that("numeric values are accepted for Stan integer data only when integral", {
  mod <- test_model("bernoulli")

  result <- mod$sample(
    data = list(N = 4, y = c(1, 0, 1, 0)),
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)

  expect_error(
    mod$sample(
      data = list(N = 4, y = c(1, 0.5, 1, 0)),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "int variable contained non-int values"
  )
})

test_that("NA integer data is rejected before Stan services run", {
  mod <- test_model("bernoulli")

  expect_error(
    mod$sample(
      data = list(N = 4L, y = c(1L, NA_integer_, 1L, 0L)),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Integer variable 'y' contains NA"
  )
})

test_that("NA/NaN real data is rejected before Stan services run", {
  mod <- test_model("model_methods")
  data <- list(N = 4L, y = c(1L, 0L, 1L, 0L), mu = NA_real_)

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Real variable 'mu' contains NA or NaN"
  )

  data$mu <- NaN
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Real variable 'mu' contains NA or NaN"
  )
})

test_that("invalid sampling counts throw a configuration error", {
  mod <- test_model("bernoulli")
  data <- list(N = 4L, y = c(1L, 0L, 1L, 0L))

  expect_error(
    mod$sample(
      data = data,
      chains = 0,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`chains` must be a single integer"
  )
  expect_error(
    mod$sample(
      data = data,
      thin = 0,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`thin` must be a single integer >= 1"
  )
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = .Machine$integer.max,
      iter_sampling = .Machine$integer.max,
      save_warmup = TRUE,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Requested number of saved draws is too large"
  )
})

test_that("numeric initialization radius is accepted", {
  mod <- test_model("bernoulli")
  expect_silent(
    mod$sample(
      data = list(N = 4L, y = c(1L, 0L, 1L, 0L)),
      init = c(0.1),
      iter_warmup = 2,
      iter_sampling = 2,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    )
  )
})

withr::deferred_run()
