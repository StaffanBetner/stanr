test_that("sampling() validates its arguments", {
  skip_if_not(requireNamespace("digest", quietly = TRUE))
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))

  # Create a minimal fit object for testing
  fit <- structure(
    list(
      model_name = "test",
      cache_path = "/tmp",
      dll = NULL,
      num_params = 1,
      data = list(N = 10L, y = c(0L, 1L, 0L, 1L))
    ),
    class = "newstan_fit"
  )

  expect_error(sampling(fit, chains = 0), "chains must be >= 1")
  expect_error(sampling(fit, iter_warmup = -1), "iter_warmup must be >= 0")
  expect_error(sampling(fit, iter_sampling = -1), "iter_sampling must be >= 0")
})

test_that("sampling() passes correct args to C++ runner", {
  # Integration test — requires a compiled model
  skip("Requires QuickJSR integration and compiled model")
})
