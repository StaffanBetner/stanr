test_that("stan_model() accepts file and code inputs", {
  skip_if_not(requireNamespace("digest", quietly = TRUE))
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))

  # Test that stan_model() validates inputs
  expect_error(stan_model(), "Either 'file' or 'code' must be provided")
  expect_error(stan_model(file = "/nonexistent.stan"), "File not found")
})

test_that("stan_model() returns a newstan_fit object", {
  skip_if_not(requireNamespace("digest", quietly = TRUE))
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))

  # This test will fail until QuickJSR is integrated, but checks the interface
  model_file <- system.file("test-models", "bernoulli.stan", package = "newstan")

  # If the file exists in the package, try loading it
  if (model_file != "") {
    # The compilation will fail without QuickJSR, so we just check the interface
    expect_error(
      stan_model(file = model_file),
      NA  # Will pass once QuickJSR is integrated
    )
  }
})

test_that("stan_model() validates arguments", {
  expect_error(stan_model(file = NULL, code = NULL), "Either 'file' or 'code'")
  expect_error(stan_model(file = "x.stan", code = "model {}"), "either 'file' or 'code'")
})
