local_test_context()

init_test_cache("advi")

test_that("advi returns expected structure", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$variational(
    data = data,
    iter = 1000,
    draws = 100,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_s3_class(result, "StanVB")
  expect_s3_class(result, "StanFit")
  expect_s3_class(result$draws(), "draws_matrix")
  expect_s3_class(result$summary(), "draws_summary")
  expect_equal(result$return_codes(), 0L)
})

test_that("advi with meanfield algorithm works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$variational(
    data = data,
    algorithm = "meanfield",
    iter = 1000,
    draws = 100,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("advi requested draw count is respected and lp__ is not included", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$variational(
    data = data,
    iter = 1000,
    draws = 100,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(posterior::ndraws(result$draws()), 100L)
  expect_false("lp__" %in% posterior::variables(result$draws()))
})

test_that("advi with an invalid algorithm errors before reaching C++", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$variational(
      data = data,
      algorithm = "typo",
      iter = 1000,
      draws = 100,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`algorithm` must be one of"
  )
})

withr::deferred_run()
