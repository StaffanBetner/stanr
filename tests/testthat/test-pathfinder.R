test_that("pathfinder single path returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- pathfinder(
    mod,
    data,
    max_lbfgs_iters = 100,
    num_draws = 1000,
    num_paths = 4,
    seed = 42
  )

  expect_type(result, "list")
  expect_s3_class(result, "StanPathfinder")
  expect_named(result, c("draws", "diagnostics", "return_code", "args"))
  expect_s3_class(summary(result), "draws_summary")
  expect_equal(result$return_code, 0L)
})

test_that("pathfinder single path returns draws_df", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- pathfinder(
    mod,
    data,
    max_lbfgs_iters = 100,
    num_draws = 50,
    num_paths = 1,
    seed = 42,
    verbose = FALSE
  )

  expect_s3_class(result$draws, "draws_df")
  expect_equal(posterior::ndraws(result$draws), 50L)
  expect_s3_class(result$diagnostics, "draws_df")
  expect_true("lp_approx__" %in% posterior::variables(result$diagnostics))
})

test_that("pathfinder multi-path returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- pathfinder(
    mod,
    data,
    max_lbfgs_iters = 100,
    num_draws = 50,
    num_paths = 4,
    num_psis_draws = 100,
    seed = 42,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_equal(result$return_code, 0L)
  expect_s3_class(result$draws, "draws_df")
  expect_equal(posterior::ndraws(result$draws), 100L)
  expect_s3_class(result$diagnostics, "draws_df")
  expect_true("lp_approx__" %in% posterior::variables(result$diagnostics))
  expect_true("lp__" %in% posterior::variables(result$diagnostics))
  expect_true("path__" %in% posterior::variables(result$diagnostics))
})

test_that("pathfinder multi-path with num_paths = 1 uses single pathfinder", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- pathfinder(
    mod,
    data,
    max_lbfgs_iters = 100,
    num_draws = 50,
    num_paths = 1,
    seed = 42,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_s3_class(result$draws, "draws_df")
  expect_equal(posterior::ndraws(result$draws), 50L)
  expect_s3_class(result$diagnostics, "draws_df")
})
