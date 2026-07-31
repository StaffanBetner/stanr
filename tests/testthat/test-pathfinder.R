test_that("pathfinder single path returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$pathfinder(
    data = data,
    max_lbfgs_iters = 100,
    single_path_draws = 100,
    draws = 100,
    num_paths = 4,
    seed = 42,
    show_messages = FALSE
  )

  expect_s3_class(result, "StanPathfinder")
  expect_s3_class(result, "StanFit")
  expect_s3_class(result$summary(), "draws_summary")
  expect_equal(result$return_codes(), 0L)
})

test_that("pathfinder single path returns draws_df", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$pathfinder(
    data = data,
    max_lbfgs_iters = 100,
    single_path_draws = 50,
    num_paths = 1,
    seed = 42,
    show_messages = FALSE
  )

  draws <- result$draws(format = "draws_df")
  expect_s3_class(draws, "draws_df")
  expect_equal(posterior::ndraws(draws), 50L)
  expect_true("lp_approx__" %in% posterior::variables(draws))
})

test_that("pathfinder multi-path returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$pathfinder(
    data = data,
    max_lbfgs_iters = 100,
    single_path_draws = 50,
    num_paths = 4,
    draws = 100,
    seed = 42,
    show_messages = FALSE
  )

  draws <- result$draws(format = "draws_df")
  expect_equal(result$return_codes(), 0L)
  expect_s3_class(draws, "draws_df")
  expect_equal(posterior::ndraws(draws), 100L)
  expect_true(all(c("lp_approx__", "lp__", "path__") %in%
    posterior::variables(draws)))
})

test_that("pathfinder multi-path with num_paths = 1 uses single pathfinder", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- mod$pathfinder(
    data = data,
    max_lbfgs_iters = 100,
    single_path_draws = 50,
    num_paths = 1,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result$draws(format = "draws_df"), "draws_df")
  expect_equal(posterior::ndraws(result$draws()), 50L)
})
