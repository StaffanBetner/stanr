test_that("pathfinder returns expected structure", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

  result <- pathfinder(
    mod,
    data,
    iter = 100,
    num_draws = 50,
    seed = 42,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("return_code", "args"))
  expect_equal(result$return_code, 0L)
})
