test_that("stan_model compiles from file", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(file = path)
  expect_true(is.environment(mod))
  expect_true(exists("new_model", envir = mod))
})

test_that("stan_model compiles from code", {
  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code)
  expect_true(is.environment(mod))
  expect_true(exists("new_model", envir = mod))
})

test_that("stan_model errors when neither file nor code given", {
  expect_snapshot(stan_model(), error = TRUE)
})

test_that("stan_model errors when both file and code given", {
  path <- test_path("test-models/bernoulli.stan")
  expect_snapshot(
    stan_model(file = path, code = "parameters { real x; }"),
    error = TRUE
  )
})

test_that("stan_model validates the precompile argument", {
  expect_error(
    stan_model(code = "parameters { real x; }", precompile = NA),
    "`precompile` must be TRUE or FALSE"
  )
})

test_that("stan_model errors on missing file", {
  expect_snapshot(stan_model(file = "nonexistent.stan"), error = TRUE)
})
