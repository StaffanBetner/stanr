test_that("stan_model compiles from file", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  expect_s3_class(mod, "StanModel")
  expect_s3_class(mod, "R6")
  expect_true(mod$is_compiled())
})

test_that("stan_model compiles from code", {
  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code)
  expect_s3_class(mod, "StanModel")
  expect_true(mod$is_compiled())
})

test_that("stan_model errors when neither file nor code given", {
  expect_snapshot(stan_model(), error = TRUE)
})

test_that("stan_model errors when both file and code given", {
  path <- test_path("test-models/bernoulli.stan")
  expect_snapshot(
    stan_model(stan_file = path, code = "parameters { real x; }"),
    error = TRUE
  )
})

test_that("stan_model validates the precompiled_headers argument", {
  expect_error(
    stan_model(code = "parameters { real x; }", precompiled_headers = NA),
    "`precompiled_headers` must be TRUE or FALSE"
  )
})

test_that("stan_model errors on missing file", {
  expect_snapshot(stan_model(stan_file = "nonexistent.stan"), error = TRUE)
})
