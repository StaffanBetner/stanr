# Tests for StanFit's delegation of expose_stan_functions()/expose_functions()
# and the `$functions` active binding to the bound StanModel. Model-side
# behavior is covered by test-expose-functions.R; these tests only check that
# the fit forwards correctly.
#
# The same tiny Stan program is reused across `make_fit()` calls so repeated
# `stan_model()` compiles in this file hit the in-session cache after the
# first one (see test-stan_model.R for the same pattern).
fit_expose_code <- "
functions {
  real my_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"

make_fit <- function() {
  mod <- stan_model(code = fit_expose_code)
  fit <- mod$sample(
    data = list(),
    chains = 1,
    iter_warmup = 10,
    iter_sampling = 10,
    refresh = 0,
    show_messages = FALSE
  )
  list(mod = mod, fit = fit)
}

test_that("fit$expose_stan_functions() populates fit$functions, shared with the model", {
  fm <- make_fit()

  fm$fit$expose_stan_functions()

  expect_identical(fm$fit$functions, fm$mod$functions)
  expect_true(is.function(fm$fit$functions$my_add))
  expect_equal(fm$fit$functions$my_add(2, 3), 5)
})

test_that("fit$expose_functions() alias works", {
  fm <- make_fit()

  fm$fit$expose_functions()

  expect_true(is.function(fm$fit$functions$my_add))
  expect_equal(fm$fit$functions$my_add(2, 3), 5)
})

test_that("fit$functions <- ... errors (read-only active binding)", {
  fm <- make_fit()

  expect_error(
    fm$fit$functions <- new.env(),
    "\\$functions.*read-only"
  )
})

test_that("reading fit$functions before any expose call does not error", {
  fm <- make_fit()

  expect_no_error(fm$fit$functions)
  expect_true(is.environment(fm$fit$functions))
})

test_that("a fit without a model binding errors with the exact 'no model binding' message", {
  fit <- newstan:::StanFit$new(
    payload = list(),
    model = NULL,
    data = list(),
    seed = 1L,
    elapsed = 0,
    metadata = list()
  )

  expect_error(
    fit$expose_stan_functions(),
    "^This fit does not retain a model binding\\.$"
  )
  expect_error(
    fit$functions,
    "^This fit does not retain a model binding\\.$"
  )
  expect_error(
    fit$unconstrain_variables(list(t = list(1))),
    "does not retain a model binding"
  )
})

test_that("delegation reaches the model: a second model-level expose is a cheap no-op", {
  fm <- make_fit()

  fm$fit$expose_stan_functions()
  first_env <- fm$fit$functions

  expect_no_error(fm$mod$expose_stan_functions())
  expect_identical(fm$fit$functions, first_env)
  expect_true(is.function(fm$fit$functions$my_add))
  expect_equal(fm$fit$functions$my_add(4, 5), 9)
})
