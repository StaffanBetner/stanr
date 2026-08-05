local_test_context()

init_test_cache("classes-fit")

# Tests for internal helpers in R/classes-fit.R

test_that(".stanr_bracket_names converts dotted Stan names to bracket form", {
  expect_equal(stanr:::.stanr_bracket_names("alpha"), "alpha")
  expect_equal(stanr:::.stanr_bracket_names("beta.1"), "beta[1]")
  expect_equal(stanr:::.stanr_bracket_names("theta.2.3"), "theta[2,3]")
  expect_equal(stanr:::.stanr_bracket_names("x[1]"), "x[1]")
})


test_that(".stanr_bracket_names handles a vector mixing dotted and plain names", {
  input <- c("alpha", "beta.1", "theta.2.3", "x[1]", "par.12.3", "sigma123")
  expected <- c(
    "alpha",
    "beta[1]",
    "theta[2,3]",
    "x[1]",
    "par[12,3]",
    "sigma123"
  )
  expect_equal(stanr:::.stanr_bracket_names(input), expected)
})


test_that("optimize() fit errors on draws(inc_warmup = TRUE)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_error(
    result$draws(inc_warmup = TRUE),
    "warmup draws were not saved"
  )
})


test_that("direct fit method coverage: time, init, code, print, lp, num_chains", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  fit <- mod$sample(
    data = data,
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 2,
    seed = 42,
    init = list(theta = 0.5),
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(is.numeric(fit$time()$total))
  expect_gte(fit$time()$total, 0)

  expect_identical(fit$init(), list(theta = 0.5))

  no_init_fit <- stanr:::StanFit$new(
    payload = list(),
    model = NULL,
    data = list(),
    seed = 1L,
    elapsed = 0,
    metadata = list()
  )
  expect_null(no_init_fit$init())

  expect_identical(fit$code(), mod$code())

  expect_output(fit$print())
  expect_identical(fit$print(), fit)

  expect_true(is.numeric(fit$lp()))
  expect_length(fit$lp(), 10L)

  expect_equal(fit$num_chains(), 2L)
})

test_that("pathfinder fit$lp_approx() returns a numeric vector", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  fit <- mod$pathfinder(
    data = data,
    max_lbfgs_iters = 20,
    single_path_draws = 10,
    draws = 10,
    num_paths = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(is.numeric(fit$lp_approx()))
  expect_length(fit$lp_approx(), 10L)
})

test_that("$lp_approx() is only available on StanLaplace/StanVB/StanPathfinder", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  mcmc <- mod$sample(
    data = data,
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  mle <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_false("lp_approx" %in% names(mcmc))
  expect_false("lp_approx" %in% names(mle))
})

test_that("$lp() returns a numeric vector for optimize()", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  fit <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(is.numeric(fit$lp()))
  expect_length(fit$lp(), 1L)
})

test_that("StanGQ$num_chains() reports the draws' own chain count, 0L with no draws", {
  mod <- test_model("bernoulli_gqs")
  data <- bernoulli_data

  samp <- mod$sample(
    data = data,
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 2,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  gq <- mod$generate_quantities(
    fitted_params = samp$draws(),
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  expect_equal(gq$num_chains(), samp$num_chains())
  expect_equal(gq$num_chains(), posterior::nchains(gq$draws()))

  empty_gq <- stanr:::StanGQ$new(
    payload = list(),
    model = NULL,
    data = list(),
    seed = 1L,
    elapsed = 0,
    metadata = list()
  )
  expect_equal(empty_gq$num_chains(), 0L)
})

test_that("mod$print() prints the Stan code and returns the model invisibly", {
  mod <- test_model("bernoulli")

  expect_output(mod$print(), "parameters")
  expect_false(withVisible(mod$print())$visible)
  expect_identical(mod$print(), mod)
})

test_that("mod$stan_version() matches the expected version format", {
  mod <- test_model("bernoulli")
  expect_match(mod$stan_version(), "^\\d+\\.\\d+")
})


test_that("save_object() does not serialize the fit data twice", {
  # Regression test: `private$data_` and `private$metadata_$data` used to
  # both hold a reference to the same data list, and R serialization does
  # not deduplicate identical objects reachable via two different fields,
  # so `saveRDS()` wrote the data twice.
  set.seed(1)
  data <- list(x = stats::rnorm(1e6)) # ~8MB as doubles

  fit <- stanr:::StanFit$new(
    payload = list(),
    model = NULL,
    data = data,
    seed = 1L,
    elapsed = 0,
    metadata = list()
  )

  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  fit$save_object(file)

  fit_size <- file.info(file)$size
  one_copy_size <- length(serialize(data, connection = NULL))

  # If `data` were still stored twice, the saved file would be at least
  # ~2x one copy of `data`. With the fix it should be close to one copy
  # plus a small, fixed overhead for the rest of the (mostly empty) fit.
  expect_lt(fit_size, 1.5 * one_copy_size)

  # The public `$metadata()` contract is unchanged: `data` still round-trips
  # and the full field set is the same as before the fix.
  meta <- fit$metadata()
  expect_identical(meta$data, data)
  expect_identical(
    names(meta),
    c("seed", "data", "arguments", "model_name")
  )
})

withr::deferred_run()
