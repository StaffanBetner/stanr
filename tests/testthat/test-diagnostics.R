# Helper: cached model instances to avoid recompilation
.newstan_diagnose_cache <- new.env(parent = emptyenv())

get_bernoulli_model <- function() {
  if (!exists("bernoulli", envir = .newstan_diagnose_cache)) {
    path <- test_path("test-models/bernoulli.stan")
    .newstan_diagnose_cache$bernoulli <- stan_model(stan_file = path)
  }
  .newstan_diagnose_cache$bernoulli
}

get_model_methods_model <- function() {
  if (!exists("model_methods", envir = .newstan_diagnose_cache)) {
    path <- test_path("test-models/model_methods.stan")
    .newstan_diagnose_cache$model_methods <- stan_model(stan_file = path)
  }
  .newstan_diagnose_cache$model_methods
}

bernoulli_data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

# ---------------------------------------------------------------------------
# Basic functionality
# ---------------------------------------------------------------------------

test_that("diagnose() returns expected class and structure", {
  mod <- get_bernoulli_model()
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  expect_s3_class(result, "StanDiagnose")
  expect_s3_class(result, "StanFit")
  expect_type(result$num_failed(), "integer")
  expect_equal(result$return_codes(), 0L)
})

test_that("diagnose() passes gradient check for bernoulli model", {
  mod <- get_bernoulli_model()
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  expect_equal(result$num_failed(), 0L)
})

# ---------------------------------------------------------------------------
# $gradients() method
# ---------------------------------------------------------------------------

test_that("$gradients() returns a data frame with correct columns", {
  mod <- get_bernoulli_model()
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  grads <- result$gradients()
  expect_true(is.data.frame(grads))
  expect_equal(
    colnames(grads),
    c("param_idx", "value", "model", "finite_diff", "error")
  )
})

test_that("$gradients() has one row per unconstrained parameter", {
  # bernoulli has one unconstrained parameter (theta, constrained to [0,1])
  mod <- get_bernoulli_model()
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  grads <- result$gradients()
  expect_equal(nrow(grads), 1L)
})

test_that("$gradients() has correct dimensions for multi-parameter model", {
  # model_methods has 3 unconstrained parameters: theta, beta[1], beta[2]
  mod <- get_model_methods_model()
  data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0), mu = 0)
  result <- suppressMessages(mod$diagnose(data = data, seed = 42))

  grads <- result$gradients()
  expect_equal(nrow(grads), 3L)
  expect_equal(ncol(grads), 5L)
})

# ---------------------------------------------------------------------------
# $lp() method
# ---------------------------------------------------------------------------

test_that("$lp() returns a numeric value", {
  mod <- get_bernoulli_model()
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  lp_val <- result$lp()
  expect_true(is.numeric(lp_val))
  expect_length(lp_val, 1L)
  expect_false(is.na(lp_val))
})

test_that("$lp() returns different values for different seeds", {
  mod <- get_bernoulli_model()
  result1 <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))
  result2 <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 123))

  # Different seeds lead to different init values, so lp should differ
  expect_false(identical(result1$lp(), result2$lp()))
})

# ---------------------------------------------------------------------------
# Argument handling
# ---------------------------------------------------------------------------

test_that("diagnose() runs with scientific notation arguments", {
  mod <- get_bernoulli_model()
  result <- suppressMessages(
    mod$diagnose(
      data = bernoulli_data,
      seed = 42,
      epsilon = 1e-6,
      error = 1e-6
    )
  )
  expect_s3_class(result, "StanDiagnose")
})

test_that("diagnose() runs with default arguments", {
  mod <- get_bernoulli_model()
  result <- suppressMessages(
    mod$diagnose(data = bernoulli_data, seed = 42)
  )
  expect_s3_class(result, "StanDiagnose")
})

test_that("diagnose() errors for invalid epsilon", {
  mod <- get_bernoulli_model()
  expect_error(
    mod$diagnose(data = bernoulli_data, seed = 42, epsilon = -1),
    regexp = "epsilon"
  )
})

test_that("diagnose() errors for invalid error threshold", {
  mod <- get_bernoulli_model()
  expect_error(
    mod$diagnose(data = bernoulli_data, seed = 42, error = -1),
    regexp = "error"
  )
})

# ---------------------------------------------------------------------------
# Metadata
# ---------------------------------------------------------------------------

test_that("metadata includes num_failed", {
  mod <- get_bernoulli_model()
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  meta <- result$metadata()
  expect_true("num_failed" %in% names(meta))
  expect_equal(meta$num_failed, result$num_failed())
})
