local_test_context()

init_test_cache("diagnostics")

# Direct unit coverage for .newstan_parse_diagnose_output() (Task 3.3): the
# black-box mod$diagnose() tests below exercise it only with real Stan
# gradient-check output, which never contains more than one "Log
# probability=" line. The rewrite from per-row rbind() to vector
# accumulation must still preserve the exact former for-loop semantics --
# in particular "last valid Log probability= line wins" -- so test that
# directly here.
test_that(".newstan_parse_diagnose_output() keeps the last valid Log probability= line and all gradient rows", {
  lines <- c(
    "",
    "Log probability=-1.5",
    "   param idx           value           model     finite diff           error ",
    "             0             0.2             1.1             1.2           0.05",
    "             1            -0.3            -2.2            -2.1            0.1",
    "Log probability=-2.75",
    "not a data row at all"
  )

  parsed <- newstan:::.newstan_parse_diagnose_output(lines)

  expect_equal(parsed$lp, -2.75)
  expect_true(is.data.frame(parsed$gradients))
  expect_equal(nrow(parsed$gradients), 2L)
  expect_equal(
    colnames(parsed$gradients),
    c("param_idx", "value", "model", "finite_diff", "error")
  )
  expect_equal(parsed$gradients$param_idx, c(0L, 1L))
  expect_type(parsed$gradients$param_idx, "integer")
  expect_equal(parsed$gradients$value, c(0.2, -0.3))
  expect_equal(parsed$gradients$error, c(0.05, 0.1))
})

test_that(".newstan_parse_diagnose_output() returns the empty-typed skeleton for non-character/empty input", {
  expected_cols <- c("param_idx", "value", "model", "finite_diff", "error")

  for (bad_input in list(NULL, character(0), 42)) {
    parsed <- newstan:::.newstan_parse_diagnose_output(bad_input)
    expect_true(is.na(parsed$lp))
    expect_equal(nrow(parsed$gradients), 0L)
    expect_equal(colnames(parsed$gradients), expected_cols)
  }
})

# ---------------------------------------------------------------------------
# Basic functionality
# ---------------------------------------------------------------------------

test_that("diagnose() returns expected class and structure", {
  mod <- test_model("bernoulli")
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  expect_s3_class(result, "StanDiagnose")
  expect_s3_class(result, "StanFit")
  expect_type(result$num_failed(), "integer")
  expect_equal(result$return_codes(), 0L)
})

test_that("diagnose() passes gradient check for bernoulli model", {
  mod <- test_model("bernoulli")
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  expect_equal(result$num_failed(), 0L)
})

test_that("diagnose() fails the gradient check for a model with cubic log density", {
  # The central finite difference deviates from autodiff by ~epsilon^2
  # independent of the evaluation point for a cubic target, so a large
  # epsilon and small error tolerance make the check fail deterministically.
  mod <- stan_model(code = "parameters { real x; } model { target += x^3; }")
  expect_message(
    result <- mod$diagnose(
      data = list(),
      seed = 42,
      epsilon = 0.1,
      error = 1e-8
    ),
    "1 parameter\\(s\\) failed"
  )
  expect_equal(result$num_failed(), 1L)
  expect_equal(result$return_codes(), 1L)
  expect_equal(nrow(result$gradients()), 1L)
})

# ---------------------------------------------------------------------------
# $gradients() method
# ---------------------------------------------------------------------------

test_that("$gradients() returns a data frame with correct columns", {
  mod <- test_model("bernoulli")
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
  mod <- test_model("bernoulli")
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  grads <- result$gradients()
  expect_equal(nrow(grads), 1L)
})

test_that("$gradients() has correct dimensions for multi-parameter model", {
  # model_methods has 3 unconstrained parameters: theta, beta[1], beta[2]
  mod <- test_model("model_methods")
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
  mod <- test_model("bernoulli")
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  lp_val <- result$lp()
  expect_true(is.numeric(lp_val))
  expect_length(lp_val, 1L)
  expect_false(is.na(lp_val))
})

test_that("$lp() returns different values for different seeds", {
  mod <- test_model("bernoulli")
  result1 <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))
  result2 <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 123))

  # Different seeds lead to different init values, so lp should differ
  expect_false(identical(result1$lp(), result2$lp()))
})

# ---------------------------------------------------------------------------
# $output() method
# ---------------------------------------------------------------------------

test_that("$output() returns the gradient-table lines", {
  mod <- test_model("bernoulli")
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  expect_true(is.character(result$output()))
  expect_gt(length(result$output()), 0)
})

# ---------------------------------------------------------------------------
# Argument handling
# ---------------------------------------------------------------------------

test_that("diagnose() runs with scientific notation arguments", {
  mod <- test_model("bernoulli")
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
  mod <- test_model("bernoulli")
  result <- suppressMessages(
    mod$diagnose(data = bernoulli_data, seed = 42)
  )
  expect_s3_class(result, "StanDiagnose")
})

test_that("diagnose() errors for invalid epsilon", {
  mod <- test_model("bernoulli")
  expect_error(
    mod$diagnose(data = bernoulli_data, seed = 42, epsilon = -1),
    regexp = "epsilon"
  )
})

test_that("diagnose() errors for invalid error threshold", {
  mod <- test_model("bernoulli")
  expect_error(
    mod$diagnose(data = bernoulli_data, seed = 42, error = -1),
    regexp = "error"
  )
})

# ---------------------------------------------------------------------------
# Metadata
# ---------------------------------------------------------------------------

test_that("metadata includes num_failed", {
  mod <- test_model("bernoulli")
  result <- suppressMessages(mod$diagnose(data = bernoulli_data, seed = 42))

  meta <- result$metadata()
  expect_true("num_failed" %in% names(meta))
  expect_equal(meta$num_failed, result$num_failed())
})

withr::deferred_run()
