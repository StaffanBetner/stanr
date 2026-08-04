# Setup ------------------------------------------------------------------

skip_if_not_installed("loo")

bernoulli_log_lik_file <- testthat::test_path(
  "test-models",
  "bernoulli_log_lik.stan"
)
bernoulli_log_lik_code <- paste(
  readLines(bernoulli_log_lik_file),
  collapse = "\n"
)
bernoulli_log_lik_mod <- newstan::stan_model(
  code = bernoulli_log_lik_code,
  compile = TRUE
)

loo_bernoulli_data <- list(N = 10, y = c(0, 1, 1, 0, 1, 0, 1, 1, 1, 0))

bernoulli_fit <- bernoulli_log_lik_mod$sample(
  data = loo_bernoulli_data,
  chains = 1,
  num_threads = 1,
  iter_sampling = 100,
  iter_warmup = 50,
  seed = 1234
)

# Basic LOO tests --------------------------------------------------------

test_that("$loo() returns a loo object", {
  loo_result <- suppressWarnings(bernoulli_fit$loo())
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() accepts r_eff = FALSE (default)", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(r_eff = FALSE))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() accepts r_eff = TRUE", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(r_eff = TRUE))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() accepts moment_match = TRUE", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(moment_match = TRUE))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() passes extra arguments to loo.array()", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(cores = 1, save_psis = TRUE))
  expect_s3_class(loo_result, "loo")
  expect_true("psis_object" %in% names(loo_result))
})

# Variable name tests ----------------------------------------------------

test_that("$loo() uses custom variable name via 'variables'", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(variables = "log_lik"))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() errors on multiple variable names", {
  expect_error(
    bernoulli_fit$loo(variables = c("log_lik", "theta")),
    "Only a single variable name is allowed"
  )
})

# Missing log_lik tests --------------------------------------------------

test_that("$loo() errors when log_lik is not in draws", {
  bernoulli_no_ll_mod <- newstan::stan_model(
    code = paste(
      readLines(testthat::test_path("test-models", "bernoulli.stan")),
      collapse = "\n"
    ),
    compile = TRUE
  )
  bernoulli_no_ll_fit <- bernoulli_no_ll_mod$sample(
    data = loo_bernoulli_data,
    chains = 1,
    num_threads = 1,
    iter_sampling = 50,
    iter_warmup = 25,
    seed = 1234
  )
  expect_error(bernoulli_no_ll_fit$loo(), "log_lik")
})
