test_that("use_opencl = TRUE stores and reports the flag without compiling", {
  mod <- stan_model(
    code = "
      data { int<lower=0> N; }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ",
    use_opencl = TRUE,
    compile = FALSE
  )

  expect_true(mod$use_opencl())
  expect_false(mod$is_compiled())
})

skip_if(!nzchar(Sys.getenv("NEWSTAN_OPENCL_LIBS")), "NEWSTAN_OPENCL_LIBS not set")
withr::local_options(newstan_opencl_libs = Sys.getenv("NEWSTAN_OPENCL_LIBS"))

.opencl_test_data <- function() {
  set.seed(123)
  n <- 200L
  k <- 2L
  x <- matrix(stats::rnorm(n * k), nrow = n, ncol = k)
  eta <- 0.5 + x %*% c(1, -0.5)
  y <- stats::rbinom(n, 1, 1 / (1 + exp(-eta)))
  list(N = n, K = k, X = x, y = as.integer(y))
}

test_that("an OpenCL-compiled model samples successfully on POCL", {
  path <- test_path("test-models/bernoulli_logit_glm.stan")
  mod <- stan_model(stan_file = path, use_opencl = TRUE)
  data <- .opencl_test_data()

  result <- mod$sample(
    data = data,
    opencl_ids = c(0, 0),
    chains = 2,
    iter_warmup = 200,
    iter_sampling = 200,
    seed = 42,
    show_messages = FALSE
  )

  expect_s3_class(result, "StanMCMC")
  expect_true(all(result$return_codes() == 0L))
})

test_that("OpenCL and CPU fits of the same model agree on posterior means", {
  path <- test_path("test-models/bernoulli_logit_glm.stan")
  data <- .opencl_test_data()

  mod_cl <- stan_model(stan_file = path, use_opencl = TRUE)
  fit_cl <- mod_cl$sample(
    data = data,
    opencl_ids = c(0, 0),
    chains = 2,
    iter_warmup = 200,
    iter_sampling = 200,
    seed = 42,
    show_messages = FALSE
  )

  mod_cpu <- stan_model(stan_file = path, use_opencl = FALSE)
  fit_cpu <- mod_cpu$sample(
    data = data,
    chains = 2,
    iter_warmup = 200,
    iter_sampling = 200,
    seed = 42,
    show_messages = FALSE
  )

  # OpenCL draws are NOT bitwise-reproducible against CPU draws even with the
  # same seed -- the RNG stream interacts differently with OpenCL-batched
  # computations -- so only posterior mean agreement is checked, not equality
  # of raw draws.
  vars <- c("alpha", "beta[1]", "beta[2]")
  means_cl <- suppressWarnings(fit_cl$summary(variables = vars))$mean
  means_cpu <- suppressWarnings(fit_cpu$summary(variables = vars))$mean

  expect_equal(means_cl, means_cpu, tolerance = 0.3)
})

test_that("an invalid OpenCL device index errors", {
  path <- test_path("test-models/bernoulli_logit_glm.stan")
  mod <- stan_model(stan_file = path, use_opencl = TRUE)
  data <- .opencl_test_data()

  expect_error(
    mod$sample(
      data = data,
      opencl_ids = c(99, 99),
      chains = 1,
      iter_warmup = 50,
      iter_sampling = 50,
      seed = 42,
      show_messages = FALSE
    )
  )
})

test_that("a non-OpenCL model rejects opencl_ids at fit time", {
  path <- test_path("test-models/bernoulli_logit_glm.stan")
  mod <- stan_model(stan_file = path, use_opencl = FALSE)
  data <- .opencl_test_data()

  expect_error(
    mod$sample(data = data, opencl_ids = c(0, 0), chains = 1, seed = 42),
    "not compiled with OpenCL support"
  )
})

test_that("optimize() runs successfully on an OpenCL-compiled model", {
  path <- test_path("test-models/bernoulli_logit_glm.stan")
  mod <- stan_model(stan_file = path, use_opencl = TRUE)
  data <- .opencl_test_data()

  result <- mod$optimize(
    data = data,
    opencl_ids = c(0, 0),
    seed = 42,
    show_messages = FALSE
  )

  expect_s3_class(result, "StanMLE")
  expect_equal(result$return_codes(), 0L)
})
