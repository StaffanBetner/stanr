local_test_context()

init_test_cache("walnuts")

test_that("sampling with walnuts engine works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 100,
    iter_sampling = 100,
    chains = 2,
    engine = "walnuts",
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_s3_class(result, "StanMCMC")
  expect_equal(result$return_codes(), c(0L, 0L))
  expect_equal(posterior::niterations(result$draws()), 100L)
  expect_equal(posterior::nchains(result$draws()), 2L)
  expect_true("lp__" %in% posterior::variables(result$draws()))
  expect_equal(posterior::nvariables(result$sampler_diagnostics()), 0L)
  mean_theta <- mean(posterior::extract_variable(result$draws(), "theta"))
  expect_equal(mean_theta, 0.5, tolerance = 0.15)
})

test_that("walnuts save_warmup and thin produce the expected draw counts", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_no_warning(
    result <- mod$sample(
      data = data,
      iter_warmup = 50,
      iter_sampling = 60,
      thin = 3,
      save_warmup = TRUE,
      chains = 1,
      engine = "walnuts",
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    )
  )

  expect_equal(posterior::niterations(result$draws()), 20L)
  expect_equal(
    posterior::niterations(result$draws(inc_warmup = TRUE)),
    37L
  )
})

test_that("walnuts honors an explicit inv_metric", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "walnuts",
    inv_metric = c(0.5),
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_length(result$inv_metric(), 1L)
})

test_that("walnuts rejects fixed_param, adapt_engaged = FALSE, and non-diag_e metrics", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$sample(data = data, engine = "walnuts", fixed_param = TRUE),
    "`fixed_param` is not supported"
  )
  expect_error(
    mod$sample(data = data, engine = "walnuts", adapt_engaged = FALSE),
    "`adapt_engaged = FALSE` is not supported"
  )
  expect_error(
    mod$sample(data = data, engine = "walnuts", metric = "dense_e"),
    "only supports `metric = \"diag_e\"`"
  )
})
