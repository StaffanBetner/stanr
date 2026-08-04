local_test_context()

init_test_cache("sampling")

test_that("sampling returns expected structure", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_s3_class(result, "StanMCMC")
  expect_s3_class(result, "StanFit")
  expect_s3_class(result$draws(), "draws_array")
  expect_s3_class(result$sampler_diagnostics(), "draws_array")
  expect_s3_class(suppressWarnings(result$summary()), "draws_summary")
  expect_true(all(result$return_codes() == 0L))
  expect_false(any(
    c("init", "inv_metric") %in%
      names(result$metadata()$arguments)
  ))
  expect_false("save_metric" %in% names(result$metadata()))
})

test_that("sampling rejects a non-logical flag argument", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 20,
      iter_sampling = 20,
      chains = 1,
      seed = 42,
      show_messages = "yes",
      num_threads = test_threads()
    ),
    "`show_messages` must be TRUE or FALSE"
  )
})

test_that("sampling output() returns non-empty Stan log messages", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_type(result$output(), "character")
  expect_true(length(result$output()) > 0)
})

test_that("show_messages = FALSE silences the console but output() still works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- expect_silent(
    mod$sample(
      data = data,
      iter_warmup = 20,
      iter_sampling = 20,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    )
  )

  expect_true(length(result$output()) > 0)
})

test_that("explicit tuning values are plumbed into the native args", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    adapt_delta = 0.95,
    max_treedepth = 15L,
    metric = "dense_e",
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  args <- result$metadata()$arguments
  expect_equal(args$delta, 0.95)
  expect_equal(args$max_depth, 15L)
  expect_equal(args$metric, "dense_e")
})

test_that("sampling with multiple chains works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 2,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(all(result$return_codes() == 0L))
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with fixed_param algorithm works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 10,
    iter_sampling = 20,
    chains = 1,
    fixed_param = TRUE,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("fixed_param sampler diagnostics keep whatever columns the service emits", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 2,
    fixed_param = TRUE,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  diagnostics <- result$sampler_diagnostics()
  expect_true("accept_stat__" %in% posterior::variables(diagnostics))
  expect_equal(posterior::nchains(diagnostics), 2L)
})

test_that("sampling with fixed_param and save_warmup warns and drops warmup draws", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_warning(
    result <- mod$sample(
      data = data,
      iter_warmup = 10,
      iter_sampling = 20,
      chains = 1,
      fixed_param = TRUE,
      save_warmup = TRUE,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "save_warmup.*ignored.*fixed_param"
  )

  expect_equal(result$return_codes(), 0L)
  expect_equal(posterior::niterations(result$draws()), 20L)
  expect_error(result$draws(inc_warmup = TRUE), "warmup draws were not saved")
})

test_that("sampling with fixed_param and save_warmup warns when iter_sampling is small", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_warning(
    result <- mod$sample(
      data = data,
      iter_warmup = 10,
      iter_sampling = 5,
      chains = 1,
      fixed_param = TRUE,
      save_warmup = TRUE,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "save_warmup.*ignored.*fixed_param"
  )

  expect_equal(result$return_codes(), 0L)
  expect_equal(posterior::niterations(result$draws()), 5L)
})

test_that("sampling with adapt_engaged = FALSE works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    adapt_engaged = FALSE,
    step_size = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("sampling with adapt_engaged = FALSE and iter_warmup = 0 works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 0,
    iter_sampling = 100,
    chains = 1,
    adapt_engaged = FALSE,
    step_size = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("sampling with adapt_engaged = TRUE and iter_warmup = 0 fails with a clear message", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 0,
    iter_sampling = 100,
    chains = 1,
    adapt_engaged = TRUE,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(all(result$return_codes() != 0L))
  expect_match(
    paste(result$output(), collapse = "\n"),
    "num_warmup must be > 0"
  )
})

test_that("sampling with inv_metric (diag_e) works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  # Bernoulli model has 1 unconstrained parameter (theta)
  # Identity inverse metric is just [1]
  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    metric = "diag_e",
    inv_metric = c(1.0),
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with a per-chain inv_metric list works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 2,
    engine = "nuts",
    metric = "diag_e",
    inv_metric = list(c(1), c(1)),
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(all(result$return_codes() == 0L))
})

test_that("sampling with inv_metric (diag_e, too short) fails with a clear message", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    metric = "diag_e",
    inv_metric = numeric(0),
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(all(result$return_codes() != 0L))
  expect_match(
    paste(result$output(), collapse = "\n"),
    "inverse Euclidean metric"
  )
})

test_that("sampling with inv_metric (diag_e, too long) fails with a clear message", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    metric = "diag_e",
    inv_metric = c(1, 1),
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(all(result$return_codes() != 0L))
  expect_match(
    paste(result$output(), collapse = "\n"),
    "inverse Euclidean metric"
  )
})

test_that("sampling with inv_metric (dense_e, wrong shape) fails with a clear message", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    metric = "dense_e",
    inv_metric = matrix(1, 2, 2),
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(all(result$return_codes() != 0L))
  expect_match(
    paste(result$output(), collapse = "\n"),
    "Cannot get inverse metric"
  )
})

test_that("fit$inv_metric() returns per-chain matrices for a diag_e adaptive fit", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 2,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  metric <- result$inv_metric()
  expect_length(metric, 2)
  for (m in metric) {
    expect_true(is.matrix(m))
    expect_equal(dim(m), c(1, 1))
  }

  metric_vec <- result$inv_metric(matrix = FALSE)
  expect_length(metric_vec, 2)
  for (v in metric_vec) {
    expect_false(is.matrix(v))
    expect_length(v, 1)
  }
})

test_that("fit$inv_metric() returns dense matrices for metric = 'dense_e' and errors on matrix = FALSE", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "nuts",
    metric = "dense_e",
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  metric <- result$inv_metric()
  expect_length(metric, 1)
  expect_true(is.matrix(metric[[1]]))
  expect_equal(dim(metric[[1]]), c(1, 1))

  expect_error(
    result$inv_metric(matrix = FALSE),
    "only available for diagonal metrics"
  )
})

test_that("fit$inv_metric() errors when sampling ran without adaptation", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 0,
    iter_sampling = 50,
    chains = 1,
    adapt_engaged = FALSE,
    step_size = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_error(result$inv_metric(), "no adapted metric is available")
})

test_that("fit$metadata()$step_size_adaptation reports one adapted step size per chain", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 2,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  step_size_adaptation <- result$metadata()$step_size_adaptation
  expect_true(is.numeric(step_size_adaptation))
  expect_length(step_size_adaptation, 2)
  expect_true(all(step_size_adaptation > 0))
})

test_that("fit$metadata() omits step_size_adaptation when sampling ran without adaptation", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 0,
    iter_sampling = 50,
    chains = 1,
    adapt_engaged = FALSE,
    step_size = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_false("step_size_adaptation" %in% names(result$metadata()))
})

test_that("sampling with save_warmup increases draws", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result_no_warmup <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = FALSE,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  result_warmup <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = TRUE,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_error(
    result_no_warmup$draws(inc_warmup = TRUE),
    "warmup draws were not saved"
  )
  expect_gt(
    posterior::ndraws(result_warmup$draws(inc_warmup = TRUE)),
    posterior::ndraws(result_warmup$draws())
  )
})

test_that("sampler_diagnostics(inc_warmup = TRUE) errors like draws() when warmup was not saved", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = FALSE,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_error(
    result$sampler_diagnostics(inc_warmup = TRUE),
    "warmup draws were not saved"
  )
})

test_that("sampler_diagnostics(inc_warmup = TRUE) includes warmup iterations when saved", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = TRUE,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(
    posterior::niterations(result$sampler_diagnostics(inc_warmup = TRUE)),
    100L
  )
})

test_that("sampling with static HMC engine works (unit_e, adapt)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "unit_e",
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
  expect_false("int_time__" %in% posterior::variables(result$draws()))
  expect_true(
    "int_time__" %in% posterior::variables(result$sampler_diagnostics())
  )
})

test_that("sampling with static HMC engine works (unit_e, no adapt)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "unit_e",
    adapt_engaged = FALSE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (diag_e, adapt)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "diag_e",
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (diag_e, no adapt)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "diag_e",
    adapt_engaged = FALSE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (dense_e, adapt)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  # Bernoulli model has 1 unconstrained parameter
  # Dense identity metric is just a 1x1 matrix
  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "dense_e",
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC engine works (dense_e, no adapt)", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "dense_e",
    adapt_engaged = FALSE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("sampling with static HMC + inv_metric (diag_e) works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  # Bernoulli model has 1 unconstrained parameter
  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 1,
    engine = "static",
    metric = "diag_e",
    inv_metric = c(1.0),
    adapt_engaged = TRUE,
    step_size = 1,
    int_time = 10,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
})

test_that("static HMC with multiple chains throws a configuration error", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 50,
      iter_sampling = 50,
      chains = 2,
      engine = "static",
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Static HMC only supports a single chain"
  )
})

test_that("sampling with an invalid engine errors before reaching C++", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 50,
      iter_sampling = 50,
      chains = 1,
      engine = "typo",
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`engine` must be one of"
  )
})

test_that("sampling with an invalid metric errors before reaching C++", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 50,
      iter_sampling = 50,
      chains = 1,
      metric = "typo",
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`metric` must be one of"
  )
})

test_that("num_threads is accepted and plumbed to the native args", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result_one <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    num_threads = 1,
    seed = 42,
    show_messages = FALSE
  )
  expect_true(all(result_one$return_codes() == 0L))
  expect_equal(result_one$metadata()$arguments$num_threads, 1L)

  result_two <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    num_threads = 2,
    seed = 42,
    show_messages = FALSE
  )
  expect_true(all(result_two$return_codes() == 0L))
  expect_equal(result_two$metadata()$arguments$num_threads, 2L)
})

test_that("sampling with num_threads = 0 errors", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 20,
      iter_sampling = 20,
      chains = 1,
      num_threads = 0,
      seed = 42,
      show_messages = FALSE
    ),
    "`num_threads` must be a single integer >= 1"
  )
})

test_that("sampling with a non-numeric iter_warmup errors", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = "abc",
      iter_sampling = 20,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`iter_warmup` must be a single integer"
  )
})

# ---------------------------------------------------------------------------
# $diagnostic_summary() method
# ---------------------------------------------------------------------------

test_that("diagnostic_summary() returns one row per chain with expected columns", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    chains = 3,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  summary_df <- result$diagnostic_summary()

  expect_true(is.data.frame(summary_df))
  expect_equal(
    colnames(summary_df),
    c("chain", "num_divergent", "num_max_treedepth")
  )
  expect_equal(nrow(summary_df), 3L)
  expect_equal(summary_df$chain, 1:3)
  expect_type(summary_df$num_divergent, "integer")
  expect_type(summary_df$num_max_treedepth, "integer")

  # Summing the per-chain counts must equal counting divergences/treedepth
  # hits across the combined draws.
  diagnostics <- result$sampler_diagnostics(format = "draws_matrix")
  expect_equal(
    sum(summary_df$num_divergent),
    sum(diagnostics[, "divergent__"] > 0)
  )
  expect_equal(
    sum(summary_df$num_max_treedepth),
    sum(diagnostics[, "treedepth__"] >= 10L)
  )

  expect_s3_class(result$sampler_diagnostics(), "draws_array")
  expect_s3_class(
    result$sampler_diagnostics(format = "draws_matrix"),
    "draws_matrix"
  )
})

test_that("diagnostic_summary() chain column reflects supplied chain_ids", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 2,
    chain_ids = 5:6,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  summary_df <- result$diagnostic_summary()
  expect_equal(summary_df$chain, 5:6)
})

test_that("diagnostic_summary() returns NA_integer_ per row for fixed_param runs", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 10,
    iter_sampling = 20,
    chains = 2,
    fixed_param = TRUE,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  summary_df <- result$diagnostic_summary()
  expect_equal(nrow(summary_df), 2L)
  expect_true(all(is.na(summary_df$num_divergent)))
  expect_true(all(is.na(summary_df$num_max_treedepth)))
  expect_type(summary_df$num_divergent, "integer")
  expect_type(summary_df$num_max_treedepth, "integer")
})

test_that("diagnostic_summary() counts per-chain divergences for a pathological model", {
  funnel_code <- "
    parameters {
      real y;
      vector[30] x;
    }
    model {
      y ~ normal(0, 3);
      x ~ normal(0, exp(y / 2));
    }
  "
  mod <- stan_model(
    code = funnel_code,
    model_name = "funnel_diagnostic_summary"
  )

  # Neal's funnel with a short warmup and a low adapt_delta reliably
  # triggers divergent transitions (verified across several seeds).
  result <- mod$sample(
    iter_warmup = 15,
    iter_sampling = 200,
    chains = 3,
    adapt_delta = 0.6,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  summary_df <- result$diagnostic_summary()
  expect_equal(nrow(summary_df), 3L)
  # Neal's funnel reliably produces some divergences under default tuning;
  # the point of this test is that divergences are actually counted (not
  # just that the structural NA/zero cases work), not exactly how many.
  expect_true(sum(summary_df$num_divergent) > 0)
})

withr::deferred_run()
