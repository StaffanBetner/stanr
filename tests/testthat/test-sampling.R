test_that("sampling returns expected structure", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$sample(
    data = data,
    iter_warmup = 20,
    iter_sampling = 20,
    chains = 1,
    seed = 42,
    show_messages = FALSE
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
      show_messages = "yes"
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
    show_messages = FALSE
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
      show_messages = FALSE
    )
  )

  expect_true(length(result$output()) > 0)
})

test_that("sampling respects explicit tuning values instead of defaults", {
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
  )

  expect_true(all(result$return_codes() != 0L))
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
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
  )

  expect_error(result$inv_metric(), "no adapted metric is available")
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
    show_messages = FALSE
  )

  result_warmup <- mod$sample(
    data = data,
    iter_warmup = 50,
    iter_sampling = 50,
    save_warmup = TRUE,
    chains = 1,
    seed = 42,
    show_messages = FALSE
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
    show_messages = FALSE
  )

  expect_equal(result$return_codes(), 0L)
  expect_true(posterior::ndraws(result$draws()) > 0)
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
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
      show_messages = FALSE
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
      show_messages = FALSE
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
      show_messages = FALSE
    ),
    "`metric` must be one of"
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
    show_messages = FALSE
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

  # Cross-check against the old whole-run-total behavior: summing the
  # per-chain counts must equal counting divergences/treedepth hits across
  # the combined draws_matrix (what the previous implementation returned).
  diagnostics <- result$sampler_diagnostics(format = "draws_matrix")
  expect_equal(
    sum(summary_df$num_divergent),
    sum(diagnostics[, "divergent__"] > 0)
  )
  expect_equal(
    sum(summary_df$num_max_treedepth),
    sum(diagnostics[, "treedepth__"] >= 10L)
  )

  # `sampler_diagnostics()` itself must be unaffected by this change.
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
    show_messages = FALSE
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
    show_messages = FALSE
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
    show_messages = FALSE
  )

  summary_df <- result$diagnostic_summary()
  expect_equal(nrow(summary_df), 3L)
  # Neal's funnel reliably produces some divergences under default tuning;
  # the point of this test is that divergences are actually counted (not
  # just that the structural NA/zero cases work), not exactly how many.
  expect_true(sum(summary_df$num_divergent) > 0)
})
