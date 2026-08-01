# Tests for the shared normalization and defaults layer

test_that("bundled defaults contain expected keys for each service", {
  defaults <- newstan:::.newstan_defaults

  expect_true(all(c("sample", "optimize", "laplace", "variational",
                     "pathfinder", "diagnose") %in% names(defaults)))

  # Sampling defaults
  expect_equal(defaults$sample$iter_warmup, 1000L)
  expect_equal(defaults$sample$iter_sampling, 1000L)
  expect_equal(defaults$sample$max_treedepth, 10L)
  expect_equal(defaults$sample$adapt_delta, 0.8)
  expect_equal(defaults$sample$step_size, 1)
  expect_equal(defaults$sample$metric, "diag_e")

  # Optimization defaults
  expect_equal(defaults$optimize$algorithm, "lbfgs")
  expect_equal(defaults$optimize$iter, 2000L)
  expect_equal(defaults$optimize$jacobian, FALSE)

  # Laplace defaults
  expect_equal(defaults$laplace$jacobian, TRUE)
  expect_equal(defaults$laplace$draws, 1000L)

  # ADVI defaults
  expect_equal(defaults$variational$algorithm, "meanfield")
  expect_equal(defaults$variational$iter, 10000L)
  expect_equal(defaults$variational$draws, 1000L)

  # Pathfinder defaults
  expect_equal(defaults$pathfinder$num_paths, 4L)
  expect_equal(defaults$pathfinder$single_path_draws, 1000L)
  expect_equal(defaults$pathfinder$draws, 1000L)

  # Diagnose defaults
  expect_equal(defaults$diagnose$epsilon, 1e-6)
  expect_equal(defaults$diagnose$error, 1e-6)
})


test_that("sample normalization resolves NULL to bundled defaults", {
  args <- newstan:::.newstan_normalize_sample(
    seed = 42L, chains = 2, chain_ids = 1:2
  )

  expect_equal(args$iter_warmup, 1000L)
  expect_equal(args$iter_sampling, 1000L)
  expect_equal(args$max_treedepth, 10L)
  expect_equal(args$adapt_delta, 0.8)
  expect_equal(args$step_size, 1)
  expect_equal(args$metric, "diag_e")
  expect_equal(args$chains, 2L)
  expect_equal(args$chain_ids, 1:2)
  expect_equal(args$seed, 42L)
  expect_equal(args$data, list())
})


test_that("sample normalization respects explicit values", {
  args <- newstan:::.newstan_normalize_sample(
    seed = 42L, chains = 1, chain_ids = 1L,
    iter_warmup = 500L, iter_sampling = 500L,
    adapt_delta = 0.95, max_treedepth = 15L,
    metric = "dense_e"
  )

  expect_equal(args$iter_warmup, 500L)
  expect_equal(args$iter_sampling, 500L)
  expect_equal(args$adapt_delta, 0.95)
  expect_equal(args$max_treedepth, 15L)
  expect_equal(args$metric, "dense_e")
})


test_that("sample normalization rejects invalid chains", {
  expect_snapshot(error = TRUE, {
    newstan:::.newstan_normalize_sample(
      seed = 42L, chains = 0, chain_ids = integer()
    )
  })
})


test_that("sample normalization rejects non-consecutive chain_ids", {
  expect_snapshot(error = TRUE, {
    newstan:::.newstan_normalize_sample(
      seed = 42L, chains = 2, chain_ids = c(1L, 3L)
    )
  })
})


test_that("optimize normalization resolves NULL to defaults", {
  args <- newstan:::.newstan_normalize_optimize(seed = 42L)

  expect_equal(args$algorithm, "lbfgs")
  expect_equal(args$iter, 2000L)
  expect_equal(args$jacobian, FALSE)
  expect_equal(args$threads, 1L)
})


test_that("laplace normalization rejects mode and opt_args together", {
  expect_snapshot(error = TRUE, {
    newstan:::.newstan_normalize_laplace(
      seed = 42L, mode = c(theta = 0.5), opt_args = list(iter = 100L)
    )
  })
})


test_that("variational normalization resolves NULL to defaults", {
  args <- newstan:::.newstan_normalize_variational(seed = 42L)

  expect_equal(args$algorithm, "meanfield")
  expect_equal(args$iter, 10000L)
  expect_equal(args$draws, 1000L)
  expect_equal(args$threads, 1L)
})


test_that("pathfinder normalization resolves NULL to defaults", {
  args <- newstan:::.newstan_normalize_pathfinder(seed = 42L)

  expect_equal(args$num_paths, 4L)
  expect_equal(args$single_path_draws, 1000L)
  expect_equal(args$draws, 1000L)
  expect_equal(args$threads, 1L)
})


test_that("diagnose normalization resolves NULL to defaults", {
  args <- newstan:::.newstan_normalize_diagnose(seed = 42L)

  expect_equal(args$epsilon, 1e-6)
  expect_equal(args$error, 1e-6)
})


test_that("common normalization resolves NULL seed", {
  args <- newstan:::.newstan_normalize_common()

  expect_true(is.integer(args$seed))
  expect_true(args$seed >= 1 && args$seed <= .Machine$integer.max)
  expect_equal(args$data, list())
  expect_equal(args$refresh, 100L)
})


test_that("run-result schema has expected structure", {
  schema <- newstan:::.newstan_run_result_schema(method = "sample")

  expect_equal(schema$method, "sample")
  expect_true(is.list(schema$draws))
  expect_true("post_warmup" %in% names(schema$draws))
  expect_true("warmup" %in% names(schema$draws))
  expect_true(is.list(schema$sampler_diagnostics))
  expect_type(schema$return_codes, "integer")
  expect_type(schema$chain_ids, "integer")
  expect_true(is.list(schema$init))
  expect_true(is.list(schema$metric))
  expect_type(schema$step_size, "double")
  expect_true(is.list(schema$timing))
  expect_true(is.list(schema$profiles))
  expect_true(is.list(schema$structured))
  expect_true(is.list(schema$metadata))
})
