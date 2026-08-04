local_test_context()

init_test_cache("normalize")

# Tests for the shared normalization and defaults layer

test_that("chain validation rejects invalid chains", {
  expect_snapshot(error = TRUE, {
    stanr:::.stanr_validate_chains(chains = 0, chain_ids = integer())
  })
})


test_that("chain validation rejects non-consecutive chain_ids", {
  expect_snapshot(error = TRUE, {
    stanr:::.stanr_validate_chains(chains = 2, chain_ids = c(1L, 3L))
  })
})


test_that("chain validation rejects malformed `chains` values", {
  expect_error(
    stanr:::.stanr_validate_chains(chains = NA, chain_ids = integer()),
    "`chains` must be a single integer"
  )
  expect_error(
    stanr:::.stanr_validate_chains(chains = c(2, 3), chain_ids = 1:2),
    "`chains` must be a single integer"
  )
  expect_error(
    stanr:::.stanr_validate_chains(chains = "4", chain_ids = 1:4),
    "`chains` must be a single integer"
  )
})


test_that("inv_metric normalization warns and drops the metric under unit_e", {
  expect_warning(
    result <- stanr:::.stanr_normalize_inv_metric(
      inv_metric = 1,
      metric = "unit_e",
      chains = 1
    ),
    "ignored"
  )
  expect_null(result)
})

test_that("inv_metric normalization rejects a per-chain list of the wrong length", {
  expect_error(
    stanr:::.stanr_normalize_inv_metric(
      inv_metric = list(1, 2, 3),
      metric = "diag_e",
      chains = 2
    ),
    "one per chain"
  )
})

test_that("inv_metric normalization wraps a single metric in a length-1 list", {
  result <- stanr:::.stanr_normalize_inv_metric(
    inv_metric = 1,
    metric = "diag_e",
    chains = 3
  )
  expect_type(result, "list")
  expect_length(result, 1L)
  expect_equal(result[[1]], 1)
})

test_that("inv_metric normalization passes a correctly-sized per-chain list through", {
  metrics <- list(1, 2)
  result <- stanr:::.stanr_normalize_inv_metric(
    inv_metric = metrics,
    metric = "diag_e",
    chains = 2
  )
  expect_identical(result, metrics)
})


test_that("seed validation rejects an invalid seed", {
  expect_snapshot(error = TRUE, {
    stanr:::.stanr_seed(-1)
  })
})


test_that("seed resolution resolves NULL seed", {
  seed <- stanr:::.stanr_seed(NULL)

  expect_true(is.integer(seed))
  expect_true(seed >= 1 && seed <= .Machine$integer.max)
})

withr::deferred_run()
