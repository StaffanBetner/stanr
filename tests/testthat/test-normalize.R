# Tests for the shared normalization and defaults layer

test_that("chain validation rejects invalid chains", {
  expect_snapshot(error = TRUE, {
    newstan:::.newstan_validate_chains(chains = 0, chain_ids = integer())
  })
})


test_that("chain validation rejects non-consecutive chain_ids", {
  expect_snapshot(error = TRUE, {
    newstan:::.newstan_validate_chains(chains = 2, chain_ids = c(1L, 3L))
  })
})


test_that("seed validation rejects an invalid seed", {
  expect_snapshot(error = TRUE, {
    newstan:::.newstan_seed(-1)
  })
})


test_that("common normalization resolves NULL seed", {
  args <- newstan:::.newstan_normalize_common()

  expect_true(is.integer(args$seed))
  expect_true(args$seed >= 1 && args$seed <= .Machine$integer.max)
  expect_equal(args$data, list())
})
