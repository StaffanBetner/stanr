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


test_that("seed resolution resolves NULL seed", {
  seed <- newstan:::.newstan_seed(NULL)

  expect_true(is.integer(seed))
  expect_true(seed >= 1 && seed <= .Machine$integer.max)
})
