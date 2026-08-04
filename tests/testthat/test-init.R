local_test_context()

init_test_cache("init")

# Tests for resolve_init(), pure R with no Stan compilation involved.

test_that("resolve_init() accepts a non-negative radius", {
  result <- stanr:::resolve_init(2)
  expect_equal(result$radius, 2)
  expect_equal(result$values, list())

  result <- stanr:::resolve_init(0)
  expect_equal(result$radius, 0)
  expect_equal(result$values, list())
})

test_that("resolve_init() treats NULL as the default radius", {
  result <- stanr:::resolve_init(NULL)
  expect_equal(result$radius, 2)
  expect_equal(result$values, list())
})

test_that("resolve_init() rejects a negative radius with a radius-specific message", {
  expect_error(
    stanr:::resolve_init(-1),
    "`init` as a radius must be a single non-negative number"
  )
})

test_that("resolve_init() accepts a named list of constrained values", {
  result <- stanr:::resolve_init(list(theta = 1))
  expect_equal(result$radius, 2)
  expect_equal(result$values, list(theta = 1))
})

test_that("resolve_init() rejects an unnamed list of per-chain init lists", {
  expect_error(
    stanr:::resolve_init(list(list(theta = 1), list(theta = 2))),
    "per-chain init lists are not supported"
  )
})

test_that("resolve_init() accepts a named numeric or complex vector", {
  result <- stanr:::resolve_init(c(theta = 1))
  expect_equal(result$radius, 2)
  expect_equal(result$values, list(theta = 1))

  result <- stanr:::resolve_init(c(z = 1 + 2i))
  expect_equal(result$radius, 2)
  expect_equal(result$values, list(z = 1 + 2i))
})

test_that("resolve_init() rejects an unnamed numeric vector", {
  expect_error(
    stanr:::resolve_init(c(1, 2)),
    "Numeric init must be a named vector of constrained parameters"
  )
})

test_that("resolve_init() rejects unsupported init types", {
  expect_error(
    stanr:::resolve_init(function() list()),
    "must be a non-negative radius"
  )
})

withr::deferred_run()
