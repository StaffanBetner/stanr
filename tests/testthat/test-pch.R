# Tests for precompiled-header subprocess memoization (see .newstan_memo)

reset_pch_memo <- function() {
  rm(list = ls(envir = newstan:::.newstan_memo), envir = newstan:::.newstan_memo)
}

test_that("second identical .newstan_pch_flags() call makes no additional .newstan_system2 calls", {
  reset_pch_memo()
  on.exit(reset_pch_memo(), add = TRUE)

  call_count <- 0
  testthat::local_mocked_bindings(
    .newstan_system2 = function(...) {
      call_count <<- call_count + 1
      # An empty/unrecognized compiler probe result routes
      # `.newstan_pch_flags()` into its "unsupported compiler" branch before
      # any cache-directory or PCH-build filesystem work happens, keeping
      # this test side-effect free.
      character()
    }
  )

  flags <- "-Ifoo -DBAR"

  suppressWarnings(suppressMessages(
    newstan:::.newstan_pch_flags(flags)
  ))
  calls_after_first <- call_count
  expect_gte(calls_after_first, 1)

  suppressWarnings(suppressMessages(
    newstan:::.newstan_pch_flags(flags)
  ))
  calls_after_second <- call_count

  expect_equal(calls_after_second - calls_after_first, 0)
})


test_that(".newstan_r_config() memoizes per variable and routes through .newstan_system2", {
  reset_pch_memo()
  on.exit(reset_pch_memo(), add = TRUE)

  call_count <- 0
  testthat::local_mocked_bindings(
    .newstan_system2 = function(...) {
      call_count <<- call_count + 1
      "mocked-value"
    }
  )

  first <- newstan:::.newstan_r_config("CXX")
  expect_equal(call_count, 1)
  expect_equal(first, "mocked-value")

  second <- newstan:::.newstan_r_config("CXX")
  expect_equal(call_count, 1)
  expect_equal(second, first)

  newstan:::.newstan_r_config("CXXFLAGS")
  expect_equal(call_count, 2)
})


test_that(".newstan_dependency_cppflags() memoizes across calls without shelling out", {
  reset_pch_memo()
  on.exit(reset_pch_memo(), add = TRUE)

  call_count <- 0
  testthat::local_mocked_bindings(
    .newstan_system2 = function(...) {
      call_count <<- call_count + 1
      character()
    }
  )

  first <- newstan:::.newstan_dependency_cppflags()
  second <- newstan:::.newstan_dependency_cppflags()

  expect_identical(first, second)
  # RcppParallel::CxxFlags() is captured in-process, so this never shells out.
  expect_equal(call_count, 0)
})
