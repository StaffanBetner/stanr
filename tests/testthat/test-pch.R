# Tests for precompiled-header subprocess memoization (see .newstan_memo)

reset_pch_memo <- function() {
  rm(
    list = ls(envir = newstan:::.newstan_memo),
    envir = newstan:::.newstan_memo
  )
}

test_that("second identical .newstan_pch_flags() call makes no additional .newstan_system2 calls", {
  reset_pch_memo()
  on.exit(reset_pch_memo(), add = TRUE)

  call_count <- 0
  testthat::local_mocked_bindings(
    # A non-empty CXX17 lets `.newstan_compiler_identity()` reach its
    # `--version` probe (mocked below), which is what needs to be exercised.
    .newstan_rcmd = function(...) "cc",
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


test_that(".newstan_r_config() memoizes per variable and routes through .newstan_rcmd", {
  reset_pch_memo()
  on.exit(reset_pch_memo(), add = TRUE)

  call_count <- 0
  testthat::local_mocked_bindings(
    .newstan_rcmd = function(...) {
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


# The compile-failure retry gate in .compile_stan_model_environment()
# (R/stan_model.R) should only rebuild the PCH when there's evidence it's
# stale, not on every sourceCpp() failure. These tests fake out the actual
# subprocess compiles (via .newstan_system2, as above) *and* Rcpp::sourceCpp()
# itself, so no real (30-60s) PCH or model compile ever runs.

# A minimal .newstan_rcmd mock shared by both tests below: it answers the
# `R CMD config CXX17` probe (used by `.newstan_compiler_identity()` to find
# the compiler to run `--version` on) with a fixed compiler name, and every
# other `R CMD config <var>` probe (used while fingerprinting the PCH) with
# "".
mock_pch_rcmd <- function(args, ...) {
  if (identical(args, c("config", "CXX17"))) {
    return("clang++")
  }
  ""
}

# A minimal .newstan_system2 mock shared by both tests below: it answers the
# `clang++ --version` probe (so .newstan_pch_flags()'s compiler_type
# resolution is deterministic across dev machines and CI) with a fixed clang
# identity, and the `... pch` build invocation by creating an empty file at
# the path the real `make ... pch` recipe would have built -- faking a
# successful PCH build without invoking a real compiler. Each `pch`
# invocation is counted via `pch_build_calls`.
mock_pch_system2 <- function(pch_build_calls_env) {
  function(command, args = character(), stdout = TRUE, stderr = TRUE, ...) {
    if (identical(command, "clang++") && identical(args, "--version")) {
      return("clang version 15.0.0 (clang-1500.3.9.4)")
    }
    if (length(args) && identical(args[[length(args)]], "pch")) {
      pch_build_calls_env$n <- pch_build_calls_env$n + 1L
      pch_arg <- grep("PCH=", args, value = TRUE)[[1]]
      # `shQuote()` wraps in `'` under a POSIX shell but `"` under Windows'
      # cmd default, so strip whichever trailing quote is there.
      pch_path <- sub("['\"]$", "", sub(".*PCH=", "", pch_arg))
      dir.create(dirname(pch_path), recursive = TRUE, showWarnings = FALSE)
      file.create(pch_path)
      return(character())
    }
    character()
  }
}

test_that("compile failure with a fresh PCH does not trigger a PCH rebuild", {
  reset_pch_memo()
  on.exit(reset_pch_memo(), add = TRUE)
  cache_home <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_home)
  withr::local_options(
    newstan_cache_dir = file.path(cache_home, "models"),
    newstan_pch_dir = file.path(cache_home, "pch")
  )

  pch_build_calls <- new.env()
  pch_build_calls$n <- 0L
  testthat::local_mocked_bindings(
    .newstan_rcmd = mock_pch_rcmd,
    .newstan_system2 = mock_pch_system2(pch_build_calls)
  )

  sourceCpp_calls <- 0L
  testthat::local_mocked_bindings(
    sourceCpp = function(...) {
      sourceCpp_calls <<- sourceCpp_calls + 1L
      stop("simulated genuine model compile error")
    },
    .package = "Rcpp"
  )

  code <- "parameters { real theta; } model { theta ~ normal(0, 1); }"
  expect_error(
    newstan:::.compile_stan_model_environment(
      code = code,
      model_name = "pch_fresh_test"
    ),
    "simulated genuine model compile error"
  )
  # One initial PCH build (there was none cached yet) and one compile
  # attempt -- but no rebuild-and-retry, because the PCH that build just
  # produced is not stale relative to its dependency headers.
  expect_equal(pch_build_calls$n, 1L)
  expect_equal(sourceCpp_calls, 1L)
})

test_that("compile failure after model_pch.hpp becomes newer than the PCH triggers exactly one rebuild-and-retry", {
  reset_pch_memo()
  on.exit(reset_pch_memo(), add = TRUE)
  cache_home <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_home)
  withr::local_options(
    newstan_cache_dir = file.path(cache_home, "models"),
    newstan_pch_dir = file.path(cache_home, "pch")
  )

  header <- system.file(
    "include",
    "newstan",
    "model_pch.hpp",
    package = "newstan",
    mustWork = TRUE
  )
  original_mtime <- file.mtime(header)
  on.exit(Sys.setFileTime(header, original_mtime), add = TRUE)

  pch_build_calls <- new.env()
  pch_build_calls$n <- 0L
  testthat::local_mocked_bindings(
    .newstan_rcmd = mock_pch_rcmd,
    .newstan_system2 = mock_pch_system2(pch_build_calls)
  )

  sourceCpp_calls <- 0L
  testthat::local_mocked_bindings(
    sourceCpp = function(...) {
      sourceCpp_calls <<- sourceCpp_calls + 1L
      # Fail only on the 2nd invocation (call B's primary attempt below);
      # the 1st (call A, establishing a fresh PCH) and 3rd (call B's
      # rebuild-and-retry) both "succeed".
      if (sourceCpp_calls == 2L) {
        stop("simulated genuine model compile error")
      }
      invisible(NULL)
    },
    .package = "Rcpp"
  )

  code <- "parameters { real theta; } model { theta ~ normal(0, 1); }"

  # Call A: establishes a fresh, on-disk PCH via a successful compile.
  newstan:::.compile_stan_model_environment(
    code = code,
    model_name = "pch_stale_test"
  )
  expect_equal(pch_build_calls$n, 1L)
  expect_equal(sourceCpp_calls, 1L)

  # Simulate model_pch.hpp changing after the PCH above was built (e.g. a
  # fresh `R CMD INSTALL` re-copying inst/include/) without anything else
  # (cppflags, newstan version, ...) changing.
  Sys.setFileTime(header, Sys.time() + 10)

  # Call B: identical code -> same model_hash (cpp_file cache hit, stanc()
  # not re-run) and the same memoized PCH flags as call A -- but the PCH on
  # disk is now stale relative to model_pch.hpp's bumped mtime, so the
  # compile-failure handler should rebuild it once and retry once.
  newstan:::.compile_stan_model_environment(
    code = code,
    model_name = "pch_stale_test"
  )
  expect_equal(sourceCpp_calls, 3L)
  expect_equal(pch_build_calls$n, 2L)
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
