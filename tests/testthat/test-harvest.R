local_test_context()

init_test_cache("harvest")

test_that("harvesting inst/stan_model.cpp exports exactly the reserved export list", {
  cpp_file <- system.file("stan_model.cpp", package = "stanr", mustWork = TRUE)
  harvest <- stanr:::.stanr_harvest_exports(cpp_file, withr::local_tempdir())
  expect_setequal(harvest$exports, stanr:::.stanr_model_support_exports)
})

test_that("harvest/embed/compile/load round trip: hash and bindings survive a real build", {
  build_dir <- withr::local_tempdir()
  cpp_file <- file.path(build_dir, "probe.cpp")
  writeLines(
    c(
      "#include <Rcpp.h>",
      "// [[Rcpp::export]]",
      "int stanr_test_add_one(int x) { return x + 1; }"
    ),
    cpp_file
  )

  harvest <- stanr:::.stanr_harvest_exports(cpp_file, build_dir)
  expect_equal(harvest$exports, "stanr_test_add_one")

  stanr:::.stanr_append_build_info(
    harvest$cpp_file,
    "test-hash-123",
    harvest$loader
  )
  lib_file <- stanr:::.stanr_compile(
    cpp_file = harvest$cpp_file,
    cppflags = "",
    libs = "",
    extra_assignments = list(),
    verbose = FALSE
  )
  expect_true(file.exists(lib_file))

  env <- new.env()
  hash <- stanr:::.stanr_load_build(lib_file, env)
  expect_equal(hash, "test-hash-123")
  expect_equal(env$stanr_test_add_one(41), 42)
})

test_that("write/restore build cache preserves the embedded hash and rejects a mismatch", {
  build_dir <- withr::local_tempdir()
  cpp_file <- file.path(build_dir, "probe.cpp")
  writeLines(
    c(
      "#include <Rcpp.h>",
      "// [[Rcpp::export]]",
      "int stanr_test_double(int x) { return x * 2; }"
    ),
    cpp_file
  )
  harvest <- stanr:::.stanr_harvest_exports(cpp_file, build_dir)
  stanr:::.stanr_append_build_info(
    harvest$cpp_file,
    "cache-hash-456",
    harvest$loader
  )
  lib_file <- stanr:::.stanr_compile(
    cpp_file = harvest$cpp_file,
    cppflags = "",
    libs = "",
    extra_assignments = list(),
    verbose = FALSE
  )

  cache_file <- file.path(
    withr::local_tempdir(),
    paste0("probe", .Platform$dynlib.ext)
  )
  stanr:::.stanr_write_build_cache(cache_file, lib_file)
  expect_true(file.exists(cache_file))

  env <- new.env()
  expect_true(stanr:::.stanr_restore_build_cache(cache_file, "cache-hash-456", env))
  expect_equal(env$stanr_test_double(21), 42)

  env2 <- new.env()
  expect_false(
    stanr:::.stanr_restore_build_cache(cache_file, "wrong-hash", env2)
  )
})

withr::deferred_run()
