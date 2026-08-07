local_test_context()

init_test_cache("harvest")

# A standalone probe TU declaring every model support export as a trivial
# stub (uniform signature -- only symbol presence matters for binding) plus
# one extra probe function, so `.stanr_load_build()`/`.stanr_restore_build_cache()`
# can be exercised without compiling a full model. The probe uses only the
# plain C API (no Rcpp types) so it needs no Rcpp callables at runtime.
probe_cpp <- function(...) {
  stubs <- vapply(
    stanr:::.stanr_model_support_exports,
    function(name) {
      paste0("extern \"C\" SEXP ", name,
             "(SEXP a, SEXP b, SEXP c, SEXP d, SEXP e, SEXP f) { return R_NilValue; }")
    },
    character(1)
  )
  c("#include <R.h>", "#include <Rinternals.h>", stubs, ...)
}

test_that("inst/stan_model.cpp declares exactly the reserved export list", {
  cpp_file <- system.file("stan_model.cpp", package = "stanr", mustWork = TRUE)
  src <- readLines(cpp_file, warn = FALSE)
  # Every model support export must be declared as an `extern "C"` SEXP
  # routine in the model TU.
  for (name in stanr:::.stanr_model_support_exports) {
    expect_true(
      any(grepl(paste0("^SEXP ", name, "\\("), src)),
      info = paste0("missing export: ", name)
    )
  }
})

test_that("append key/compile/load round trip: hash and bindings survive a real build", {
  build_dir <- withr::local_tempdir()
  cpp_file <- file.path(build_dir, "probe.cpp")
  writeLines(
    probe_cpp(
      "extern \"C\" SEXP stanr_test_add_one(SEXP x) {",
      "  return Rf_ScalarInteger(Rf_asInteger(x) + 1);",
      "}"
    ),
    cpp_file
  )

  stanr:::.stanr_append_build_key(cpp_file, "test-hash-123")
  lib_file <- stanr:::.stanr_compile(
    cpp_file = cpp_file,
    cppflags = "",
    libs = "",
    extra_assignments = list(),
    verbose = FALSE
  )
  expect_true(file.exists(lib_file))

  env <- new.env()
  hash <- stanr:::.stanr_load_build(lib_file, env)
  expect_equal(hash, "test-hash-123")
  # Every model support export is bound as a .Call routine.
  for (name in stanr:::.stanr_model_support_exports) {
    expect_true(is.function(env[[name]]), info = paste0("missing binding: ", name))
  }
  # The extra probe function is reachable via its own symbol.
  dll <- dyn.load(lib_file)
  add_one <- function(...) {
    .Call(getNativeSymbolInfo("stanr_test_add_one", dll)$address, ...)
  }
  expect_equal(add_one(41), 42)
})

test_that("write/restore build cache preserves the embedded hash and rejects a mismatch", {
  build_dir <- withr::local_tempdir()
  cpp_file <- file.path(build_dir, "probe.cpp")
  writeLines(
    probe_cpp(
      "extern \"C\" SEXP stanr_test_double(SEXP x) {",
      "  return Rf_ScalarInteger(Rf_asInteger(x) * 2);",
      "}"
    ),
    cpp_file
  )
  stanr:::.stanr_append_build_key(cpp_file, "cache-hash-456")
  lib_file <- stanr:::.stanr_compile(
    cpp_file = cpp_file,
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
  expect_true(is.function(env$new_model))

  env2 <- new.env()
  expect_false(
    stanr:::.stanr_restore_build_cache(cache_file, "wrong-hash", env2)
  )
})

withr::deferred_run()
