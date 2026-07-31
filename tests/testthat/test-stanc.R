test_that("stanc resolves nested includes from include_directories", {
  code <- paste(readLines(test_path("test-models/include_model.stan")), collapse = "\n")

  cpp_code <- stanc(
    code,
    include_directories = test_path("test-models/includes")
  )

  expect_type(cpp_code, "character")
  expect_match(cpp_code, "shifted_normal_lpdf")
  expect_match(cpp_code, "centered")
})

test_that("stan_model forwards include_paths to stanc", {
  mod <- stan_model(
    stan_file = test_path("test-models/include_model.stan"),
    include_paths = test_path("test-models/includes")
  )

  expect_s3_class(mod, "StanModel")
  expect_true(mod$is_compiled())
})

test_that("stanc reports an unresolved include and its search path", {
  expect_error(
    stanc(
      'functions {\n#include "does-not-exist.stan"\n}',
      include_directories = test_path("test-models/includes")
    ),
    "Could not find Stan include file 'does-not-exist.stan'"
  )
})

test_that("stanc detects circular includes", {
  include_dir <- tempfile("newstan-includes-")
  dir.create(include_dir)
  writeLines('#include "second.stan"', file.path(include_dir, "first.stan"))
  writeLines('#include "first.stan"', file.path(include_dir, "second.stan"))

  expect_error(
    stanc(
      'functions {\n#include "first.stan"\n}',
      include_directories = include_dir
    ),
    "Circular Stan include detected"
  )
})

test_that("stanc prepends external C++ and permits declared C++ functions", {
  external_cpp <- c(
    test_path("test-models/external_mean.hpp"),
    test_path("test-models/external_marker.hpp")
  )
  code <- paste(
    "functions { real external_mean(real x); }",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  cpp_code <- stanc(code, external_cpp = external_cpp)

  expect_match(cpp_code, "^#include <ostream>")
  expect_match(cpp_code, "external_mean\\(x, pstream__\\)")
  expect_match(cpp_code, "external_cpp_marker")
  expect_lt(
    regexpr("external_mean", cpp_code)[1],
    regexpr("external_cpp_marker", cpp_code)[1]
  )
})

test_that("stan_model compiles a model using external C++", {
  code <- paste(
    "functions { real external_mean(real x); }",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  mod <- stan_model(
    code = code,
    external_cpp = test_path("test-models/external_mean.hpp")
  )

  expect_s3_class(mod, "StanModel")
  expect_true(mod$is_compiled())
})

test_that("stanc validates external C++ paths", {
  expect_error(
    stanc("parameters { real x; } model { x ~ normal(0, 1); }",
      external_cpp = "does-not-exist.hpp"
    ),
    "External C\\+\\+ files do not exist"
  )
})
