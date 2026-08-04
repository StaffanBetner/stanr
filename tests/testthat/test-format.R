local_test_context()

init_test_cache("format")

test_that("format() returns tidied code without a stan_file and without compiling", {
  mod <- stan_model(
    code = "parameters{\nreal   x;\n}\nmodel{\nx~normal(0,1);\n}\n",
    compile = FALSE
  )

  expect_equal(
    mod$format(),
    "parameters {\n  real x;\n}\nmodel {\n  x ~ normal(0, 1);\n}\n"
  )
  expect_false(mod$is_compiled())
})

test_that("format() wraps lines at max_line_length", {
  mod <- stan_model(
    code = "parameters { real x; } model { x ~ normal(0, 1); }",
    compile = FALSE
  )

  expect_match(mod$format(max_line_length = 15), "normal\\s*\\n\\s*\\(0, 1\\)")
})

test_that("format() canonicalizes with TRUE or a character vector of transforms", {
  code <- "parameters {\n  real x;\n}\nmodel {\n  if (x > 0)\n    x ~ normal((0), 1);\n}\n"
  mod <- stan_model(code = code, compile = FALSE)

  expect_match(mod$format(canonicalize = TRUE), "normal(0, 1)", fixed = TRUE)
  expect_match(mod$format(canonicalize = "braces"), "if \\(x > 0\\) \\{")
})

test_that("format() rejects a non-logical, non-character canonicalize", {
  mod <- stan_model(code = "parameters { real x; }", compile = FALSE)

  expect_error(mod$format(canonicalize = 1), "canonicalize")
})

test_that("format() errors on invalid Stan code", {
  mod <- stan_model(code = "parameters { real x }", compile = FALSE)

  expect_error(mod$format(), "Syntax error")
})

test_that("format(overwrite_file = TRUE) writes the file and backs up the original", {
  path <- tempfile(fileext = ".stan")
  writeLines("parameters{\nreal   x;\n}\nmodel{\nx~normal(0,1);\n}\n", path)
  mod <- stan_model(stan_file = path, compile = FALSE)
  original_code <- mod$code()

  expect_message(
    result <- mod$format(overwrite_file = TRUE),
    "Old version of the model stored to"
  )

  expect_equal(
    paste(readLines(path), collapse = "\n"),
    sub("\\s*$", "", result)
  )
  backups <- list.files(
    dirname(path),
    pattern = paste0(basename(path), "\\.bak-")
  )
  expect_length(backups, 1L)
  # the live object keeps the code it was constructed with
  expect_equal(mod$code(), original_code)
})

test_that("format(overwrite_file = TRUE, backup = FALSE) skips the backup", {
  path <- tempfile(fileext = ".stan")
  writeLines("parameters { real x; } model { x ~ normal(0, 1); }", path)
  mod <- stan_model(stan_file = path, compile = FALSE)

  expect_no_message(mod$format(
    overwrite_file = TRUE,
    backup = FALSE,
    quiet = TRUE
  ))
  backups <- list.files(
    dirname(path),
    pattern = paste0(basename(path), "\\.bak-")
  )
  expect_length(backups, 0L)
})

test_that("format(overwrite_file = TRUE) requires a stan_file", {
  mod <- stan_model(
    code = "parameters { real x; } model { x ~ normal(0, 1); }",
    compile = FALSE
  )

  expect_error(mod$format(overwrite_file = TRUE), "stan_file")
})

test_that("format(overwrite_file = TRUE) refuses programs with #include directives", {
  mod <- stan_model(
    stan_file = test_path("test-models/include_model.stan"),
    include_paths = test_path("test-models/includes"),
    compile = FALSE
  )

  expect_error(mod$format(overwrite_file = TRUE), "#include")
})

withr::deferred_run()
