test_that("stan_model compiles from file", {
  path <- test_path("test-models/bernoulli.stan")
  mod <- stan_model(stan_file = path)
  expect_s3_class(mod, "StanModel")
  expect_s3_class(mod, "R6")
  expect_true(mod$is_compiled())
})

test_that("stan_model compiles from code", {
  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code)
  expect_s3_class(mod, "StanModel")
  expect_true(mod$is_compiled())
})

test_that("stan_model errors when neither file nor code given", {
  expect_snapshot(stan_model(), error = TRUE)
})

test_that("stan_model errors when both file and code given", {
  path <- test_path("test-models/bernoulli.stan")
  expect_snapshot(
    stan_model(stan_file = path, code = "parameters { real x; }"),
    error = TRUE
  )
})

test_that("stan_model validates the precompiled_headers argument", {
  expect_error(
    stan_model(code = "parameters { real x; }", precompiled_headers = NA),
    "`precompiled_headers` must be TRUE or FALSE"
  )
})

test_that("stan_model errors on missing file", {
  expect_snapshot(stan_model(stan_file = "nonexistent.stan"), error = TRUE)
})

# Persistent, cross-session model cache (Task A1) -----------------------------

test_that("stan_model writes the .cpp and a built artifact under the resolved cache dir, not tempdir", {
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())

  cache_dir <- getOption("newstan_cache_dir")
  expect_true(dir.exists(cache_dir))

  cpp_files <- list.files(cache_dir, pattern = "[.]cpp$", full.names = TRUE)
  built_artifacts <- list.files(
    cache_dir,
    pattern = "[.](so|dylib|dll)$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_gte(length(cpp_files), 1L)
  expect_gte(length(built_artifacts), 1L)

  # A second compile of identical code in the same session is a cache hit:
  # no new .cpp is written (the pre-existing file.exists() guard).
  n_cpp_before <- length(cpp_files)
  mod2 <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod2$is_compiled())
  cpp_files_after <- list.files(cache_dir, pattern = "[.]cpp$", full.names = TRUE)
  expect_equal(length(cpp_files_after), n_cpp_before)
})

test_that("force_recompile refreshes the cached artifact in place under cache_dir", {
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))
  cache_dir <- getOption("newstan_cache_dir")

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())

  artifact_before <- list.files(
    cache_dir,
    pattern = "[.](so|dylib|dll)$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_gte(length(artifact_before), 1L)
  artifact_dir <- unique(dirname(artifact_before))
  expect_length(artifact_dir, 1L)
  rebuild_started <- Sys.time()
  Sys.sleep(1)

  mod2 <- stan_model(
    code = code,
    precompiled_headers = FALSE,
    force_recompile = TRUE
  )
  expect_true(mod2$is_compiled())

  artifact_after <- list.files(
    artifact_dir,
    pattern = "[.](so|dylib|dll)$",
    full.names = TRUE
  )
  # The rebuild happens in place, under the same sourceCpp build directory
  # inside cache_dir (sourceCpp itself may version the built filename, e.g.
  # sourceCpp_2.so -> sourceCpp_3.so, removing the old one -- see
  # Rcpp:::.sourceCppDynlibInsert / .sourceCppFindCacheEntryIndex). What
  # matters is that exactly one built artifact exists in that directory
  # both before and after (no stale duplicate lingers alongside the fresh
  # one), and the surviving artifact is freshly built by this rebuild.
  expect_equal(length(artifact_after), 1L)
  expect_gt(file.info(artifact_after)$mtime, rebuild_started)
})

test_that("newstan_clear_cache() removes the models/pch cache dirs and a later compile recreates them", {
  cache_home <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_home)

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())

  cache_root <- tools::R_user_dir("newstan", "cache")
  models_dir <- file.path(cache_root, "models")
  expect_true(dir.exists(models_dir))

  freed <- newstan_clear_cache()
  expect_setequal(names(freed), c("models", "pch"))
  expect_equal(unname(freed["models"]), models_dir)
  expect_false(dir.exists(models_dir))
  expect_false(dir.exists(file.path(cache_root, "pch")))

  # Calling it again with nothing cached is a harmless no-op.
  expect_no_error(newstan_clear_cache())

  mod2 <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod2$is_compiled())
  expect_true(dir.exists(models_dir))
})

test_that("stan_model falls back to tempdir when the cache dir is unwritable", {
  cache_root <- withr::local_tempdir()
  unwritable <- file.path(cache_root, "readonly")
  dir.create(unwritable)
  old_mode <- file.mode(unwritable)
  Sys.chmod(unwritable, mode = "0500")
  on.exit(Sys.chmod(unwritable, mode = old_mode), add = TRUE)
  skip_if_not(
    file.access(unwritable, 2) != 0,
    "cannot make a directory unwritable in this environment (e.g. running as root)"
  )
  withr::local_options(newstan_cache_dir = unwritable)

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  expect_no_warning(
    mod <- stan_model(code = code, precompiled_headers = FALSE)
  )
  expect_true(mod$is_compiled())
  # Nothing should have been written into the unwritable dir itself.
  expect_equal(length(list.files(unwritable)), 0L)
})
