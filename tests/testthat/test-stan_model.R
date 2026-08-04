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

test_that("stan_model validates the cpp_options argument", {
  expect_error(
    stan_model(code = "parameters { real x; }", cpp_options = "not-a-list"),
    "`cpp_options` must be a list"
  )
  expect_error(
    stan_model(code = "parameters { real x; }", cpp_options = list(1)),
    "Unnamed `cpp_options` entries must each be a single string"
  )
  expect_error(
    stan_model(
      code = "parameters { real x; }",
      cpp_options = list("not an assignment")
    ),
    "form '<NAME> = <value>' or '<NAME> \\+= <value>'"
  )
  expect_error(
    stan_model(
      code = "parameters { real x; }",
      cpp_options = list(CXXFLAGS = list("-O3"))
    ),
    "single non-missing string, number, or logical"
  )
  # The same name may legitimately appear more than once (e.g. an
  # overriding assignment followed by an appending one), so this must not
  # error.
  expect_no_error(
    stan_model(
      code = "parameters { real x; }",
      cpp_options = list(CXXFLAGS = "-O3", "CXXFLAGS += -Wall"),
      compile = FALSE
    )
  )
})

test_that("stan_model stores and reports cpp_options via $cpp_options()", {
  mod <- stan_model(
    code = "parameters { real x; }",
    cpp_options = list(CXXFLAGS = "-O3", STAN_THREADS = TRUE),
    compile = FALSE
  )
  expect_equal(
    mod$cpp_options(),
    list(CXXFLAGS = "-O3", STAN_THREADS = TRUE)
  )
})

test_that("a named cpp_options entry compiles successfully (overrides, doesn't break the build)", {
  # Overriding CXXFLAGS drops newstan's own -O3 -g0 optimization flags, but
  # must not break the build: Makeconf's own CXXFLAGS still apply via the
  # outer `withr::with_makevars(assignment = "+=")` layer, and PKG_CPPFLAGS
  # (carrying the `-I` include path newstan's headers need) is untouched.
  mod <- stan_model(
    code = '
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ',
    precompiled_headers = FALSE,
    cpp_options = list(CXXFLAGS = "-O0")
  )
  expect_true(mod$is_compiled())
})

test_that(".newstan_parse_cpp_options parses named, '=', and '+=' entries", {
  parsed <- newstan:::.newstan_parse_cpp_options(
    list(
      CXX = "g++",
      "CXXFLAGS = -O3",
      "CXXFLAGS += -Wno-psabi",
      THREADS = TRUE
    )
  )
  expect_equal(
    parsed,
    list(
      list(name = "CXX", op = "=", value = "g++"),
      list(name = "CXXFLAGS", op = "=", value = "-O3"),
      list(name = "CXXFLAGS", op = "+=", value = "-Wno-psabi"),
      list(name = "THREADS", op = "=", value = "TRUE")
    )
  )
})

test_that(".newstan_apply_makevars overrides on '=' and appends on '+='", {
  base <- c(CXXFLAGS = "-g -O2", PKG_LIBS = "-lfoo")

  # A named argument (i.e. `op = "="`) overrides rather than appends.
  overridden <- newstan:::.newstan_apply_makevars(
    base,
    list(list(name = "CXXFLAGS", op = "=", value = "-O3"))
  )
  expect_equal(unname(overridden[["CXXFLAGS"]]), "-O3")

  # `+=` appends to the existing value instead of replacing it.
  appended <- newstan:::.newstan_apply_makevars(
    base,
    list(list(name = "CXXFLAGS", op = "+=", value = "-Wno-psabi"))
  )
  expect_equal(unname(appended[["CXXFLAGS"]]), "-g -O2 -Wno-psabi")

  # `+=` on a name absent from `base` degrades to a plain set.
  new_var <- newstan:::.newstan_apply_makevars(
    base,
    list(list(name = "LDFLAGS", op = "+=", value = "-lbar"))
  )
  expect_equal(unname(new_var[["LDFLAGS"]]), "-lbar")

  # Assignments are applied in order: override then append composes.
  composed <- newstan:::.newstan_apply_makevars(
    base,
    list(
      list(name = "CXXFLAGS", op = "=", value = "-O3"),
      list(name = "CXXFLAGS", op = "+=", value = "-Wall")
    )
  )
  expect_equal(unname(composed[["CXXFLAGS"]]), "-O3 -Wall")
})

# Persistent, cross-session model cache ---------------------------------------

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
  cpp_files_after <- list.files(
    cache_dir,
    pattern = "[.]cpp$",
    full.names = TRUE
  )
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
  # matters on every platform is that this rebuild produced exactly one
  # fresh artifact in that directory.
  fresh <- artifact_after[file.info(artifact_after)$mtime > rebuild_started]
  expect_length(fresh, 1L)

  # `mod` above still holds the superseded build mapped into this session.
  # Rcpp retires it with file.remove(), which POSIX honours even while the
  # file is mapped, so only the fresh artifact survives there. Windows
  # refuses to unlink a mapped DLL (sourceCpp's own removal fails with
  # "Permission denied"), so the superseded artifact legitimately lingers
  # until the session ends -- a platform property, not a stale duplicate
  # newstan failed to clean up.
  if (.Platform$OS.type != "windows") {
    expect_equal(length(artifact_after), 1L)
  }
})

test_that("model_hash is computed before stanc() is called, so a warm cache skips stanc entirely", {
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))

  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "newstan"
  )

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())
  expect_lte(call_count, 1L)

  # A second compile of identical code in the same session is a cache hit:
  # stanc() must not be invoked again.
  mod2 <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod2$is_compiled())
  expect_lte(call_count, 1L)

  # force_recompile = TRUE forces regeneration of the cached .cpp (and thus
  # a fresh stanc() call) even though the hash is unchanged.
  mod3 <- stan_model(
    code = code,
    precompiled_headers = FALSE,
    force_recompile = TRUE
  )
  expect_true(mod3$is_compiled())
  expect_equal(call_count, 2L)
})

test_that("the newstan_force_recompile option forces a fresh compile on a warm cache", {
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))

  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "newstan"
  )

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())
  expect_lte(call_count, 1L)

  withr::local_options(newstan_force_recompile = TRUE)
  mod2 <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod2$is_compiled())
  expect_equal(call_count, 2L)
})

test_that("changing external_cpp file contents (same path) changes model_hash and triggers a fresh compile", {
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))
  cache_dir <- getOption("newstan_cache_dir")

  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "newstan"
  )

  external_cpp_dir <- withr::local_tempdir()
  external_cpp_path <- file.path(external_cpp_dir, "external_mean.hpp")
  writeLines(
    c(
      "#include <ostream>",
      "",
      "template <typename T__>",
      "T__ external_mean(const T__& x, std::ostream* pstream__) {",
      "  return x;",
      "}"
    ),
    external_cpp_path
  )

  code <- paste(
    "functions { real external_mean(real x); }",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  mod <- stan_model(
    code = code,
    external_cpp = external_cpp_path,
    precompiled_headers = FALSE
  )
  expect_true(mod$is_compiled())
  expect_equal(call_count, 1L)

  cpp_files_before <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_length(cpp_files_before, 1L)

  # Same path, different contents: the returned value doubles rather than
  # passing `x` straight through, so the hash (keyed on file contents) must
  # differ even though `external_cpp_path` is unchanged.
  writeLines(
    c(
      "#include <ostream>",
      "",
      "template <typename T__>",
      "T__ external_mean(const T__& x, std::ostream* pstream__) {",
      "  return x + x;",
      "}"
    ),
    external_cpp_path
  )

  mod2 <- stan_model(
    code = code,
    external_cpp = external_cpp_path,
    precompiled_headers = FALSE
  )
  expect_true(mod2$is_compiled())
  expect_equal(call_count, 2L)

  cpp_files_after <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_length(cpp_files_after, 2L)
})

test_that("changing cpp_options changes model_hash and triggers a fresh compile", {
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))
  cache_dir <- getOption("newstan_cache_dir")

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '

  # `+=` (not a named override) so the internally computed `PKG_CPPFLAGS`
  # (which carries the `-I` include path newstan's own headers need) is
  # appended to rather than replaced.
  mod <- stan_model(
    code = code,
    precompiled_headers = FALSE,
    cpp_options = list("PKG_CPPFLAGS += -DNEWSTAN_TEST_FLAG=1")
  )
  expect_true(mod$is_compiled())
  cpp_files_before <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_length(cpp_files_before, 1L)

  # Same Stan code, different `cpp_options`: must be treated as a distinct
  # compiled artifact, not reused from the first model's cache entry.
  mod2 <- stan_model(
    code = code,
    precompiled_headers = FALSE,
    cpp_options = list("PKG_CPPFLAGS += -DNEWSTAN_TEST_FLAG=2")
  )
  expect_true(mod2$is_compiled())
  cpp_files_after <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_length(cpp_files_after, 2L)
})

test_that("newstan_clear_cache() removes the models/pch cache dirs and a later compile recreates them", {
  cache_home <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_home)
  cache_root <- tools::R_user_dir("newstan", "cache")
  # newstan_clear_cache() always targets tools::R_user_dir()'s default root
  # (see its docs), never the getOption() overrides -- so these must resolve
  # to that same default root, or compilation below (which does consult the
  # options) would land somewhere newstan_clear_cache() never looks.
  withr::local_options(
    newstan_cache_dir = file.path(cache_root, "models"),
    newstan_pch_dir = file.path(cache_root, "pch")
  )

  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())

  models_dir <- file.path(cache_root, "models")
  expect_true(dir.exists(models_dir))

  # `mod` is still loaded in this session. Windows will not unlink a mapped
  # DLL, so there newstan_clear_cache() removes all it can and warns about
  # the remainder; POSIX unlinks the mapped file and the tree goes entirely.
  on_windows <- .Platform$OS.type == "windows"
  if (on_windows) {
    expect_warning(
      freed <- newstan_clear_cache(),
      "Could not fully clear the newstan cache"
    )
  } else {
    freed <- newstan_clear_cache()
  }
  expect_setequal(names(freed), c("models", "pch"))
  expect_equal(unname(freed["models"]), models_dir)

  # The pch dir only ever holds .gch/.hpp files, never anything loadable, so
  # it comes out completely on both platforms.
  expect_false(dir.exists(file.path(cache_root, "pch")))

  if (on_windows) {
    # Everything that is not a still-mapped DLL must be gone.
    survivors <- normalizePath(
      list.files(models_dir, recursive = TRUE, full.names = TRUE),
      winslash = "/",
      mustWork = FALSE
    )
    expect_true(all(
      survivors %in%
        newstan:::.newstan_loaded_dlls_under(
          models_dir
        )
    ))
  } else {
    expect_false(dir.exists(models_dir))
  }

  # Calling it again with nothing left to release is a harmless no-op.
  expect_no_error(suppressWarnings(newstan_clear_cache()))

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
