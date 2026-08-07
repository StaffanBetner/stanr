# Compiled models now cache to a single file next to their `stan_file` (see
# `.stanr_build_cache_file()`, R/stan_model.R), so tests must never compile
# directly from `test_path("test-models", ...)`  -- that would write a
# `.so`/`.dll` sibling into the checked-in fixtures tree. `test_stan_file()`
# copies the whole fixtures tree into a session-scratch tempdir once (so
# `#include`/`include_paths`-relative lookups inside a copied `.stan` file
# still resolve against copied siblings) and every `stan_file =` test call
# site is routed through it instead of `test_path()` directly.
test_stan_file <- local({
  scratch_dir <- NULL
  function(relpath) {
    if (is.null(scratch_dir)) {
      scratch_dir <<- file.path(tempdir(), "test-models-scratch")
      dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
      file.copy(
        list.files(test_path("test-models"), full.names = TRUE),
        scratch_dir,
        recursive = TRUE
      )
    }
    file.path(scratch_dir, relpath)
  }
})

# Compiled-model cache shared across test files (helpers load once per run).
test_model <- local({
  models <- new.env(parent = emptyenv())
  function(name) {
    if (is.null(models[[name]])) {
      models[[name]] <- stan_model(
        stan_file = test_stan_file(paste0(name, ".stan"))
      )
    }
    models[[name]]
  }
})

bernoulli_data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

# Data for the tuple_complex_battery test model: exercises every tuple/complex
# shape it declares (scalar/vector/matrix/array complex, plain and nested and
# array-of tuples).
battery_data <- function() {
  list(
    zd = 1 + 2i,
    zv = c(1 + 1i, 2 - 2i),
    zm = matrix(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i), 2, 2),
    za = c(5 + 5i, 6 - 6i),
    td = list(1.5, c(2.5, 3.5)),
    tad = list(list(10L, 1 + 1i), list(20L, 2 + 2i)),
    acv = list(
      list(complex(real = 1:3, imaginary = 11:13), 100),
      list(complex(real = 4:6, imaginary = 14:16), 200)
    ),
    t2d = list(
      list(list(11L, 1.1), list(12L, 1.2)),
      list(list(21L, 2.1), list(22L, 2.2))
    ),
    nt = list(999, list(list(1, 1 + 2i), list(2, 3 + 4i)))
  )
}

# Initialise a unique PCH cache per test file, per run. Compiled models no
# longer go through an options-driven cache dir (see `test_stan_file()`
# above), but the PCH cache is still a shared, options-driven, on-disk cache
# and test files still get their own isolated copy of it.
init_test_cache <- function(test_name) {
  cache_path <- file.path(tempdir(), "test-cache", test_name)
  if (dir.exists(cache_path)) {
    unlink(cache_path, recursive = TRUE, force = TRUE)
  }

  options(stanr_pch_dir = file.path(cache_path, "pch"))
}

# `model_hash` (R/stan_model.R) is keyed on Stan code content alone, and the
# on-disk cache file persists for the whole test run -- so any two `code = `
# calls anywhere in the suite using textually identical Stan source would
# otherwise share a cache entry (including with a mocked/fake compile from a
# PCH test). Wrap any throwaway `code` string in this to keep it unique to
# its call site.
unique_stan_code <- function(
  body = "parameters { real theta; } model { theta ~ normal(0, 1); }"
) {
  paste0("// ", basename(tempfile()), "\n", body)
}

test_threads <- function() {
  if (utils::getFromNamespace("on_cran", "testthat")()) {
    1
  } else {
    RcppParallel::defaultNumThreads()
  }
}
