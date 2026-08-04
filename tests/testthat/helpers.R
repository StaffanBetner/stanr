# Compiled-model cache shared across test files (helpers load once per run).
test_model <- local({
  models <- new.env(parent = emptyenv())
  function(name) {
    if (is.null(models[[name]])) {
      models[[name]] <- stan_model(
        stan_file = test_path("test-models", paste0(name, ".stan"))
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

# Initialise a unique cache per test file, per run.
init_test_cache <- function(test_name) {
  cache_path <- file.path(tempdir(), "test-cache", test_name)
  if (dir.exists(cache_path)) {
    unlink(cache_path, recursive = TRUE, force = TRUE)
  }

  options(
    newstan_cache_dir = file.path(cache_path, "models"),
    newstan_pch_dir = file.path(cache_path, "pch")
  )
}
