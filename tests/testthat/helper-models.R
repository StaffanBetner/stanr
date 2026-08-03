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
