test_that("compute_model_hash produces consistent hashes", {
  skip_if_not(requireNamespace("digest", quietly = TRUE))

  code <- "model {}"
  h1 <- newstan:::compute_model_hash("m", code)
  h2 <- newstan:::compute_model_hash("m", code)
  expect_equal(h1, h2)

  # Different code should give different hash
  h3 <- newstan:::compute_model_hash("m", "parameters { real x; }")
  expect_false(h1 == h3)

  # Different model name should give different hash
  h4 <- newstan:::compute_model_hash("other", code)
  expect_false(h1 == h4)
})

test_that("cache_get returns NULL for uncached models", {
  skip_if_not(requireNamespace("digest", quietly = TRUE))
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))

  result <- newstan:::cache_get("nonexistent_model_xyz", "model {}")
  expect_null(result)
})
