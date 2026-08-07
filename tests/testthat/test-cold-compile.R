local_test_context()

init_test_cache("cold-compile")

test_that("a cold-cache compile emits no warnings and uses a PCH", {
  skip_on_cran()
  mod <- NULL
  expect_no_warning(
    mod <- stan_model(
      code = "parameters { real theta; } model { theta ~ normal(0, 1); }"
    )
  )
  expect_true(mod$is_compiled())
  pch_flags <- stanr:::.stanr_pch_flags(stanr:::.stanr_base_cppflags())
  expect_true(nzchar(pch_flags))
  # The returned flags name a real, on-disk PCH.
  pch_path <- sub("^.*-include-pch ['\"]?([^'\"]+).*$", "\\1", pch_flags)
  expect_true(file.exists(pch_path))
})

withr::deferred_run()
