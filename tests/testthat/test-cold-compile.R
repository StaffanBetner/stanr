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
  pch_path <- stanr:::.stanr_pch_current(stanr:::.stanr_base_cppflags())
  expect_false(is.na(pch_path))
  expect_true(file.exists(pch_path))
})

withr::deferred_run()
