test_that("use_opencl = TRUE stores and reports the flag without compiling", {
  mod <- stan_model(
    code = "
      data { int<lower=0> N; }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ",
    use_opencl = TRUE,
    compile = FALSE
  )

  expect_true(mod$use_opencl())
  expect_false(mod$is_compiled())
})
