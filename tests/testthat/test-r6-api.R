local_test_context()

init_test_cache("r6-api")

test_that("the old procedural API is not exported", {
  expect_false(any(
    c(
      "sampling",
      "optimizing",
      "laplace",
      "advi",
      "variational",
      "pathfinder",
      "generated_quantities",
      "diagnose",
      "gradient_check"
    ) %in%
      getNamespaceExports("newstan")
  ))
})

test_that("each StanModel service method returns the documented fit class", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data
  args <- list(data = data, seed = 42, show_messages = FALSE)

  mcmc <- do.call(
    mod$sample,
    c(args, list(iter_warmup = 20, iter_sampling = 20, chains = 1))
  )
  expect_s3_class(mcmc, "StanMCMC")

  mle <- do.call(mod$optimize, args)
  expect_s3_class(mle, "StanMLE")

  laplace <- do.call(mod$laplace, c(args, list(jacobian = FALSE)))
  expect_s3_class(laplace, "StanLaplace")

  vb <- do.call(mod$variational, args)
  expect_s3_class(vb, "StanVB")

  pf <- do.call(mod$pathfinder, args)
  expect_s3_class(pf, "StanPathfinder")

  gq <- do.call(
    mod$generate_quantities,
    c(
      list(fitted_params = posterior::as_draws_matrix(mcmc$draws())),
      args
    )
  )
  expect_s3_class(gq, "StanGQ")

  diag <- suppressMessages(mod$diagnose(data = data, seed = 42))
  expect_s3_class(diag, "StanDiagnose")

  for (fit in list(mcmc, mle, laplace, vb, pf, gq, diag)) {
    expect_s3_class(fit, "StanFit")
    expect_s3_class(fit, "R6")
  }
})

withr::deferred_run()
