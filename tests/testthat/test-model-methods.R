.newstan_model_method_state <- new.env(parent = emptyenv())

.newstan_model_method_data <- function(double = FALSE) {
  y <- c(1L, 1L, 1L, 0L)
  if (double) {
    y <- rep(y, 2L)
  }
  list(N = length(y), y = y, mu = 0)
}

.newstan_model_method_model <- function() {
  if (!exists("model", envir = .newstan_model_method_state, inherits = FALSE)) {
    model <- stan_model(
      stan_file = test_path("test-models/model_methods.stan"),
      quiet = TRUE
    )
    assign("model", model, envir = .newstan_model_method_state)
  }
  get("model", envir = .newstan_model_method_state, inherits = FALSE)
}

.newstan_model_method_fit <- function(double = FALSE) {
  key <- if (double) "fit_double" else "fit"
  if (!exists(key, envir = .newstan_model_method_state, inherits = FALSE)) {
    fit <- .newstan_model_method_model()$optimize(
      data = .newstan_model_method_data(double),
      seed = 2468,
      init = list(theta = 0.5, beta = c(0.5, -0.5)),
      iter = 50,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE
    )
    assign(key, fit, envir = .newstan_model_method_state)
  }
  get(key, envir = .newstan_model_method_state, inherits = FALSE)
}

.newstan_model_method_mcmc <- function() {
  key <- "mcmc"
  if (!exists(key, envir = .newstan_model_method_state, inherits = FALSE)) {
    fit <- .newstan_model_method_model()$sample(
      data = .newstan_model_method_data(),
      seed = 1357,
      init = list(theta = 0.5, beta = c(0.5, -0.5)),
      chains = 2,
      parallel_chains = 2,
      iter_warmup = 2,
      iter_sampling = 3,
      save_warmup = TRUE,
      adapt_engaged = FALSE,
      step_size = 0.1,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE
    )
    assign(key, fit, envir = .newstan_model_method_state)
  }
  get(key, envir = .newstan_model_method_state, inherits = FALSE)
}

.newstan_expected_target <- function(upars, double = FALSE, jacobian = TRUE) {
  theta <- stats::plogis(upars[[1L]])
  beta <- upars[2:3]
  y <- .newstan_model_method_data(double)$y
  value <- sum(y) * log(theta) + (length(y) - sum(y)) * log1p(-theta)
  value <- value - 0.5 * sum(beta^2)
  if (jacobian) {
    value <- value + log(theta) + log1p(-theta)
  }
  value
}

test_that("log probability, gradient, and Hessian match analytical values", {
  fit <- .newstan_model_method_fit()
  upars <- c(0, 0.5, -0.5)

  expected_lp <- -6 * log(2) - 0.25
  expected_lp_no_jacobian <- -4 * log(2) - 0.25
  expected_gradient <- c(1, -0.5, 0.5)
  expected_hessian <- diag(c(-1.5, -1, -1))
  expected_hessian_no_jacobian <- diag(c(-1, -1, -1))

  # Calling a method before init_model_methods() must initialize lazily.
  expect_equal(fit$log_prob(upars), expected_lp, tolerance = 1e-10)
  expect_equal(
    fit$log_prob(upars, jacobian = FALSE),
    expected_lp_no_jacobian,
    tolerance = 1e-10
  )

  gradient <- fit$grad_log_prob(upars)
  expect_equal(as.numeric(gradient), expected_gradient, tolerance = 1e-9)
  expect_equal(attr(gradient, "log_prob"), expected_lp, tolerance = 1e-10)

  hessian <- fit$hessian(upars)
  expect_named(hessian, c("log_prob", "grad_log_prob", "hessian"))
  expect_equal(hessian$log_prob, expected_lp, tolerance = 1e-10)
  expect_equal(
    as.numeric(hessian$grad_log_prob),
    expected_gradient,
    tolerance = 1e-8
  )
  expect_equal(hessian$hessian, expected_hessian, tolerance = 1e-6)
  expect_equal(hessian$hessian, t(hessian$hessian), tolerance = 1e-12)

  hessian_no_jacobian <- fit$hessian(upars, jacobian = FALSE)
  expect_equal(
    hessian_no_jacobian$log_prob,
    expected_lp_no_jacobian,
    tolerance = 1e-10
  )
  expect_equal(
    as.numeric(hessian_no_jacobian$grad_log_prob),
    expected_gradient,
    tolerance = 1e-8
  )
  expect_equal(
    hessian_no_jacobian$hessian,
    expected_hessian_no_jacobian,
    tolerance = 1e-6
  )
})

test_that("constraining and unconstraining preserve structure and values", {
  fit <- .newstan_model_method_fit()
  upars <- c(0, 0.5, -0.5)

  parameters <- fit$constrain_variables(
    upars,
    transformed_parameters = FALSE,
    generated_quantities = FALSE
  )
  expect_named(parameters, c("theta", "beta"))
  expect_equal(parameters$theta, 0.5, tolerance = 1e-12)
  expect_equal(as.numeric(parameters$beta), c(0.5, -0.5), tolerance = 1e-12)
  expect_equal(fit$unconstrain_variables(parameters), upars, tolerance = 1e-10)

  with_tparams <- fit$constrain_variables(
    upars,
    transformed_parameters = TRUE,
    generated_quantities = FALSE
  )
  expect_named(with_tparams, c("theta", "beta", "beta_sum"))
  expect_equal(with_tparams$beta_sum, 0, tolerance = 1e-12)

  with_gqs <- fit$constrain_variables(upars)
  expect_named(
    with_gqs,
    c(
      "theta",
      "beta",
      "beta_sum",
      "deterministic_gq",
      "stochastic_gq"
    )
  )
  expect_equal(with_gqs$deterministic_gq, 0.5, tolerance = 1e-12)

  skeleton <- fit$variable_skeleton()
  expect_named(
    skeleton,
    c(
      "theta",
      "beta",
      "beta_sum",
      "deterministic_gq",
      "stochastic_gq"
    )
  )
  expect_identical(
    vapply(skeleton, length, integer(1)),
    c(
      theta = 1L,
      beta = 2L,
      beta_sum = 1L,
      deterministic_gq = 1L,
      stochastic_gq = 1L
    )
  )
  expect_named(
    fit$variable_skeleton(FALSE, FALSE),
    c("theta", "beta")
  )
})

test_that("model-method RNG is fit-local, advances, and can be reset", {
  fit <- .newstan_model_method_fit()
  upars <- c(0, 0.5, -0.5)

  fit$init_model_methods(seed = 901)
  first <- fit$constrain_variables(upars)$stochastic_gq
  second <- fit$constrain_variables(upars)$stochastic_gq
  expect_false(isTRUE(all.equal(first, second)))

  fit$init_model_methods(seed = 901)
  replay <- fit$constrain_variables(upars)$stochastic_gq
  expect_equal(replay, first, tolerance = 0)
})

test_that("model methods reject malformed or non-finite inputs", {
  fit <- .newstan_model_method_fit()

  for (method in c("log_prob", "grad_log_prob", "hessian")) {
    expect_error(fit[[method]](c(0, 1)), "3 unconstrained")
    expect_error(fit[[method]](c(0, 1, 2, 3)), "3 unconstrained")
    expect_error(fit[[method]](c(0, NA_real_, 1)), "finite|NA")
    expect_error(fit[[method]](c(0, Inf, 1)), "finite|Inf")
    expect_error(fit[[method]](c(0, 1, 2), jacobian = NA), "jacobian")
  }

  expect_error(fit$constrain_variables(c(0, 1)), "3 unconstrained")
  expect_error(
    fit$constrain_variables(c(0, 1, Inf)),
    "finite|Inf"
  )
  expect_error(
    fit$unconstrain_variables(list(theta = 0.5)),
    "beta|not provided|missing"
  )
  expect_error(
    fit$unconstrain_variables(list(theta = 0.5, beta = 1)),
    "beta|dimension|dims|size"
  )
  expect_error(
    fit$unconstrain_variables(list(theta = 1.5, beta = c(0, 0))),
    "theta|bounds|range"
  )

  expected <- fit$unconstrain_variables(list(theta = 0.5, beta = c(1, -1)))
  with_extra <- fit$unconstrain_variables(list(
    theta = 0.5,
    beta = c(1, -1),
    not_a_parameter = 42
  ))
  expect_equal(with_extra, expected)
})

test_that("fits from one model retain independent data-bound targets", {
  fit <- .newstan_model_method_fit()
  fit_double <- .newstan_model_method_fit(double = TRUE)
  upars <- c(0.4, 0.2, -0.1)

  target <- fit$log_prob(upars)
  target_double <- fit_double$log_prob(upars)
  expect_equal(
    target,
    .newstan_expected_target(upars),
    tolerance = 1e-10
  )
  expect_equal(
    target_double,
    .newstan_expected_target(upars, double = TRUE),
    tolerance = 1e-10
  )
  expect_false(isTRUE(all.equal(target, target_double)))

  # Re-evaluating the first fit detects accidental shared mutable model state.
  expect_equal(fit$log_prob(upars), target, tolerance = 0)
})

test_that("unconstrain_draws preserves posterior dimensions and formats", {
  fit <- .newstan_model_method_mcmc()
  constrained <- posterior::as_draws_matrix(fit$draws(format = "draws_matrix"))
  unconstrained <- fit$unconstrain_draws(format = "draws_matrix")

  expect_s3_class(unconstrained, "draws_matrix")
  expect_identical(nrow(unconstrained), nrow(constrained))
  expect_identical(ncol(unconstrained), 3L)
  expect_identical(
    posterior::variables(unconstrained),
    c("theta", "beta[1]", "beta[2]")
  )
  expect_equal(
    unconstrained[, "theta"],
    stats::qlogis(constrained[, "theta"]),
    tolerance = 1e-8
  )
  expect_equal(
    unconstrained[, c("beta[1]", "beta[2]")],
    constrained[, c("beta[1]", "beta[2]")],
    tolerance = 1e-10
  )

  unconstrained_array <- fit$unconstrain_draws()
  expect_s3_class(unconstrained_array, "draws_array")
  expect_identical(posterior::nchains(unconstrained_array), 2L)
  expect_identical(posterior::niterations(unconstrained_array), 3L)

  with_warmup <- fit$unconstrain_draws(inc_warmup = TRUE)
  expect_identical(posterior::nchains(with_warmup), 2L)
  expect_identical(posterior::niterations(with_warmup), 5L)

  supplied <- fit$unconstrain_draws(
    draws = fit$draws(format = "draws_array"),
    format = "draws_df"
  )
  expect_s3_class(supplied, "draws_df")
  expect_identical(posterior::nchains(supplied), 2L)
  expect_identical(posterior::niterations(supplied), 3L)
})

test_that("serialized fits lazily rebuild data-bound model methods", {
  fit <- .newstan_model_method_fit()
  upars <- c(0.2, -0.3, 0.4)
  expected_lp <- fit$log_prob(upars)
  expected_gradient <- fit$grad_log_prob(upars)
  expected_hessian <- fit$hessian(upars)
  expected_parameters <- fit$constrain_variables(
    upars,
    transformed_parameters = FALSE,
    generated_quantities = FALSE
  )

  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(fit, path)
  restored <- readRDS(path)

  expect_s3_class(restored, "StanMLE")
  expect_s3_class(restored$draws(), "draws")
  expect_equal(restored$log_prob(upars), expected_lp, tolerance = 1e-10)
  expect_equal(
    restored$grad_log_prob(upars),
    expected_gradient,
    tolerance = 1e-8
  )
  restored_hessian <- restored$hessian(upars)
  expect_equal(restored_hessian$log_prob, expected_hessian$log_prob)
  expect_equal(
    restored_hessian$grad_log_prob,
    expected_hessian$grad_log_prob,
    tolerance = 1e-8
  )
  expect_equal(
    restored_hessian$hessian,
    expected_hessian$hessian,
    tolerance = 1e-6
  )
  expect_equal(
    restored$constrain_variables(
      upars,
      transformed_parameters = FALSE,
      generated_quantities = FALSE
    ),
    expected_parameters,
    tolerance = 1e-10
  )
  expect_equal(
    restored$unconstrain_variables(expected_parameters),
    upars,
    tolerance = 1e-10
  )
})
