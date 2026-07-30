test_that("reduce_sum model runs with a configured TBB thread pool", {
  mod <- stan_model(file = test_path("test-models/reduce_sum_normal.stan"))

  result <- sampling(
    mod,
    data = list(N = 8, y = c(-1.1, -0.4, -0.2, 0, 0.3, 0.7, 0.9, 1.2)),
    num_warmup = 10,
    num_samples = 10,
    num_chains = 1,
    num_threads = 2,
    seed = 123,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(all(c("mu", "sigma", "log_lik") %in%
    posterior::variables(result$draws)))
})

test_that("ODE model evaluates solver output during fixed-parameter sampling", {
  mod <- stan_model(file = test_path("test-models/ode_decay.stan"))

  result <- sampling(
    mod,
    data = list(T = 3, ts = c(0.5, 1, 2), y0 = 4, rate = 0.5),
    algorithm = "fixed_param",
    num_warmup = 0,
    num_samples = 2,
    num_chains = 1,
    seed = 123,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_equal(
    as.numeric(result$draws$concentration.1),
    rep(4 * exp(-0.5 * 0.5), 2),
    tolerance = 1e-5
  )
})

test_that("hierarchical non-centred logistic model samples", {
  mod <- stan_model(file = test_path("test-models/hierarchical_logistic.stan"))
  data <- list(
    N = 8,
    K = 2,
    G = 2,
    X = rbind(c(1, -1), c(1, -0.5), c(1, 0), c(1, 0.5),
              c(1, 1), c(1, -0.8), c(1, 0.3), c(1, 0.8)),
    group = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    y = c(0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L)
  )

  result <- sampling(
    mod, data,
    num_warmup = 10,
    num_samples = 10,
    num_chains = 1,
    seed = 123,
    verbose = FALSE
  )

  expect_equal(result$return_code, 0L)
  expect_true(all(c("beta.1", "tau.1", "Omega.1.1", "y_rep.1") %in%
    posterior::variables(result$draws)))
})
