.newstan_expect_formals <- function(fun, expected) {
  expect_true(is.function(fun))
  expect_identical(names(formals(fun)), expected)
}

.newstan_generator <- function(class) {
  get(class, envir = asNamespace("newstan"), inherits = FALSE)
}

test_that("stan_model exposes the replacement constructor contract", {
  .newstan_expect_formals(
    stan_model,
    c(
      "stan_file",
      "code",
      "compile",
      "model_name",
      "include_paths",
      "user_header",
      "cpp_options",
      "stanc_options",
      "force_recompile",
      "precompiled_headers",
      "quiet",
      "external_cpp"
    )
  )
  expect_identical(formals(stan_model)$compile, TRUE)
  expect_identical(formals(stan_model)$quiet, TRUE)
  expect_identical(formals(stan_model)$cpp_options, quote(list()))
  expect_identical(formals(stan_model)$stanc_options, quote(list()))

  # The old procedural API is intentionally not retained during development.
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

test_that("all public result types are R6 classes with the Stan names", {
  class_names <- c(
    "StanModel",
    "StanFit",
    "StanMCMC",
    "StanMLE",
    "StanLaplace",
    "StanVB",
    "StanPathfinder",
    "StanGQ",
    "StanDiagnose"
  )

  generators <- lapply(class_names, .newstan_generator)
  names(generators) <- class_names
  for (class_name in class_names) {
    expect_true(
      R6::is.R6Class(generators[[class_name]]),
      info = paste(class_name, "is not an R6 generator")
    )
    expect_identical(generators[[class_name]]$classname, class_name)
  }

  fit_classes <- setdiff(class_names, c("StanModel", "StanFit"))
  for (class_name in fit_classes) {
    expect_identical(as.character(generators[[class_name]]$inherit), "StanFit")
  }
})

test_that("StanModel has the Phase 1-3 public surface", {
  methods <- .newstan_generator("StanModel")$public_methods
  expect_true(all(
    c(
      "sample",
      "optimize",
      "laplace",
      "variational",
      "pathfinder",
      "generate_quantities",
      "diagnose",
      "code",
      "print",
      "model_name",
      "stan_file",
      "has_stan_file",
      "include_paths",
      "stan_version",
      "compile",
      "variables"
    ) %in%
      names(methods)
  ))

  .newstan_expect_formals(
    methods$sample,
    c(
      "data",
      "seed",
      "refresh",
      "init",
      "save_latent_dynamics",
      "output_dir",
      "output_basename",
      "sig_figs",
      "chains",
      "parallel_chains",
      "chain_ids",
      "threads_per_chain",
      "opencl_ids",
      "iter_warmup",
      "iter_sampling",
      "save_warmup",
      "thin",
      "max_treedepth",
      "adapt_engaged",
      "adapt_delta",
      "step_size",
      "metric",
      "metric_file",
      "inv_metric",
      "init_buffer",
      "term_buffer",
      "window",
      "fixed_param",
      "show_messages",
      "show_exceptions",
      "diagnostics",
      "save_metric",
      "save_cmdstan_config",
      "engine",
      "int_time",
      "step_size_jitter",
      "adapt_gamma",
      "adapt_kappa",
      "adapt_t0"
    )
  )
  .newstan_expect_formals(
    methods$optimize,
    c(
      "data",
      "seed",
      "refresh",
      "init",
      "output_dir",
      "output_basename",
      "sig_figs",
      "threads",
      "opencl_ids",
      "algorithm",
      "jacobian",
      "init_alpha",
      "iter",
      "tol_obj",
      "tol_rel_obj",
      "tol_grad",
      "tol_rel_grad",
      "tol_param",
      "history_size",
      "show_messages",
      "show_exceptions",
      "save_cmdstan_config",
      "save_iterations"
    )
  )
  .newstan_expect_formals(
    methods$laplace,
    c(
      "data",
      "seed",
      "refresh",
      "init",
      "output_dir",
      "output_basename",
      "sig_figs",
      "threads",
      "opencl_ids",
      "mode",
      "opt_args",
      "jacobian",
      "draws",
      "show_messages",
      "show_exceptions",
      "save_cmdstan_config",
      "calculate_lp"
    )
  )
  .newstan_expect_formals(
    methods$variational,
    c(
      "data",
      "seed",
      "refresh",
      "init",
      "save_latent_dynamics",
      "output_dir",
      "output_basename",
      "sig_figs",
      "threads",
      "opencl_ids",
      "algorithm",
      "iter",
      "grad_samples",
      "elbo_samples",
      "eta",
      "adapt_engaged",
      "adapt_iter",
      "tol_rel_obj",
      "eval_elbo",
      "draws",
      "show_messages",
      "show_exceptions",
      "save_cmdstan_config"
    )
  )
  .newstan_expect_formals(
    methods$pathfinder,
    c(
      "data",
      "seed",
      "refresh",
      "init",
      "output_dir",
      "output_basename",
      "sig_figs",
      "threads",
      "opencl_ids",
      "init_alpha",
      "tol_obj",
      "tol_rel_obj",
      "tol_grad",
      "tol_rel_grad",
      "tol_param",
      "history_size",
      "single_path_draws",
      "draws",
      "num_paths",
      "max_lbfgs_iters",
      "num_elbo_draws",
      "save_single_paths",
      "psis_resample",
      "calculate_lp",
      "show_messages",
      "show_exceptions",
      "save_cmdstan_config"
    )
  )
  .newstan_expect_formals(
    methods$generate_quantities,
    c(
      "fitted_params",
      "data",
      "seed",
      "output_dir",
      "output_basename",
      "sig_figs",
      "parallel_chains",
      "threads_per_chain",
      "opencl_ids",
      "show_messages",
      "show_exceptions"
    )
  )
  .newstan_expect_formals(
    methods$diagnose,
    c(
      "data",
      "seed",
      "init",
      "output_dir",
      "output_basename",
      "epsilon",
      "error"
    )
  )

  expect_identical(formals(methods$sample)$chains, 4)
  expect_identical(formals(methods$sample)$save_warmup, FALSE)
  expect_identical(formals(methods$sample)$adapt_engaged, TRUE)
  expect_identical(formals(methods$sample)$fixed_param, FALSE)
  expect_identical(formals(methods$optimize)$jacobian, FALSE)
  expect_identical(formals(methods$laplace)$jacobian, TRUE)
  expect_identical(formals(methods$pathfinder)$num_paths, 4)
})

test_that("StanFit has common accessors and model methods", {
  methods <- .newstan_generator("StanFit")$public_methods
  expect_true(all(
    c(
      "draws",
      "summary",
      "print",
      "return_codes",
      "metadata",
      "time",
      "output",
      "init",
      "code",
      "materialize",
      "save_object",
      "init_model_methods",
      "log_prob",
      "grad_log_prob",
      "hessian",
      "unconstrain_variables",
      "unconstrain_draws",
      "variable_skeleton",
      "constrain_variables"
    ) %in%
      names(methods)
  ))

  .newstan_expect_formals(methods$init_model_methods, c("seed", "verbose"))
  .newstan_expect_formals(
    methods$log_prob,
    c("unconstrained_variables", "jacobian")
  )
  .newstan_expect_formals(
    methods$grad_log_prob,
    c("unconstrained_variables", "jacobian")
  )
  .newstan_expect_formals(
    methods$hessian,
    c("unconstrained_variables", "jacobian")
  )
  .newstan_expect_formals(methods$unconstrain_variables, "variables")
  .newstan_expect_formals(
    methods$unconstrain_draws,
    c("draws", "format", "inc_warmup")
  )
  .newstan_expect_formals(
    methods$variable_skeleton,
    c("transformed_parameters", "generated_quantities")
  )
  .newstan_expect_formals(
    methods$constrain_variables,
    c(
      "unconstrained_variables",
      "transformed_parameters",
      "generated_quantities"
    )
  )

  expect_identical(formals(methods$init_model_methods)$seed, 1)
  expect_identical(formals(methods$init_model_methods)$verbose, FALSE)
  expect_identical(formals(methods$log_prob)$jacobian, TRUE)
  expect_identical(formals(methods$grad_log_prob)$jacobian, TRUE)
  expect_identical(formals(methods$hessian)$jacobian, TRUE)
  expect_identical(
    formals(methods$variable_skeleton)$transformed_parameters,
    TRUE
  )
  expect_identical(
    formals(methods$variable_skeleton)$generated_quantities,
    TRUE
  )
})

test_that("code-only StanModel retains source metadata without compiling", {
  code <- paste(
    "parameters { real x; }",
    "model { x ~ std_normal(); }",
    sep = "\n"
  )
  model <- stan_model(code = code, compile = FALSE, model_name = "metadata")

  expect_s3_class(model, "StanModel")
  expect_s3_class(model, "R6")
  expect_identical(model$model_name(), "metadata")
  expect_identical(model$code(), code)
  expect_identical(model$stan_file(), character())
  expect_false(model$has_stan_file())
  expect_type(model$include_paths(), "character")
  expect_length(model$stan_version(), 1L)
})

test_that("model service methods adapt native payloads to R6 fits", {
  model <- stan_model(
    stan_file = test_path("test-models/bernoulli.stan"),
    quiet = TRUE
  )
  data <- list(N = 4L, y = c(1L, 0L, 1L, 0L))

  mcmc <- model$sample(
    data = data,
    seed = 123,
    chains = 1,
    parallel_chains = 1,
    iter_warmup = 2,
    iter_sampling = 2,
    adapt_engaged = FALSE,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE
  )
  mle <- model$optimize(
    data = data,
    seed = 123,
    init = list(theta = 0.5),
    iter = 20,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE
  )

  expect_s3_class(mcmc, "StanMCMC")
  expect_s3_class(mcmc, "StanFit")
  expect_s3_class(mcmc, "R6")
  expect_s3_class(mle, "StanMLE")
  expect_s3_class(mle, "StanFit")
  expect_type(mcmc$return_codes(), "integer")
  expect_length(mcmc$return_codes(), 1L)
  metadata <- mcmc$metadata()
  expect_type(metadata, "list")
  expect_false(is.null(names(metadata)))
  expect_s3_class(mcmc$draws(), "draws")
  expect_s3_class(mcmc$summary(), "draws_summary")
  expect_identical(mcmc$materialize(), mcmc)
})
