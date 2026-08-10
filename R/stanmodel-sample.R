# StanModel $sample() method
#' Run MCMC sampling
#'
#' @name model-method-sample
#' @aliases sample
#' @family StanModel methods
#'
#' @description The `$sample()` method of a [`StanModel`] object runs Stan's
#'   `"sample"` method, which uses Hamiltonian Monte Carlo (HMC) with the
#'   No-U-Turn Sampler (NUTS) to draw from the posterior distribution.
#'   Returns a [`StanMCMC`] object.
#'
#' @template param-data
#' @template param-seed
#' @param refresh (integer) How often (in iterations) to print progress.
#' @template param-init
#' @param chains (integer) The number of MCMC chains.
#' @param chain_ids (integer vector) The IDs for each chain.
#' @template param-num_threads
#' @param iter_warmup (integer) The number of warmup iterations.
#' @param iter_sampling (integer) The number of sampling iterations.
#' @param save_warmup (logical) Should warmup samples be saved? Ignored when
#'   `fixed_param = TRUE`, since that mode runs no warmup and so has nothing
#'   to save.
#' @param thin (integer) Thin interval.
#' @param max_treedepth (integer) Maximum tree depth for NUTS.
#' @param adapt_engaged (logical) Should step size adaptation be used?
#' @param adapt_delta (number) Target acceptance statistic for HMC.
#' @param step_size (number) Step size for HMC.
#' @param metric Character string indicating the metric to use: `"diag_e"`,
#'   `"dense_e"`, or `"unit_e"`.
#' @param inv_metric (numeric) Initial inverse mass matrix values.
#' @param init_buffer (integer) Adaptation phase: initial buffer length.
#' @param term_buffer (integer) Adaptation phase: terminal buffer length.
#' @param window (integer) Adaptation phase: window length.
#' @param fixed_param (logical) Treat all parameters as fixed (no adaptation).
#' @template param-show_messages
#' @template param-show_exceptions
#' @param diagnostics (character vector) Which sampler diagnostics to check
#'   immediately after sampling, printing a warning message for any problems
#'   found -- see [`$diagnostic_summary()`][fit-method-mcmc]. One or more of
#'   `"divergences"`, `"treedepth"`, and `"ebfmi"`. The default checks all
#'   three; `NULL` or `""` skips the check entirely. Ignored when
#'   `fixed_param = TRUE`.
#' @param engine (string) The sampling engine: `"nuts"`, `"static"`, or
#'   `"walnuts"` (see [walnutpie](https://github.com/flatironinstitute/walnutpie)).
#'   `walnuts` only supports `metric = "diag_e"` and `adapt_engaged = TRUE`.
#'   It reuses `max_treedepth` as the maximum trajectory doublings,
#'   `adapt_delta` as the step size acceptance target, and `step_size` as the
#'   initial step size; `refresh`, `step_size_jitter`, `init_buffer`,
#'   `term_buffer`, `window`, and `adapt_gamma`/`adapt_kappa`/`adapt_t0` are
#'   ignored, and no sampler diagnostics or progress output are produced.
#' @param int_time (number) Integration time for static HMC.
#' @param step_size_jitter (number) Jitter for step size after adaptation.
#' @param adapt_gamma (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_kappa (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_t0 (number) Adaptation hyperparameter for dual averaging.
#' @param max_step_halvings (integer) `walnuts` only: maximum number of step
#'   size halvings per macro step.
#' @param min_micro_steps (integer) `walnuts` only: minimum number of micro
#'   steps per macro step.
#' @param max_hamiltonian_error (number) `walnuts` only: maximum allowed
#'   error in the Hamiltonian at a macro step.
#' @param mass_init_count (number) `walnuts` only: initial count for the
#'   mass matrix estimator.
#' @param mass_additive_smoothing (number) `walnuts` only: additive
#'   smoothing for the mass matrix estimator.
#' @param max_macro_steps_target (number) `walnuts` only: target number of
#'   macro steps per iteration.
#' @param step_learning_rate (number) `walnuts` only: learning rate for the
#'   Adam step size adaptation.
#' @param step_gradient_decay (number) `walnuts` only: gradient decay rate
#'   for the Adam step size adaptation.
#' @param step_sq_gradient_decay (number) `walnuts` only: squared gradient
#'   decay rate for the Adam step size adaptation.
#' @param step_stabilization (number) `walnuts` only: stabilization term for
#'   the Adam step size adaptation.
#' @param step_learn_rate_decay (number) `walnuts` only: learning rate decay
#'   exponent for the Adam step size adaptation.
#' @template param-opencl_ids
#'
#' @return A [`StanMCMC`] object containing posterior draws and diagnostics.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_sample_impl <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  save_latent_dynamics = FALSE,
  chains = 4,
  chain_ids = seq_len(chains),
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  iter_warmup = 1000L,
  iter_sampling = 1000L,
  save_warmup = FALSE,
  thin = 1L,
  max_treedepth = 10L,
  adapt_engaged = TRUE,
  adapt_delta = 0.8,
  step_size = 1,
  metric = "diag_e",
  metric_file = NULL,
  inv_metric = NULL,
  init_buffer = 75L,
  term_buffer = 50L,
  window = 25L,
  fixed_param = FALSE,
  show_messages = TRUE,
  show_exceptions = TRUE,
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  engine = "nuts",
  int_time = 2 * pi,
  step_size_jitter = 0,
  adapt_gamma = 0.05,
  adapt_kappa = 0.75,
  adapt_t0 = 10,
  max_step_halvings = 5L,
  min_micro_steps = 1L,
  max_hamiltonian_error = 0.5,
  mass_init_count = 4,
  mass_additive_smoothing = 1e-5,
  max_macro_steps_target = 15,
  step_learning_rate = 0.05,
  step_gradient_decay = 0.8,
  step_sq_gradient_decay = 0.9,
  step_stabilization = 1e-4,
  step_learn_rate_decay = 0.5
) {
  save_latent_dynamics <- .stanr_flag(
    save_latent_dynamics,
    "save_latent_dynamics"
  )
  save_warmup <- .stanr_flag(save_warmup, "save_warmup")
  adapt_engaged <- .stanr_flag(adapt_engaged, "adapt_engaged")
  fixed_param <- .stanr_flag(fixed_param, "fixed_param")
  common <- .stanr_common_service_flags(
    show_messages,
    show_exceptions,
    opencl_ids,
    private,
    num_threads,
    refresh
  )
  show_messages <- common$show_messages
  show_exceptions <- common$show_exceptions
  num_threads <- common$num_threads
  refresh <- common$refresh
  if (is.null(diagnostics) || identical(diagnostics, "")) {
    diagnostics <- character()
  } else {
    diagnostics <- match.arg(
      diagnostics,
      choices = c("divergences", "treedepth", "ebfmi"),
      several.ok = TRUE
    )
  }
  if (fixed_param && save_warmup) {
    warning(
      "`save_warmup` is ignored when `fixed_param = TRUE`.",
      call. = FALSE
    )
    save_warmup <- FALSE
  }
  if (save_latent_dynamics) {
    stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
  }
  if (!is.null(metric_file)) {
    stop(
      "`metric_file` is not yet supported; supply `inv_metric` in memory.",
      call. = FALSE
    )
  }
  if (!engine %in% c("nuts", "static", "walnuts")) {
    stop(
      "`engine` must be one of \"nuts\", \"static\", \"walnuts\".",
      call. = FALSE
    )
  }
  if (!metric %in% c("diag_e", "dense_e", "unit_e")) {
    stop(
      "`metric` must be one of \"diag_e\", \"dense_e\", \"unit_e\".",
      call. = FALSE
    )
  }
  if (engine == "walnuts") {
    if (fixed_param) {
      stop(
        "`fixed_param` is not supported by `engine = \"walnuts\"`.",
        call. = FALSE
      )
    }
    if (!adapt_engaged) {
      stop(
        "`adapt_engaged = FALSE` is not supported by `engine = \"walnuts\"`.",
        call. = FALSE
      )
    }
    if (metric != "diag_e") {
      stop(
        "`engine = \"walnuts\"` only supports `metric = \"diag_e\"`.",
        call. = FALSE
      )
    }
  }
  ids <- .stanr_validate_chains(chains, chain_ids)
  chains <- ids$chains
  chain_ids <- ids$chain_ids
  iter_warmup <- .stanr_int(iter_warmup, "iter_warmup")
  iter_sampling <- .stanr_int(iter_sampling, "iter_sampling")
  thin <- .stanr_int(thin, "thin", min = 1L)
  # Double precision so valid int iteration counts can't overflow to NA.
  num_saved_draws <- ceiling(as.double(iter_sampling) / as.double(thin))
  if (save_warmup) {
    num_saved_draws <- num_saved_draws +
      ceiling(as.double(iter_warmup) / as.double(thin))
  }
  if (num_saved_draws > .Machine$integer.max) {
    stop("Requested number of saved draws is too large.", call. = FALSE)
  }
  if (!fixed_param && engine == "static" && chains > 1L) {
    stop(
      "Static HMC only supports a single chain. Set `chains` = 1.",
      call. = FALSE
    )
  }
  inv_metric <- .stanr_normalize_inv_metric(
    inv_metric = inv_metric,
    metric = metric,
    chains = chains
  )
  max_treedepth <- .stanr_int(max_treedepth, "max_treedepth")
  init_buffer <- .stanr_int(init_buffer, "init_buffer")
  term_buffer <- .stanr_int(term_buffer, "term_buffer")
  window <- .stanr_int(window, "window")
  max_step_halvings <- .stanr_int(max_step_halvings, "max_step_halvings")
  min_micro_steps <- .stanr_int(min_micro_steps, "min_micro_steps")

  diagnostic_vars <- if (engine == "walnuts") {
    character()
  } else if (!fixed_param && engine == "static") {
    c("accept_stat__", "stepsize__", "int_time__")
  } else {
    c(
      "accept_stat__",
      "stepsize__",
      "treedepth__",
      "n_leapfrog__",
      "divergent__",
      "energy__"
    )
  }

  native_args_fn <- function(seed, resolved_init, model) {
    native <- list(
      method = "sample",
      algorithm = if (fixed_param) "fixed_param" else "hmc",
      engine = if (fixed_param) "nuts" else engine,
      metric = metric,
      adapt_engaged = adapt_engaged,
      seed = seed,
      id = chain_ids[[1]],
      num_chains = chains,
      init_radius = resolved_init$radius,
      num_warmup = iter_warmup,
      num_samples = iter_sampling,
      thin = thin,
      save_warmup = save_warmup,
      refresh = refresh,
      stepsize = as.double(step_size),
      stepsize_jitter = as.double(step_size_jitter),
      max_depth = max_treedepth,
      int_time = as.double(int_time),
      delta = as.double(adapt_delta),
      gamma = as.double(adapt_gamma),
      kappa = as.double(adapt_kappa),
      t0 = as.double(adapt_t0),
      init_buffer = init_buffer,
      term_buffer = term_buffer,
      window = window,
      init = resolved_init$values,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      diagnostic_names = diagnostic_vars,
      max_step_halvings = max_step_halvings,
      min_micro_steps = min_micro_steps,
      max_hamiltonian_error = as.double(max_hamiltonian_error),
      mass_init_count = as.double(mass_init_count),
      mass_additive_smoothing = as.double(mass_additive_smoothing),
      max_macro_steps_target = as.double(max_macro_steps_target),
      step_learning_rate = as.double(step_learning_rate),
      step_gradient_decay = as.double(step_gradient_decay),
      step_sq_gradient_decay = as.double(step_sq_gradient_decay),
      step_stabilization = as.double(step_stabilization),
      step_learn_rate_decay = as.double(step_learn_rate_decay)
    )
    if (!is.null(inv_metric)) {
      native$inv_metric <- inv_metric
    }
    native
  }

  payload_fn <- function(result) {
    if (result$return_code != 0) {
      list(draws = NULL, diagnostics = NULL)
    } else {
      draws <- result$samples
      # walnuts collects no sampler diagnostics; keep checks honestly silent.
      diagnostics <- if (
        engine == "walnuts" || dim(result$diagnostics)[3] > 0
      ) {
        result$diagnostics
      } else {
        posterior::draws_df(
          "stepsize__" = NA,
          "treedepth__" = NA,
          "n_leapfrog__" = NA,
          "divergent__" = NA,
          "energy__" = NA
        )
      }

      list(
        draws = draws,
        diagnostics = diagnostics,
        warmup_draws = if (!is.null(result$warmup_samples)) {
          result$warmup_samples
        },
        warmup_diagnostics = if (!is.null(result$warmup_diagnostics)) {
          result$warmup_diagnostics
        },
        inv_metric = result$inv_metric,
        step_size = result$step_size
      )
    }
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanMCMC$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "sample",
      chains = ids$chains,
      chain_ids = ids$chain_ids,
      num_threads = num_threads,
      show_exceptions = show_exceptions,
      save_warmup = save_warmup,
      fixed_param = fixed_param,
      diagnostics = diagnostics
    )
  )
}
