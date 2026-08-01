`%||%` <- function(x, y) if (is.null(x)) y else x

.newstan_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

.newstan_seed <- function(seed) {
  if (is.null(seed)) {
    seed <- as.integer(stats::runif(1, 1, .Machine$integer.max))
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) {
    stop("`seed` must be NULL or a single integer between 0 and 2^31 - 1.",
         call. = FALSE)
  }
  as.integer(seed)
}

.newstan_reject_backend_files <- function(output_dir = NULL,
                                           output_basename = NULL,
                                           sig_figs = NULL,
                                           opencl_ids = NULL,
                                           save_cmdstan_config = FALSE) {
  unsupported <- c(
    if (!is.null(output_dir)) "output_dir",
    if (!is.null(output_basename)) "output_basename",
    if (!is.null(sig_figs)) "sig_figs",
    if (!is.null(opencl_ids)) "opencl_ids",
    if (isTRUE(save_cmdstan_config)) "save_cmdstan_config"
  )
  if (length(unsupported)) {
    stop(
      "The in-process backend does not yet support: ",
      paste(unsupported, collapse = ", "), ".",
      call. = FALSE
    )
  }
}

.newstan_elapsed <- function(expr) {
  started <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(value = value, elapsed = unname(proc.time()[["elapsed"]] - started))
}

.newstan_stan_version <- function() {
  header <- system.file("include", "stan", "version.hpp", package = "newstan")
  if (!nzchar(header) || !file.exists(header)) {
    return(NA_character_)
  }
  lines <- readLines(header, warn = FALSE)
  value <- function(macro) {
    line <- grep(paste0("^#define[[:space:]]+", macro, "[[:space:]]+"),
                 lines, value = TRUE)
    if (!length(line)) return(NA_character_)
    sub(paste0("^#define[[:space:]]+", macro, "[[:space:]]+"), "", line[[1]])
  }
  paste(value("STAN_MAJOR"), value("STAN_MINOR"), value("STAN_PATCH"), sep = ".")
}

#' An in-process compiled Stan model
#'
#' `StanModel` owns source and compilation metadata and exposes Stan services as
#' methods. Construct models with [stan_model()] rather than calling `$new()`.
#'
#' @noRd
StanModel <- R6Class(
  "StanModel",
  public = list(
    initialize = function(stan_file = NULL, code = NULL, compile = TRUE,
                          model_name = NULL, include_paths = NULL,
                          user_header = NULL, cpp_options = list(),
                          stanc_options = list(), force_recompile = FALSE,
                          precompiled_headers = FALSE, quiet = TRUE,
                          external_cpp = NULL) {
      compile <- .newstan_flag(compile, "compile")
      force_recompile <- .newstan_flag(force_recompile, "force_recompile")
      precompiled_headers <- .newstan_flag(
        precompiled_headers, "precompiled_headers"
      )
      quiet <- .newstan_flag(quiet, "quiet")
      if (is.null(stan_file) == is.null(code)) {
        stop("Supply exactly one of `stan_file` and `code`.", call. = FALSE)
      }
      if (!is.null(stan_file)) {
        if (!is.character(stan_file) || length(stan_file) != 1L ||
            is.na(stan_file) || !file.exists(stan_file)) {
          stop("`stan_file` must name an existing Stan file.", call. = FALSE)
        }
        stan_file <- normalizePath(stan_file, mustWork = TRUE)
        code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")
      }
      if (!is.character(code) || length(code) != 1L || is.na(code)) {
        stop("`code` must be a single non-missing string.", call. = FALSE)
      }
      if (is.null(model_name)) {
        model_name <- if (is.null(stan_file)) "model" else
          sub("\\.stan$", "", basename(stan_file))
      }
      if (!is.character(model_name) || length(model_name) != 1L ||
          is.na(model_name) || !nzchar(model_name)) {
        stop("`model_name` must be a single non-empty string.", call. = FALSE)
      }
      include_paths <- include_paths %||% character()
      if (!is.character(include_paths) || anyNA(include_paths)) {
        stop("`include_paths` must be a character vector.", call. = FALSE)
      }
      if (length(include_paths)) {
        include_paths <- normalizePath(include_paths, mustWork = TRUE)
      }
      if (!is.list(cpp_options) || !is.list(stanc_options)) {
        stop("`cpp_options` and `stanc_options` must be lists.", call. = FALSE)
      }
      if (length(cpp_options)) {
        stop("Non-empty `cpp_options` are not yet supported by the in-process backend.",
             call. = FALSE)
      }
      if (length(stanc_options)) {
        stop("Non-empty `stanc_options` are not yet supported by `stan_model()`.",
             call. = FALSE)
      }
      if (!is.null(user_header)) {
        stop("`user_header` is not yet supported; use `external_cpp`.",
             call. = FALSE)
      }

      private$stan_file_ <- stan_file
      private$code_ <- code
      private$model_name_ <- model_name
      private$include_paths_ <- include_paths
      private$user_header_ <- user_header
      private$cpp_options_ <- cpp_options
      private$stanc_options_ <- stanc_options
      private$force_recompile_ <- force_recompile
      private$precompiled_headers_ <- precompiled_headers
      private$quiet_ <- quiet
      private$external_cpp_ <- external_cpp
      if (compile) self$compile()
      invisible(self)
    },

    compile = function(force_recompile = private$force_recompile_,
                       quiet = private$quiet_) {
      private$compiled_env_ <- .compile_stan_model_environment(
        file = private$stan_file_,
        code = if (is.null(private$stan_file_)) private$code_ else NULL,
        model_name = private$model_name_,
        include_directories = private$include_paths_,
        external_cpp = private$external_cpp_,
        verbose = !isTRUE(quiet),
        precompiled_headers = private$precompiled_headers_,
        force_recompile = isTRUE(force_recompile)
      )
      invisible(self)
    },

    code = function() private$code_,
    print = function(...) {
      cat(private$code_, "\n", sep = "")
      invisible(self)
    },
    model_name = function() private$model_name_,
    stan_file = function() private$stan_file_ %||% character(),
    has_stan_file = function() !is.null(private$stan_file_),
    include_paths = function() private$include_paths_,
    stan_version = function() .newstan_stan_version(),
    variables = function() {
      list(
        data = NULL,
        parameters = NULL,
        transformed_parameters = NULL,
        generated_quantities = NULL
      )
    },
    is_compiled = function() !is.null(private$compiled_env_),
    cpp_options = function() private$cpp_options_,
    stanc_options = function() private$stanc_options_,

    # Internal native entry points remain public because sourceCpp functions
    # live in a model-specific environment. They are not the user-facing API.
    new_model = function(data, seed) {
      private$ensure_compiled()
      private$compiled_env_$new_model(data, seed)
    },
    run_model = function(model, args) {
      private$ensure_compiled()
      private$compiled_env_$run_model(model, args)
    },
    constrained_param_names = function(model) {
      private$ensure_compiled()
      private$compiled_env_$constrained_param_names(model)
    },
    native_function = function(name, required = TRUE) {
      private$ensure_compiled()
      fun <- private$compiled_env_[[name]]
      if (is.null(fun) && required) {
        stop("The compiled model does not provide native function `", name,
             "`; recompile the model with the current newstan version.",
             call. = FALSE)
      }
      fun
    },

    sample = function(
      data = NULL, seed = NULL, refresh = NULL, init = NULL,
      save_latent_dynamics = FALSE,
      output_dir = getOption("newstan_output_dir"), output_basename = NULL,
      sig_figs = NULL, chains = 4,
      parallel_chains = getOption("mc.cores", 1),
      chain_ids = seq_len(chains), threads_per_chain = NULL,
      opencl_ids = NULL, iter_warmup = NULL, iter_sampling = NULL,
      save_warmup = FALSE, thin = NULL, max_treedepth = NULL,
      adapt_engaged = TRUE, adapt_delta = NULL, step_size = NULL,
      metric = NULL, metric_file = NULL, inv_metric = NULL,
      init_buffer = NULL, term_buffer = NULL, window = NULL,
      fixed_param = FALSE, show_messages = TRUE, show_exceptions = TRUE,
      diagnostics = c("divergences", "treedepth", "ebfmi"),
      save_metric = getOption("newstan_save_metric", FALSE),
      save_cmdstan_config = getOption("newstan_save_config", FALSE),
      engine = "nuts", int_time = 2 * pi, step_size_jitter = 0,
      adapt_gamma = 0.05, adapt_kappa = 0.75, adapt_t0 = 10
    ) {
      .newstan_reject_backend_files(output_dir, output_basename, sig_figs,
                                    opencl_ids, save_cmdstan_config)
      if (isTRUE(save_latent_dynamics)) {
        stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
      }
      if (!is.null(metric_file)) {
        stop("`metric_file` is not yet supported; supply `inv_metric` in memory.",
             call. = FALSE)
      }
      args <- .newstan_normalize_sample(
        data = data, seed = seed, refresh = refresh, init = init,
        chains = chains, chain_ids = chain_ids,
        parallel_chains = parallel_chains, threads_per_chain = threads_per_chain,
        iter_warmup = iter_warmup, iter_sampling = iter_sampling,
        save_warmup = save_warmup, thin = thin, max_treedepth = max_treedepth,
        adapt_engaged = adapt_engaged, adapt_delta = adapt_delta,
        step_size = step_size, metric = metric, inv_metric = inv_metric,
        init_buffer = init_buffer, term_buffer = term_buffer, window = window,
        fixed_param = fixed_param, show_messages = show_messages,
        show_exceptions = show_exceptions,
        engine = engine, int_time = int_time,
        step_size_jitter = step_size_jitter,
        adapt_gamma = adapt_gamma, adapt_kappa = adapt_kappa, adapt_t0 = adapt_t0
      )
      call <- .newstan_elapsed(.newstan_run_sampling(self, args))
      StanMCMC$new(
        payload = call$value, model = self, data = args$data,
        seed = args$seed, init = args$init, elapsed = call$elapsed,
        metadata = list(
          method = "sample", chains = args$chains,
          chain_ids = as.integer(args$chain_ids),
          parallel_chains = args$parallel_chains,
          threads_per_chain = args$threads_per_chain,
          diagnostics = diagnostics,
          save_metric = isTRUE(save_metric),
          show_exceptions = args$show_exceptions,
          save_warmup = args$save_warmup
        )
      )
    },

    optimize = function(
      data = NULL, seed = NULL, refresh = NULL, init = NULL,
      output_dir = getOption("newstan_output_dir"), output_basename = NULL,
      sig_figs = NULL, threads = NULL, opencl_ids = NULL,
      algorithm = NULL, jacobian = FALSE, init_alpha = NULL, iter = NULL,
      tol_obj = NULL, tol_rel_obj = NULL, tol_grad = NULL,
      tol_rel_grad = NULL, tol_param = NULL, history_size = NULL,
      show_messages = TRUE, show_exceptions = TRUE,
      save_cmdstan_config = getOption("newstan_save_config", FALSE),
      save_iterations = FALSE
    ) {
      .newstan_reject_backend_files(output_dir, output_basename, sig_figs,
                                    opencl_ids, save_cmdstan_config)
      if (isTRUE(jacobian)) {
        stop("Jacobian-adjusted optimization is not yet supported by the native service.",
             call. = FALSE)
      }
      args <- .newstan_normalize_optimize(
        data = data, seed = seed, refresh = refresh, init = init,
        threads = threads, algorithm = algorithm, jacobian = jacobian,
        init_alpha = init_alpha, iter = iter,
        tol_obj = tol_obj, tol_rel_obj = tol_rel_obj,
        tol_grad = tol_grad, tol_rel_grad = tol_rel_grad,
        tol_param = tol_param, history_size = history_size,
        show_messages = show_messages, show_exceptions = show_exceptions
      )
      call <- .newstan_elapsed(.newstan_run_optimize(self, args))
      StanMLE$new(
        payload = call$value, model = self, data = args$data,
        seed = args$seed, init = args$init, elapsed = call$elapsed,
        metadata = list(method = "optimize", jacobian = args$jacobian,
                        threads = args$threads,
                        show_exceptions = args$show_exceptions)
      )
    },

    laplace = function(
      data = NULL, seed = NULL, refresh = NULL, init = NULL,
      output_dir = getOption("newstan_output_dir"), output_basename = NULL,
      sig_figs = NULL, threads = NULL, opencl_ids = NULL,
      mode = NULL, opt_args = NULL, jacobian = TRUE, draws = NULL,
      show_messages = TRUE, show_exceptions = TRUE,
      save_cmdstan_config = getOption("newstan_save_config", FALSE),
      calculate_lp = TRUE
    ) {
      .newstan_reject_backend_files(output_dir, output_basename, sig_figs,
                                    opencl_ids, save_cmdstan_config)
      args <- .newstan_normalize_laplace(
        data = data, seed = seed, refresh = refresh, init = init,
        threads = threads, mode = mode, opt_args = opt_args,
        jacobian = jacobian, draws = draws, calculate_lp = calculate_lp,
        show_messages = show_messages, show_exceptions = show_exceptions
      )
      mode_fit <- NULL
      if (is.null(args$mode)) {
        mode_fit <- do.call(
          self$optimize,
          c(list(data = args$data, seed = args$seed, init = args$init,
                 jacobian = args$jacobian,
                 show_messages = args$show_messages,
                 show_exceptions = args$show_exceptions),
            args$opt_args)
        )
        mode_val <- mode_fit$mle()
      } else if (inherits(args$mode, "StanMLE")) {
        mode_fit <- args$mode
        mode_val <- mode_fit$mle()
      } else {
        # Numeric mode vector (newstan extension)
        mode_val <- args$mode
      }
      call <- .newstan_elapsed(.newstan_run_laplace(self, args, mode_val))
      StanLaplace$new(
        payload = call$value, model = self, data = args$data,
        seed = args$seed, init = args$init, elapsed = call$elapsed,
        mode = mode_fit %||% args$mode,
        metadata = list(method = "laplace", jacobian = args$jacobian,
                        threads = args$threads,
                        show_exceptions = args$show_exceptions)
      )
    },

    variational = function(
      data = NULL, seed = NULL, refresh = NULL, init = NULL,
      save_latent_dynamics = FALSE,
      output_dir = getOption("newstan_output_dir"), output_basename = NULL,
      sig_figs = NULL, threads = NULL, opencl_ids = NULL,
      algorithm = NULL, iter = NULL, grad_samples = NULL,
      elbo_samples = NULL, eta = NULL, adapt_engaged = NULL,
      adapt_iter = NULL, tol_rel_obj = NULL, eval_elbo = NULL,
      draws = NULL, show_messages = TRUE, show_exceptions = TRUE,
      save_cmdstan_config = getOption("newstan_save_config", FALSE)
    ) {
      .newstan_reject_backend_files(output_dir, output_basename, sig_figs,
                                    opencl_ids, save_cmdstan_config)
      if (isTRUE(save_latent_dynamics)) {
        stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
      }
      args <- .newstan_normalize_variational(
        data = data, seed = seed, refresh = refresh, init = init,
        threads = threads, algorithm = algorithm, iter = iter,
        grad_samples = grad_samples, elbo_samples = elbo_samples,
        tol_rel_obj = tol_rel_obj, eta = eta,
        adapt_engaged = adapt_engaged, adapt_iter = adapt_iter,
        eval_elbo = eval_elbo, draws = draws,
        show_messages = show_messages, show_exceptions = show_exceptions
      )
      call <- .newstan_elapsed(.newstan_run_variational(self, args))
      StanVB$new(
        payload = call$value, model = self, data = args$data,
        seed = args$seed, init = args$init, elapsed = call$elapsed,
        metadata = list(method = "variational", threads = args$threads,
                        show_exceptions = args$show_exceptions)
      )
    },

    pathfinder = function(
      data = NULL, seed = NULL, refresh = NULL, init = NULL,
      output_dir = getOption("newstan_output_dir"), output_basename = NULL,
      sig_figs = NULL, threads = NULL, opencl_ids = NULL,
      init_alpha = NULL, tol_obj = NULL, tol_rel_obj = NULL,
      tol_grad = NULL, tol_rel_grad = NULL, tol_param = NULL,
      history_size = NULL, single_path_draws = NULL, draws = NULL,
      num_paths = 4, max_lbfgs_iters = NULL, num_elbo_draws = NULL,
      save_single_paths = NULL, psis_resample = NULL, calculate_lp = NULL,
      show_messages = TRUE, show_exceptions = TRUE,
      save_cmdstan_config = getOption("newstan_save_config", FALSE)
    ) {
      .newstan_reject_backend_files(output_dir, output_basename, sig_figs,
                                    opencl_ids, save_cmdstan_config)
      args <- .newstan_normalize_pathfinder(
        data = data, seed = seed, refresh = refresh, init = init,
        threads = threads, init_alpha = init_alpha,
        tol_obj = tol_obj, tol_rel_obj = tol_rel_obj,
        tol_grad = tol_grad, tol_rel_grad = tol_rel_grad,
        tol_param = tol_param, history_size = history_size,
        single_path_draws = single_path_draws, draws = draws,
        num_paths = num_paths, max_lbfgs_iters = max_lbfgs_iters,
        num_elbo_draws = num_elbo_draws,
        save_single_paths = save_single_paths,
        psis_resample = psis_resample, calculate_lp = calculate_lp,
        show_messages = show_messages, show_exceptions = show_exceptions
      )
      call <- .newstan_elapsed(.newstan_run_pathfinder(self, args))
      StanPathfinder$new(
        payload = call$value, model = self, data = args$data,
        seed = args$seed, init = args$init, elapsed = call$elapsed,
        metadata = list(method = "pathfinder", num_paths = args$num_paths,
                        threads = args$threads,
                        show_exceptions = args$show_exceptions)
      )
    },

    generate_quantities = function(
      fitted_params, data = NULL, seed = NULL,
      output_dir = getOption("newstan_output_dir"), output_basename = NULL,
      sig_figs = NULL, parallel_chains = getOption("mc.cores", 1),
      threads_per_chain = NULL, opencl_ids = NULL,
      show_messages = TRUE, show_exceptions = TRUE
    ) {
      .newstan_reject_backend_files(output_dir, output_basename, sig_figs,
                                    opencl_ids, FALSE)
      common <- .newstan_normalize_common(data = data, seed = seed)
      parallel_chains <- as.integer(parallel_chains %||% 1L)
      threads_per_chain <- as.integer(threads_per_chain %||% 1L)
      input <- if (inherits(fitted_params, "StanFit")) {
        fitted_params$draws(format = "draws_matrix")
      } else {
        fitted_params
      }
      call <- .newstan_elapsed(.newstan_run_generate_quantities(
        self, common, input, parallel_chains, threads_per_chain
      ))
      StanGQ$new(
        payload = call$value, model = self, data = common$data,
        seed = common$seed, init = NULL, elapsed = call$elapsed,
        metadata = list(method = "generate_quantities",
                        parallel_chains = parallel_chains,
                        threads_per_chain = threads_per_chain,
                        show_exceptions = common$show_exceptions)
      )
    },

    diagnose = function(
      data = NULL, seed = NULL, init = NULL,
      output_dir = getOption("newstan_output_dir"), output_basename = NULL,
      epsilon = NULL, error = NULL
    ) {
      .newstan_reject_backend_files(output_dir, output_basename)
      args <- .newstan_normalize_diagnose(
        data = data, seed = seed, init = init,
        epsilon = epsilon, error = error
      )
      call <- .newstan_elapsed(.newstan_run_diagnose(self, args))
      StanDiagnose$new(
        payload = call$value, model = self, data = args$data,
        seed = args$seed, init = args$init, elapsed = call$elapsed,
        metadata = list(method = "diagnose", epsilon = args$epsilon,
                        error = args$error)
      )
    }
  ),
  private = list(
    stan_file_ = NULL,
    code_ = NULL,
    model_name_ = NULL,
    include_paths_ = NULL,
    user_header_ = NULL,
    cpp_options_ = NULL,
    stanc_options_ = NULL,
    force_recompile_ = FALSE,
    precompiled_headers_ = FALSE,
    quiet_ = TRUE,
    external_cpp_ = NULL,
    compiled_env_ = NULL,
    ensure_compiled = function() {
      if (is.null(private$compiled_env_)) self$compile()
      invisible(NULL)
    }
  ),
  cloneable = FALSE
)
