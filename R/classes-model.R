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
  if (
    !is.numeric(seed) ||
      length(seed) != 1L ||
      is.na(seed) ||
      seed < 0 ||
      seed > .Machine$integer.max ||
      seed != floor(seed)
  ) {
    stop(
      "`seed` must be NULL or a single integer between 0 and 2^31 - 1.",
      call. = FALSE
    )
  }
  as.integer(seed)
}

.newstan_reject_backend_files <- function(
  output_dir = NULL,
  output_basename = NULL,
  sig_figs = NULL,
  opencl_ids = NULL,
  save_cmdstan_config = FALSE
) {
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
      paste(unsupported, collapse = ", "),
      ".",
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
    line <- grep(
      paste0("^#define[[:space:]]+", macro, "[[:space:]]+"),
      lines,
      value = TRUE
    )
    if (!length(line)) {
      return(NA_character_)
    }
    sub(paste0("^#define[[:space:]]+", macro, "[[:space:]]+"), "", line[[1]])
  }
  paste(
    value("STAN_MAJOR"),
    value("STAN_MINOR"),
    value("STAN_PATCH"),
    sep = "."
  )
}

# StanModel class definition ---------------------------------------------------

#' StanModel objects
#'
#' @name StanModel
#' @description A `StanModel` object is an [R6][R6::R6Class] object created by
#'   [stan_model()]. The object stores the Stan program source code and compiled
#'   model environment, and provides methods for fitting the model using Stan's
#'   inference algorithms.
#'
#' @section Methods: `StanModel` objects have the following associated methods,
#'   many of which have their own (linked) documentation pages:
#'
#'  ## Stan code
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$code()`][model-method-model-info] | Return Stan program as a string. |
#'  [`$print()`][model-method-model-info] | Print the Stan program. |
#'
#'  ## Model information
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$model_name()`][model-method-model-info] | Return the model name. |
#'  [`$stan_file()`][model-method-model-info] | Return the path to the Stan file. |
#'  [`$has_stan_file()`][model-method-model-info] | Check whether the model was created with a Stan file. |
#'  [`$include_paths()`][model-method-model-info] | Return the Stan include paths. |
#'  [`$stan_version()`][model-method-model-info] | Return the Stan version used by the package. |
#'  [`$variables()`][model-method-variables] | Return input and output variable information. |
#'  [`$cpp_options()`][model-method-model-info] | Return the C++ options associated with the model. |
#'  [`$stanc_options()`][model-method-model-info] | Return the stanc options associated with the model. |
#'
#'  ## Compilation
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$compile()`][model-method-compile] | Compile the Stan program. |
#'  [`$is_compiled()`][model-method-model-info] | Check whether the model has been compiled. |
#'
#'  ## Diagnostics
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$diagnose()`][model-method-diagnose] | Run Stan's `"diagnose"` method to test gradients, return [`StanDiagnose`] object. |
#'
#'  ## Model fitting
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$sample()`][model-method-sample] | Run Stan's `"sample"` method (HMC/NUTS MCMC), return [`StanMCMC`] object. |
#'  [`$optimize()`][model-method-optimize] | Run Stan's `"optimize"` method, return [`StanMLE`] object. |
#'  [`$laplace()`][model-method-laplace] | Run Stan's `"laplace"` method, return [`StanLaplace`] object. |
#'  [`$variational()`][model-method-variational] | Run Stan's `"variational"` method (ADVI), return [`StanVB`] object. |
#'  [`$pathfinder()`][model-method-pathfinder] | Run Stan's `"pathfinder"` method, return [`StanPathfinder`] object. |
#'  [`$generate_quantities()`][model-method-generate-quantities] | Run Stan's `"generate quantities"` method, return [`StanGQ`] object. |
#'
NULL

StanModel <- R6Class(
  "StanModel",
  public = list(
    initialize = function(
      stan_file = NULL,
      code = NULL,
      compile = TRUE,
      model_name = NULL,
      include_paths = NULL,
      user_header = NULL,
      cpp_options = list(),
      stanc_options = list(),
      force_recompile = FALSE,
      precompiled_headers = TRUE,
      quiet = TRUE,
      external_cpp = NULL
    ) {
      compile <- .newstan_flag(compile, "compile")
      force_recompile <- .newstan_flag(force_recompile, "force_recompile")
      precompiled_headers <- .newstan_flag(
        precompiled_headers,
        "precompiled_headers"
      )
      quiet <- .newstan_flag(quiet, "quiet")
      if (is.null(stan_file) == is.null(code)) {
        stop("Supply exactly one of `stan_file` and `code`.", call. = FALSE)
      }
      if (!is.null(stan_file)) {
        if (
          !is.character(stan_file) ||
            length(stan_file) != 1L ||
            is.na(stan_file) ||
            !file.exists(stan_file)
        ) {
          stop("`stan_file` must name an existing Stan file.", call. = FALSE)
        }
        stan_file <- normalizePath(stan_file, mustWork = TRUE)
        code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")
      }
      if (!is.character(code) || length(code) != 1L || is.na(code)) {
        stop("`code` must be a single non-missing string.", call. = FALSE)
      }
      if (is.null(model_name)) {
        model_name <- if (is.null(stan_file)) {
          "model"
        } else {
          sub("\\.stan$", "", basename(stan_file))
        }
      }
      if (
        !is.character(model_name) ||
          length(model_name) != 1L ||
          is.na(model_name) ||
          !nzchar(model_name)
      ) {
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
        stop(
          "Non-empty `cpp_options` are not yet supported by the in-process backend.",
          call. = FALSE
        )
      }
      if (length(stanc_options)) {
        stop(
          "Non-empty `stanc_options` are not yet supported by `stan_model()`.",
          call. = FALSE
        )
      }
      if (!is.null(user_header)) {
        stop(
          "`user_header` is not yet supported; use `external_cpp`.",
          call. = FALSE
        )
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
      if (compile) {
        self$compile()
      }
      invisible(self)
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
    precompiled_headers_ = TRUE,
    quiet_ = TRUE,
    external_cpp_ = NULL,
    compiled_env_ = NULL,
    variables_ = NULL,
    ensure_compiled = function() {
      if (is.null(private$compiled_env_)) {
        self$compile()
      }
      invisible(NULL)
    }
  ),
  cloneable = FALSE
)

# StanModel information methods ------------------------------------------------

#' Access information from a `StanModel` object
#'
#' @name model-method-model-info
#' @family StanModel methods
#'
#' @description These methods access information stored in a [`StanModel`]
#'   object and print its Stan program.
#'
#'   ```
#'   stan_file()
#'   has_stan_file()
#'   code()
#'   print()
#'   model_name()
#'   include_paths()
#'   stan_version()
#'   is_compiled()
#'   cpp_options()
#'   stanc_options()
#'   ```
#'
#' @return
#' * `$stan_file()` returns a path as a string, or `character()` if the model
#'   was created from code (not a file).
#' * `$has_stan_file()` returns `TRUE` if the model was created with a Stan file
#'   and `FALSE` otherwise.
#' * `$code()` returns the Stan program as a single string.
#' * `$print()` prints the Stan program and returns the [`StanModel`] object
#'   invisibly.
#' * `$model_name()` returns the model name as a string.
#' * `$include_paths()` returns a character vector of absolute paths.
#' * `$stan_version()` returns the Stan version bundled with the package as a
#'   string.
#' * `$is_compiled()` returns `TRUE` if the model has been compiled.
#' * `$cpp_options()` returns a named list of C++ options.
#' * `$stanc_options()` returns a named list of stanc options.
#'
#' @seealso [`$compile()`][model-method-compile] and [stan_model()]
#'
NULL

stan_model_stan_file <- function() private$stan_file_ %||% character()
StanModel$set("public", "stan_file", stan_model_stan_file)

stan_model_has_stan_file <- function() !is.null(private$stan_file_)
StanModel$set("public", "has_stan_file", stan_model_has_stan_file)

stan_model_code <- function() private$code_
StanModel$set("public", "code", stan_model_code)

stan_model_print <- function() {
  cat(private$code_, "\n", sep = "")
  invisible(self)
}
StanModel$set("public", "print", stan_model_print)

stan_model_model_name <- function() private$model_name_
StanModel$set("public", "model_name", stan_model_model_name)

stan_model_include_paths <- function() private$include_paths_
StanModel$set("public", "include_paths", stan_model_include_paths)

stan_model_stan_version <- function() .newstan_stan_version()
StanModel$set("public", "stan_version", stan_model_stan_version)

stan_model_is_compiled <- function() !is.null(private$compiled_env_)
StanModel$set("public", "is_compiled", stan_model_is_compiled)

stan_model_cpp_options <- function() private$cpp_options_
StanModel$set("public", "cpp_options", stan_model_cpp_options)

stan_model_stanc_options <- function() private$stanc_options_
StanModel$set("public", "stanc_options", stan_model_stanc_options)

#' Input and output variables of a Stan program
#'
#' @name model-method-variables
#' @aliases variables
#' @family StanModel methods
#'
#' @description The `$variables()` method of a [`StanModel`] object returns
#'   a list, each element representing a Stan model block: `data`, `parameters`,
#'   `transformed_parameters` and `generated_quantities`.
#'
#'   Each element contains a list of variables, with each variable represented
#'   as a list with information on its scalar type (`real` or `int`) and
#'   number of dimensions.
#'
#'   The number of dimensions reported is the number of indexing dimensions in
#'   the declared Stan variable, equivalently the number of indices needed to
#'   access one scalar element. This means a scalar has 0 dimensions, a vector
#'   or one-dimensional array has 1, and a matrix or two-dimensional array has
#'   2. Array dimensions are added to any vector or matrix dimensions, so
#'   `array[J] matrix[N, K]` has 3 dimensions.
#'
#'   `transformed data` is not included, as variables in that block are not
#'   part of the model's input or output.
#'
#' @return A list with information on input and output variables for each of
#'   the Stan model blocks.
#'
#' @examples
#' \dontrun{
#' mod <- stan_model(
#'   code = "
#'   data {
#'     int N;
#'     array[2, 3] int y;
#'   }
#'   parameters {
#'     real alpha;
#'     vector[N] beta;
#'     array[2] matrix[3, 4] theta;
#'   }
#'   ",
#'   compile = FALSE
#' )
#'
#' vars <- mod$variables()
#' str(vars)
#' }
#'
NULL

stan_model_variables <- function() {
  if (is.null(private$variables_)) {
    private$variables_ <- model_variables(
      model_code = private$code_,
      include_directories = private$include_paths_,
      allow_undefined = length(private$external_cpp_) > 0
    )
  }
  private$variables_
}
StanModel$set("public", "variables", stan_model_variables)

# StanModel compilation methods ------------------------------------------------

#' Compile a Stan program
#'
#' @name model-method-compile
#' @aliases compile
#' @family StanModel methods
#'
#' @description The `$compile()` method of a [`StanModel`] object compiles the
#'   Stan program using the in-process backend. In most cases the user does not
#'   need to explicitly call `$compile()` as compilation occurs automatically
#'   when calling [stan_model()]. However it is possible to set `compile = FALSE`
#'   in the call to `stan_model()` and subsequently call `$compile()` directly.
#'
#' @param force_recompile (logical) Should the model be recompiled even if it
#'   has not been modified since it was last compiled? The default is `FALSE`.
#' @param quiet (logical) Should verbose output from compilation be suppressed?
#'   The default is `TRUE`.
#'
#' @return The [`StanModel`] object, invisibly.
#'
#' @seealso [`$is_compiled()`][model-method-model-info] and [stan_model()]
#'
NULL

stan_model_compile <- function(
  force_recompile = private$force_recompile_,
  quiet = private$quiet_
) {
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
}
StanModel$set("public", "compile", stan_model_compile)

# StanModel fitting methods ----------------------------------------------------

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
#' @param data (named list) Values for all or part of the data `block` in the
#'   Stan program.
#' @param seed (integer) The random seed for reproducibility.
#' @param refresh (integer) How often (in iterations) to print progress.
#' @param init Initial values for parameters. Can be a list (single chain),
#'   a list of lists (one per chain), or a function that takes an optional
#'   `chain_id` argument and returns a list.
#' @param chains (integer) The number of MCMC chains.
#' @param parallel_chains (integer) The number of chains to run in parallel.
#' @param chain_ids (integer vector) The IDs for each chain.
#' @param threads_per_chain (integer) The number of threads per chain.
#' @param iter_warmup (integer) The number of warmup iterations.
#' @param iter_sampling (integer) The number of sampling iterations.
#' @param save_warmup (logical) Should warmup samples be saved?
#' @param thin (integer) Thin interval.
#' @param max_treedepth (integer) Maximum tree depth for NUTS.
#' @param adapt_engaged (logical) Should step size adaptation be used?
#' @param adapt_delta (number) Target acceptance statistic for HMC.
#' @param step_size (number) Step size for HMC.
#' @param metric Character string indicating the metric to use: `"diag_e"` or
#'   `"dense_e"`.
#' @param inv_metric (numeric) Initial inverse mass matrix values.
#' @param init_buffer (integer) Adaptation phase: initial buffer length.
#' @param term_buffer (integer) Adaptation phase: terminal buffer length.
#' @param window (integer) Adaptation phase: window length.
#' @param fixed_param (logical) Treat all parameters as fixed (no adaptation).
#' @param show_messages (logical) Should Stan messages be shown?
#' @param show_exceptions (logical) Should Stan exceptions be shown?
#' @param diagnostics (character vector) Which diagnostics to compute.
#' @param engine (string) The sampling engine: `"nuts"`, `"static_hmc"`, or
#'   `"fixed_param"`.
#' @param int_time (number) Integration time for static HMC.
#' @param step_size_jitter (number) Jitter for step size after adaptation.
#' @param adapt_gamma (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_kappa (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_t0 (number) Adaptation hyperparameter for dual averaging.
#'
#' @return A [`StanMCMC`] object containing posterior draws and diagnostics.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_sample <- function(
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  save_latent_dynamics = FALSE,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  chains = 4,
  parallel_chains = getOption("mc.cores", 1),
  chain_ids = seq_len(chains),
  threads_per_chain = NULL,
  opencl_ids = NULL,
  iter_warmup = NULL,
  iter_sampling = NULL,
  save_warmup = FALSE,
  thin = NULL,
  max_treedepth = NULL,
  adapt_engaged = TRUE,
  adapt_delta = NULL,
  step_size = NULL,
  metric = NULL,
  metric_file = NULL,
  inv_metric = NULL,
  init_buffer = NULL,
  term_buffer = NULL,
  window = NULL,
  fixed_param = FALSE,
  show_messages = TRUE,
  show_exceptions = TRUE,
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  save_metric = getOption("newstan_save_metric", FALSE),
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  engine = "nuts",
  int_time = 2 * pi,
  step_size_jitter = 0,
  adapt_gamma = 0.05,
  adapt_kappa = 0.75,
  adapt_t0 = 10
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    opencl_ids,
    save_cmdstan_config
  )
  if (isTRUE(save_latent_dynamics)) {
    stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
  }
  if (!is.null(metric_file)) {
    stop(
      "`metric_file` is not yet supported; supply `inv_metric` in memory.",
      call. = FALSE
    )
  }
  args <- .newstan_normalize_sample(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    chains = chains,
    chain_ids = chain_ids,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    save_warmup = save_warmup,
    thin = thin,
    max_treedepth = max_treedepth,
    adapt_engaged = adapt_engaged,
    adapt_delta = adapt_delta,
    step_size = step_size,
    metric = metric,
    inv_metric = inv_metric,
    init_buffer = init_buffer,
    term_buffer = term_buffer,
    window = window,
    fixed_param = fixed_param,
    show_messages = show_messages,
    show_exceptions = show_exceptions,
    engine = engine,
    int_time = int_time,
    step_size_jitter = step_size_jitter,
    adapt_gamma = adapt_gamma,
    adapt_kappa = adapt_kappa,
    adapt_t0 = adapt_t0
  )
  call <- .newstan_elapsed(.newstan_run_sampling(self, args))
  StanMCMC$new(
    payload = call$value,
    model = self,
    data = args$data,
    seed = args$seed,
    init = args$init,
    elapsed = call$elapsed,
    metadata = list(
      method = "sample",
      chains = args$chains,
      chain_ids = as.integer(args$chain_ids),
      parallel_chains = args$parallel_chains,
      threads_per_chain = args$threads_per_chain,
      diagnostics = diagnostics,
      save_metric = isTRUE(save_metric),
      show_exceptions = args$show_exceptions,
      save_warmup = args$save_warmup
    )
  )
}
StanModel$set("public", "sample", stan_model_sample)

#' Run optimization
#'
#' @name model-method-optimize
#' @aliases optimize
#' @family StanModel methods
#'
#' @description The `$optimize()` method of a [`StanModel`] object runs Stan's
#'   `"optimize"` method to find the maximum a posteriori (MAP) estimate or
#'   maximum likelihood estimate (MLE). Returns a [`StanMLE`] object.
#'
#' @param algorithm (string) The optimization algorithm: `"lbfgs"`, `"bfgs"`,
#'   `"newton"`, or `"irr"` (identity rational function).
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacob of the inverse transformation?
#' @param init_alpha (number) Initial step size for LBFGS.
#' @param iter (integer) Maximum number of optimization iterations.
#' @param tol_obj (number) Absolute tolerance for changes in objective value.
#' @param tol_rel_obj (number) Relative tolerance for changes in objective value.
#' @param tol_grad (number) Absolute tolerance for the norm of the gradient.
#' @param tol_rel_grad (number) Relative tolerance for the norm of the gradient.
#' @param tol_param (number) Absolute tolerance for changes in parameter values.
#' @param history_size (integer) Number of corrections in LBFGS approximation.
#' @param save_iterations (logical) Should optimization iterations be saved?
#'
#' @return A [`StanMLE`] object containing the point estimate.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$laplace()`][model-method-laplace]
#'
NULL

stan_model_optimize <- function(
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  algorithm = NULL,
  jacobian = FALSE,
  init_alpha = NULL,
  iter = NULL,
  tol_obj = NULL,
  tol_rel_obj = NULL,
  tol_grad = NULL,
  tol_rel_grad = NULL,
  tol_param = NULL,
  history_size = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  save_iterations = FALSE
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    opencl_ids,
    save_cmdstan_config
  )
  if (isTRUE(jacobian)) {
    stop(
      "Jacobian-adjusted optimization is not yet supported by the native service.",
      call. = FALSE
    )
  }
  args <- .newstan_normalize_optimize(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    threads = threads,
    algorithm = algorithm,
    jacobian = jacobian,
    init_alpha = init_alpha,
    iter = iter,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    history_size = history_size,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  call <- .newstan_elapsed(.newstan_run_optimize(self, args))
  StanMLE$new(
    payload = call$value,
    model = self,
    data = args$data,
    seed = args$seed,
    init = args$init,
    elapsed = call$elapsed,
    metadata = list(
      method = "optimize",
      jacobian = args$jacobian,
      threads = args$threads,
      show_exceptions = args$show_exceptions
    )
  )
}
StanModel$set("public", "optimize", stan_model_optimize)

#' Run Laplace approximation
#'
#' @name model-method-laplace
#' @aliases laplace
#' @family StanModel methods
#'
#' @description The `$laplace()` method of a [`StanModel`] object runs Stan's
#'   `"laplace"` method to draw from a Gaussian approximation to the posterior
#'   centered at the mode. Returns a [`StanLaplace`] object.
#'
#' @param mode A numeric vector of parameter values at the mode, or a
#'   [`StanMLE`] object from [`$optimize()`][model-method-optimize]. If `NULL`,
#'   optimization is run first.
#' @param opt_args (list) Additional arguments to pass to
#'   [`$optimize()`][model-method-optimize] when finding the mode.
#' @param draws (integer) The number of draws from the Laplace approximation.
#' @param calculate_lp (logical) Should the log density of the Laplace
#'   approximation be calculated?
#'
#' @return A [`StanLaplace`] object containing approximate posterior draws.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_laplace <- function(
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  mode = NULL,
  opt_args = NULL,
  jacobian = TRUE,
  draws = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  calculate_lp = TRUE
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    opencl_ids,
    save_cmdstan_config
  )
  args <- .newstan_normalize_laplace(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    threads = threads,
    mode = mode,
    opt_args = opt_args,
    jacobian = jacobian,
    draws = draws,
    calculate_lp = calculate_lp,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  mode_fit <- NULL
  if (is.null(args$mode)) {
    mode_fit <- do.call(
      self$optimize,
      c(
        list(
          data = args$data,
          seed = args$seed,
          init = args$init,
          jacobian = args$jacobian,
          show_messages = args$show_messages,
          show_exceptions = args$show_exceptions
        ),
        args$opt_args
      )
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
    payload = call$value,
    model = self,
    data = args$data,
    seed = args$seed,
    init = args$init,
    elapsed = call$elapsed,
    mode = mode_fit %||% args$mode,
    metadata = list(
      method = "laplace",
      jacobian = args$jacobian,
      threads = args$threads,
      show_exceptions = args$show_exceptions
    )
  )
}
StanModel$set("public", "laplace", stan_model_laplace)

#' Run variational inference (ADVI)
#'
#' @name model-method-variational
#' @aliases variational
#' @family StanModel methods
#'
#' @description The `$variational()` method of a [`StanModel`] object runs
#'   Stan's `"variational"` method, which uses Automatic Differentiation
#'   Variational Inference (ADVI) to approximate the posterior distribution.
#'   Returns a [`StanVB`] object.
#'
#' @param algorithm (string) The variational inference algorithm: `"meanfield"`
#'   or `"fullrank"`.
#' @param iter (integer) The number of ADVI iterations.
#' @param grad_samples (integer) Number of samples to use for gradient estimation.
#' @param elbo_samples (integer) Number of samples for ELBO evaluation.
#' @param eta (number) Learning rate for ADVI.
#' @param adapt_engaged (logical) Should the learning rate be adapted?
#' @param adapt_iter (integer) Number of iterations for learning rate adaptation.
#' @param tol_rel_obj (number) Relative tolerance for ELBO convergence.
#' @param eval_elbo (integer) How often to evaluate the ELBO.
#' @param draws (integer) The number of draws from the variational approximation.
#'
#' @return A [`StanVB`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$pathfinder()`][model-method-pathfinder]
#'
NULL

stan_model_variational <- function(
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  save_latent_dynamics = FALSE,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  algorithm = NULL,
  iter = NULL,
  grad_samples = NULL,
  elbo_samples = NULL,
  eta = NULL,
  adapt_engaged = NULL,
  adapt_iter = NULL,
  tol_rel_obj = NULL,
  eval_elbo = NULL,
  draws = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE)
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    opencl_ids,
    save_cmdstan_config
  )
  if (isTRUE(save_latent_dynamics)) {
    stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
  }
  args <- .newstan_normalize_variational(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    threads = threads,
    algorithm = algorithm,
    iter = iter,
    grad_samples = grad_samples,
    elbo_samples = elbo_samples,
    tol_rel_obj = tol_rel_obj,
    eta = eta,
    adapt_engaged = adapt_engaged,
    adapt_iter = adapt_iter,
    eval_elbo = eval_elbo,
    draws = draws,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  call <- .newstan_elapsed(.newstan_run_variational(self, args))
  StanVB$new(
    payload = call$value,
    model = self,
    data = args$data,
    seed = args$seed,
    init = args$init,
    elapsed = call$elapsed,
    metadata = list(
      method = "variational",
      threads = args$threads,
      show_exceptions = args$show_exceptions
    )
  )
}
StanModel$set("public", "variational", stan_model_variational)

#' Run Pathfinder
#'
#' @name model-method-pathfinder
#' @aliases pathfinder
#' @family StanModel methods
#'
#' @description The `$pathfinder()` method of a [`StanModel`] object runs
#'   Stan's `"pathfinder"` method, which uses a parallel iterative optimization
#'   approach to approximate the posterior distribution. Returns a
#'   [`StanPathfinder`] object.
#'
#' @param num_paths (integer) The number of paths to use.
#' @param single_path_draws (integer) Number of draws per path.
#' @param draws (integer) Total number of draws from the approximation.
#' @param max_lbfgs_iters (integer) Maximum LBFGS iterations per path.
#' @param num_elbo_draws (integer) Number of draws for ELBO estimation.
#' @param save_single_paths (logical) Should single path results be saved?
#' @param psis_resample (logical) Should Pareto smoothed importance sampling
#'   resampling be used?
#' @param calculate_lp (logical) Should the log density be calculated?
#'
#' @return A [`StanPathfinder`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_pathfinder <- function(
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  init_alpha = NULL,
  tol_obj = NULL,
  tol_rel_obj = NULL,
  tol_grad = NULL,
  tol_rel_grad = NULL,
  tol_param = NULL,
  history_size = NULL,
  single_path_draws = NULL,
  draws = NULL,
  num_paths = 4,
  max_lbfgs_iters = NULL,
  num_elbo_draws = NULL,
  save_single_paths = NULL,
  psis_resample = NULL,
  calculate_lp = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE)
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    opencl_ids,
    save_cmdstan_config
  )
  args <- .newstan_normalize_pathfinder(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    threads = threads,
    init_alpha = init_alpha,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    history_size = history_size,
    single_path_draws = single_path_draws,
    draws = draws,
    num_paths = num_paths,
    max_lbfgs_iters = max_lbfgs_iters,
    num_elbo_draws = num_elbo_draws,
    save_single_paths = save_single_paths,
    psis_resample = psis_resample,
    calculate_lp = calculate_lp,
    show_messages = show_messages,
    show_exceptions = show_exceptions
  )
  call <- .newstan_elapsed(.newstan_run_pathfinder(self, args))
  StanPathfinder$new(
    payload = call$value,
    model = self,
    data = args$data,
    seed = args$seed,
    init = args$init,
    elapsed = call$elapsed,
    metadata = list(
      method = "pathfinder",
      num_paths = args$num_paths,
      threads = args$threads,
      show_exceptions = args$show_exceptions
    )
  )
}
StanModel$set("public", "pathfinder", stan_model_pathfinder)

#' Run generated quantities
#'
#' @name model-method-generate-quantities
#' @aliases generate_quantities
#' @family StanModel methods
#'
#' @description The `$generate_quantities()` method of a [`StanModel`] object
#'   runs Stan's `"generate quantities"` method, which evaluates the generated
#'   quantities block of the Stan program for a set of parameter values.
#'   Returns a [`StanGQ`] object.
#'
#' @param fitted_params A [`StanFit`] object or a draws matrix containing
#'   parameter values to use for the generated quantities block.
#' @param parallel_chains (integer) The number of chains to run in parallel.
#' @param threads_per_chain (integer) The number of threads per chain.
#'
#' @return A [`StanGQ`] object containing the generated quantities.
#'
#' @seealso [`$sample()`][model-method-sample]
#'
NULL

stan_model_generate_quantities <- function(
  fitted_params,
  data = NULL,
  seed = NULL,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  parallel_chains = getOption("mc.cores", 1),
  threads_per_chain = NULL,
  opencl_ids = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    opencl_ids,
    FALSE
  )
  common <- .newstan_normalize_common(data = data, seed = seed)
  parallel_chains <- as.integer(parallel_chains %||% 1L)
  threads_per_chain <- as.integer(threads_per_chain %||% 1L)
  input <- if (inherits(fitted_params, "StanFit")) {
    fitted_params$draws(format = "draws_matrix")
  } else {
    fitted_params
  }
  call <- .newstan_elapsed(.newstan_run_generate_quantities(
    self,
    common,
    input,
    parallel_chains,
    threads_per_chain
  ))
  StanGQ$new(
    payload = call$value,
    model = self,
    data = common$data,
    seed = common$seed,
    init = NULL,
    elapsed = call$elapsed,
    metadata = list(
      method = "generate_quantities",
      parallel_chains = parallel_chains,
      threads_per_chain = threads_per_chain,
      show_exceptions = common$show_exceptions
    )
  )
}
StanModel$set("public", "generate_quantities", stan_model_generate_quantities)

#' Run gradient diagnostics
#'
#' @name model-method-diagnose
#' @aliases diagnose
#' @family StanModel methods
#'
#' @description The `$diagnose()` method of a [`StanModel`] object runs Stan's
#'   `"diagnose"` method to check the correctness of gradients computed by
#'   Stan. Returns a [`StanDiagnose`] object.
#'
#' @param epsilon (number) The finite difference step size.
#' @param error (number) The maximum allowed relative error.
#'
#' @return A [`StanDiagnose`] object containing gradient check results.
#'
#' @seealso [`$sample()`][model-method-sample]
#'
NULL

stan_model_diagnose <- function(
  data = NULL,
  seed = NULL,
  init = NULL,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  epsilon = NULL,
  error = NULL
) {
  .newstan_reject_backend_files(output_dir, output_basename)
  args <- .newstan_normalize_diagnose(
    data = data,
    seed = seed,
    init = init,
    epsilon = epsilon,
    error = error
  )
  call <- .newstan_elapsed(.newstan_run_diagnose(self, args))
  StanDiagnose$new(
    payload = call$value,
    model = self,
    data = args$data,
    seed = args$seed,
    init = args$init,
    elapsed = call$elapsed,
    metadata = list(
      method = "diagnose",
      epsilon = args$epsilon,
      error = args$error
    )
  )
}
StanModel$set("public", "diagnose", stan_model_diagnose)

# StanModel internal native entry points ---------------------------------------
# These remain public because sourceCpp functions live in a model-specific
# environment. They are not the user-facing API.

stan_model_new_model <- function(data, seed) {
  private$ensure_compiled()
  private$compiled_env_$new_model(data, seed)
}
StanModel$set("public", "new_model", stan_model_new_model)

stan_model_run_model <- function(model, args) {
  private$ensure_compiled()
  private$compiled_env_$run_model(model, args)
}
StanModel$set("public", "run_model", stan_model_run_model)

stan_model_constrained_param_names <- function(model) {
  private$ensure_compiled()
  private$compiled_env_$constrained_param_names(model)
}
StanModel$set(
  "public",
  "constrained_param_names",
  stan_model_constrained_param_names
)

stan_model_native_function <- function(name, required = TRUE) {
  private$ensure_compiled()
  fun <- private$compiled_env_[[name]]
  if (is.null(fun) && required) {
    stop(
      "The compiled model does not provide native function `",
      name,
      "`; recompile the model with the current newstan version.",
      call. = FALSE
    )
  }
  fun
}
StanModel$set("public", "native_function", stan_model_native_function)
