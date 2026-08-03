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
  save_cmdstan_config = FALSE
) {
  unsupported <- c(
    if (!is.null(output_dir)) "output_dir",
    if (!is.null(output_basename)) "output_basename",
    if (!is.null(sig_figs)) "sig_figs",
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

# Shared execution path for all StanModel service methods: seed resolution,
# native model construction, service dispatch, timing, and payload assembly.
.newstan_run_service <- function(
  self,
  data,
  seed,
  init = NULL,               # NULL when native_args don't need init (laplace, generate_quantities)
  native_args_fn,            # function(seed, resolved_init, model) -> list
  payload_fn                 # function(result) -> list of method-specific fields
) {
  started <- proc.time()[["elapsed"]]
  seed <- .newstan_seed(seed)
  resolved_init <- if (!is.null(init)) resolve_init(init)
  model <- self$new_model(data, seed)
  native_args <- native_args_fn(seed, resolved_init, model)
  result <- self$run_model(model, native_args)
  payload <- c(
    payload_fn(result),
    list(
      return_code = result$return_code,
      args = service_args(native_args),
      output = result$output %||% character(),
      model_ptr = model
    )
  )
  list(
    payload = payload,
    seed = seed,
    elapsed = unname(proc.time()[["elapsed"]] - started)
  )
}

#' Return the bundled Stan library version.
#'
#' Memoized for the life of the R session (single key, this function takes
#' no arguments): the bundled header cannot change within a session.
#'
#' @keywords internal
.newstan_stan_version <- function() {
  cached <- .newstan_memo$stan_version
  if (!is.null(cached)) {
    return(cached)
  }
  header <- system.file("include", "stan", "version.hpp", package = "newstan")
  value <- if (!nzchar(header) || !file.exists(header)) {
    NA_character_
  } else {
    lines <- readLines(header, warn = FALSE)
    macro_value <- function(macro) {
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
      macro_value("STAN_MAJOR"),
      macro_value("STAN_MINOR"),
      macro_value("STAN_PATCH"),
      sep = "."
    )
  }
  .newstan_memo$stan_version <- value
  value
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
      force_recompile = getOption("newstan_force_recompile", FALSE),
      precompiled_headers = TRUE,
      quiet = TRUE,
      external_cpp = NULL,
      use_opencl = FALSE
    ) {
      compile <- .newstan_flag(compile, "compile")
      force_recompile <- .newstan_flag(force_recompile, "force_recompile")
      precompiled_headers <- .newstan_flag(
        precompiled_headers,
        "precompiled_headers"
      )
      quiet <- .newstan_flag(quiet, "quiet")
      use_opencl <- .newstan_flag(use_opencl, "use_opencl")
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
      private$use_opencl_ <- use_opencl
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
    use_opencl_ = FALSE,
    compiled_env_ = NULL,
    compile_generation_ = 0L,
    variables_ = NULL,
    resolved_code_ = NULL,
    ensure_compiled = function() {
      if (is.null(private$compiled_env_)) {
        self$compile()
      }
      invisible(NULL)
    },
    # `code_` is immutable after `initialize()` (never reassigned), so this
    # needs no invalidation logic: once resolved, it is valid for the
    # object's whole lifetime. Shared between `$compile()` and `$variables()`
    # so `#include` resolution -- a recursive file-system walk -- happens at
    # most once per model, however many times either method is called.
    resolved_code = function() {
      if (is.null(private$resolved_code_)) {
        private$resolved_code_ <- resolve_stan_includes(
          private$code_,
          private$include_paths_
        )
      }
      private$resolved_code_
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
#'   use_opencl()
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
#' * `$use_opencl()` returns `TRUE` if the model was created with
#'   `use_opencl = TRUE` and `FALSE` otherwise.
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

stan_model_compile_generation <- function() private$compile_generation_
StanModel$set("public", "compile_generation", stan_model_compile_generation)

stan_model_cpp_options <- function() private$cpp_options_
StanModel$set("public", "cpp_options", stan_model_cpp_options)

stan_model_stanc_options <- function() private$stanc_options_
StanModel$set("public", "stanc_options", stan_model_stanc_options)

stan_model_use_opencl <- function() private$use_opencl_
StanModel$set("public", "use_opencl", stan_model_use_opencl)

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
      model_code = private$resolved_code(),
      include_directories = character(),
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
  force_recompile <- .newstan_flag(force_recompile, "force_recompile")
  quiet <- .newstan_flag(quiet, "quiet")
  # Incremented unconditionally, before compilation runs, so the generation
  # always reflects "a compile was attempted" -- fits use this (via
  # `$compile_generation()`) to know whether their cached native pointer
  # might now be stale, even if `.compile_stan_model_environment()` below
  # throws partway through.
  private$compile_generation_ <- private$compile_generation_ + 1L
  private$compiled_env_ <- .compile_stan_model_environment(
    code = private$resolved_code(),
    model_name = private$model_name_,
    include_directories = character(),
    external_cpp = private$external_cpp_,
    verbose = !quiet,
    precompiled_headers = private$precompiled_headers_,
    force_recompile = force_recompile,
    use_opencl = private$use_opencl_
  )
  invisible(self)
}
StanModel$set("public", "compile", stan_model_compile)

# Selects the OpenCL platform/device for the model's native computations to
# run on. Triggers lazy compilation via `native_function()` if needed, and
# runs on the R thread before `new_model()` -- with `use_opencl` codegen,
# data transfer to the OpenCL device happens inside the model constructor
# itself, so device selection must happen first. Errors cleanly (via the
# C++ stub's `Rcpp::stop()`) if the compiled model was not built with
# `use_opencl = TRUE`.
stan_model_select_opencl <- function(opencl_ids) {
  ids <- as.integer(opencl_ids)
  if (length(ids) != 2L || anyNA(ids) || any(ids < 0L)) {
    stop("`opencl_ids` must be c(platform_id, device_id).", call. = FALSE)
  }
  self$native_function("select_opencl_device")(ids[[1]], ids[[2]])
  invisible(NULL)
}
StanModel$set("private", "select_opencl", stan_model_select_opencl)

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
#' @param chain_ids (integer vector) The IDs for each chain.
#' @param num_threads (integer) The total number of threads to use across all
#'   chains.
#' @param iter_warmup (integer) The number of warmup iterations.
#' @param iter_sampling (integer) The number of sampling iterations.
#' @param save_warmup (logical) Should warmup samples be saved?
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
#' @param show_messages (logical) When `TRUE` (the default), print progress
#'   and informational output (e.g. iteration and timing messages) to the
#'   console. Set to `FALSE` to silence it. Suppressed output is still
#'   recorded and available via the fit's `$output()` method.
#' @param show_exceptions (logical) When `TRUE` (the default), print
#'   informational messages about numerical exceptions -- e.g. Metropolis
#'   proposal rejections and rejected initial values. Set to `FALSE` to
#'   silence them. Suppressed messages are still recorded and available via
#'   the fit's `$output()` method.
#' @param engine (string) The sampling engine: `"nuts"` or `"static"`.
#' @param int_time (number) Integration time for static HMC.
#' @param step_size_jitter (number) Jitter for step size after adaptation.
#' @param adapt_gamma (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_kappa (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_t0 (number) Adaptation hyperparameter for dual averaging.
#' @param opencl_ids (integer vector) `c(platform_id, device_id)` identifying
#'   the OpenCL platform/device to run on. Only meaningful for a model
#'   compiled with `use_opencl = TRUE` (see [stan_model()]); errors if the
#'   model was not compiled with OpenCL support. Defaults to `NULL`, meaning
#'   `select_opencl_device()` is never called and the platform/device baked
#'   in at compile time (0/0) is used.
#'
#' @return A [`StanMCMC`] object containing posterior draws and diagnostics.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_sample <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  save_latent_dynamics = FALSE,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  chains = 4,
  chain_ids = seq_len(chains),
  num_threads = getOption("mc.cores", 1),
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
    save_cmdstan_config
  )
  save_latent_dynamics <- .newstan_flag(
    save_latent_dynamics,
    "save_latent_dynamics"
  )
  save_warmup <- .newstan_flag(save_warmup, "save_warmup")
  adapt_engaged <- .newstan_flag(adapt_engaged, "adapt_engaged")
  fixed_param <- .newstan_flag(fixed_param, "fixed_param")
  show_messages <- .newstan_flag(show_messages, "show_messages")
  show_exceptions <- .newstan_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
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
  if (!engine %in% c("nuts", "static")) {
    stop("`engine` must be one of \"nuts\", \"static\".", call. = FALSE)
  }
  ids <- .newstan_validate_chains(chains, chain_ids)
  chains <- ids$chains
  chain_ids <- ids$chain_ids
  num_threads <- as.integer(num_threads %||% 1L)
  if (num_threads < 1L) {
    stop("`num_threads` must be positive.", call. = FALSE)
  }
  iter_warmup <- as.integer(iter_warmup)
  iter_sampling <- as.integer(iter_sampling)
  thin <- as.integer(thin)
  if (iter_warmup < 0 || iter_sampling < 0) {
    stop("iter_warmup and iter_sampling must be non-negative.", call. = FALSE)
  }
  if (thin < 1L) {
    stop("thin must be at least 1.", call. = FALSE)
  }
  # Calculate in double precision so adding two valid integer iteration counts
  # cannot overflow to NA before the explicit R-size guard below.
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
  inv_metric <- .newstan_normalize_inv_metric(
    inv_metric = inv_metric,
    metric = metric,
    chains = chains
  )
  refresh <- as.integer(refresh)

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "sample",
      algorithm = if (fixed_param) "fixed_param" else "hmc",
      engine = if (fixed_param) "nuts" else engine,
      metric = metric,
      adapt_engaged = as.logical(adapt_engaged),
      seed = as.integer(seed),
      id = as.integer(chain_ids[[1]]),
      num_chains = as.integer(chains),
      init_radius = resolved_init$radius,
      num_warmup = iter_warmup,
      num_samples = iter_sampling,
      thin = thin,
      save_warmup = as.logical(save_warmup),
      refresh = refresh,
      stepsize = as.double(step_size),
      stepsize_jitter = as.double(step_size_jitter),
      max_depth = as.integer(max_treedepth),
      int_time = as.double(int_time),
      delta = as.double(adapt_delta),
      gamma = as.double(adapt_gamma),
      kappa = as.double(adapt_kappa),
      t0 = as.double(adapt_t0),
      init_buffer = as.integer(init_buffer),
      term_buffer = as.integer(term_buffer),
      window = as.integer(window),
      init = resolved_init$values,
      inv_metric = inv_metric,
      inv_metric_na = is.null(inv_metric),
      verbose = as.logical(show_messages),
      show_exceptions = as.logical(show_exceptions),
      num_threads = num_threads
    )
  }

  payload_fn <- function(result) {
    if (result$return_code != 0) {
      list(draws = NULL, diagnostics = NULL)
    } else {
      draw_names <- dimnames(result$samples)[[3]]
      if (!fixed_param && engine == "static") {
        diagnostic_vars <- c("accept_stat__", "stepsize__")
      } else {
        diagnostic_vars <- c(
          "accept_stat__",
          "stepsize__",
          "treedepth__",
          "n_leapfrog__",
          "divergent__",
          "energy__"
        )
      }
      par_vars <- draw_names[!(draw_names %in% diagnostic_vars)]

      if (fixed_param) {
        diagnostics <- posterior::draws_df(
          "stepsize__" = NA,
          "treedepth__" = NA,
          "n_leapfrog__" = NA,
          "divergent__" = NA,
          "energy__" = NA
        )
        draws <- posterior::as_draws_array(
          result$samples[, , par_vars, drop = FALSE]
        )
      } else {
        all_draws <- posterior::as_draws_array(result$samples)
        diagnostics <- posterior::subset_draws(all_draws, variable = diagnostic_vars)
        draws <- posterior::subset_draws(all_draws, par_vars)
      }

      list(
        draws = draws,
        diagnostics = diagnostics,
        inv_metric = result$inv_metric,
        step_size = result$step_size
      )
    }
  }

  res <- .newstan_run_service(
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
      save_warmup = save_warmup
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
#'   or `"newton"`.
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacobian of the inverse transformation?
#' @param init_alpha (number) Initial step size for LBFGS.
#' @param iter (integer) Maximum number of optimization iterations.
#' @param tol_obj (number) Absolute tolerance for changes in objective value.
#' @param tol_rel_obj (number) Relative tolerance for changes in objective value.
#' @param tol_grad (number) Absolute tolerance for the norm of the gradient.
#' @param tol_rel_grad (number) Relative tolerance for the norm of the gradient.
#' @param tol_param (number) Absolute tolerance for changes in parameter values.
#' @param history_size (integer) Number of corrections in LBFGS approximation.
#' @param save_iterations (logical) Should optimization iterations be saved?
#'   When `TRUE`, [`$draws()`][fit-method-draws] on the returned [`StanMLE`]
#'   exposes the full optimization path (one row per saved iteration,
#'   including the initial point) instead of a single row for the final
#'   estimate. [`$mle()`][fit-method-mle] is unaffected either way and always
#'   reflects the final iteration.
#' @param opencl_ids (integer vector) `c(platform_id, device_id)` identifying
#'   the OpenCL platform/device to run on. Only meaningful for a model
#'   compiled with `use_opencl = TRUE` (see [stan_model()]); errors if the
#'   model was not compiled with OpenCL support. Defaults to `NULL`, meaning
#'   `select_opencl_device()` is never called and the platform/device baked
#'   in at compile time (0/0) is used.
#'
#' @return A [`StanMLE`] object containing the point estimate.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$laplace()`][model-method-laplace]
#'
NULL

stan_model_optimize <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  algorithm = "lbfgs",
  jacobian = FALSE,
  init_alpha = 0.001,
  iter = 2000L,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  history_size = 5L,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  save_iterations = FALSE
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    save_cmdstan_config
  )
  jacobian <- .newstan_flag(jacobian, "jacobian")
  show_messages <- .newstan_flag(show_messages, "show_messages")
  show_exceptions <- .newstan_flag(show_exceptions, "show_exceptions")
  save_iterations <- .newstan_flag(save_iterations, "save_iterations")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }

  threads <- as.integer(threads %||% 1L)
  refresh <- as.integer(refresh)

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "optimize",
      algorithm = algorithm,
      seed = as.integer(seed),
      id = 1L,
      init_radius = resolved_init$radius,
      iter = as.integer(iter),
      init_alpha = as.double(init_alpha),
      tol_obj = as.double(tol_obj),
      tol_rel_obj = as.double(tol_rel_obj),
      tol_grad = as.double(tol_grad),
      tol_rel_grad = as.double(tol_rel_grad),
      tol_param = as.double(tol_param),
      history_size = as.integer(history_size),
      save_iterations = save_iterations,
      jacobian = as.logical(jacobian),
      refresh = refresh,
      verbose = as.logical(show_messages),
      show_exceptions = as.logical(show_exceptions),
      num_threads = threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    # Extract parameter values from last row of par matrix
    par_mat <- result$par
    par_vec <- if (is.matrix(par_mat) && nrow(par_mat) > 0) {
      par_mat[nrow(par_mat), , drop = TRUE]
    } else {
      numeric(0)
    }
    payload <- list(par = par_vec, value = result$value)
    if (save_iterations) {
      payload$iterations <- par_mat
    }
    payload
  }

  res <- .newstan_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanMLE$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "optimize",
      jacobian = jacobian,
      threads = threads,
      show_exceptions = show_exceptions
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
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacobian of the inverse transformation?
#' @param draws (integer) The number of draws from the Laplace approximation.
#' @param calculate_lp (logical) Should the log density of the Laplace
#'   approximation be calculated?
#' @param opencl_ids (integer vector) `c(platform_id, device_id)` identifying
#'   the OpenCL platform/device to run on. Only meaningful for a model
#'   compiled with `use_opencl = TRUE` (see [stan_model()]); errors if the
#'   model was not compiled with OpenCL support. Defaults to `NULL`, meaning
#'   `select_opencl_device()` is never called and the platform/device baked
#'   in at compile time (0/0) is used.
#'
#' @return A [`StanLaplace`] object containing approximate posterior draws.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_laplace <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  mode = NULL,
  opt_args = NULL,
  jacobian = TRUE,
  draws = 1000L,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  calculate_lp = TRUE
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    save_cmdstan_config
  )
  if (!is.null(mode) && !is.null(opt_args)) {
    stop("`mode` and `opt_args` cannot both be supplied.", call. = FALSE)
  }
  jacobian <- .newstan_flag(jacobian, "jacobian")
  calculate_lp <- .newstan_flag(calculate_lp, "calculate_lp")
  show_messages <- .newstan_flag(show_messages, "show_messages")
  show_exceptions <- .newstan_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  # Resolved once so the internal mode-finding optimize() run (if any) and
  # the laplace run itself agree on data/seed/init.
  resolved_data <- data
  resolved_seed <- .newstan_seed(seed)
  resolved_init <- init

  mode_fit <- NULL
  if (is.null(mode)) {
    mode_fit <- do.call(
      self$optimize,
      c(
        list(
          data = resolved_data,
          seed = resolved_seed,
          init = resolved_init,
          jacobian = jacobian,
          show_messages = show_messages,
          show_exceptions = show_exceptions
        ),
        opt_args %||% list()
      )
    )
    mode_val <- mode_fit$mle()
  } else if (inherits(mode, "StanMLE")) {
    mode_fit <- mode
    mode_val <- mode_fit$mle()
  } else {
    # Numeric mode vector (newstan extension)
    mode_val <- mode
  }

  threads <- as.integer(threads %||% 1L)
  refresh <- as.integer(refresh)

  # Extract constrained parameter vector from mode result if needed
  if (is.list(mode_val) && !is.null(mode_val$par)) {
    mode_val <- mode_val$par
  }
  if (!is.numeric(mode_val) || is.null(names(mode_val))) {
    stop(
      "mode must be a named numeric vector or an optimization result.",
      call. = FALSE
    )
  }

  native_args_fn <- function(seed, resolved_init, model) {
    pars <- self$constrained_param_names(model)
    mode_val <- mode_val[.newstan_bracket_names(pars)]
    if (anyNA(mode_val)) {
      stop("mode must contain every constrained model parameter.", call. = FALSE)
    }
    list(
      method = "laplace",
      mode = as.double(mode_val),
      jacobian = as.logical(jacobian),
      draws = as.integer(draws),
      calculate_lp = as.logical(calculate_lp),
      seed = as.integer(seed),
      refresh = refresh,
      verbose = as.logical(show_messages),
      show_exceptions = as.logical(show_exceptions),
      num_threads = threads
    )
  }

  payload_fn <- function(result) {
    list(draws = posterior::as_draws_df(result$draws))
  }

  # `init` is not part of laplace's native_args (the Laplace approximation is
  # centered at `mode`, not resolved via init), so no `resolve_init()` call is
  # made here -- pass init = NULL to the shared runner to preserve that.
  res <- .newstan_run_service(
    self = self,
    data = resolved_data,
    seed = resolved_seed,
    init = NULL,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanLaplace$new(
    payload = res$payload,
    model = self,
    data = resolved_data,
    seed = resolved_seed,
    init = resolved_init,
    elapsed = res$elapsed,
    mode = mode_fit %||% mode,
    metadata = list(
      method = "laplace",
      jacobian = jacobian,
      threads = threads,
      show_exceptions = show_exceptions
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
#' @param opencl_ids (integer vector) `c(platform_id, device_id)` identifying
#'   the OpenCL platform/device to run on. Only meaningful for a model
#'   compiled with `use_opencl = TRUE` (see [stan_model()]); errors if the
#'   model was not compiled with OpenCL support. Defaults to `NULL`, meaning
#'   `select_opencl_device()` is never called and the platform/device baked
#'   in at compile time (0/0) is used.
#'
#' @return A [`StanVB`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$pathfinder()`][model-method-pathfinder]
#'
NULL

stan_model_variational <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  save_latent_dynamics = FALSE,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  algorithm = "meanfield",
  iter = 10000L,
  grad_samples = 1L,
  elbo_samples = 100L,
  eta = 1,
  adapt_engaged = TRUE,
  adapt_iter = 50L,
  tol_rel_obj = 0.01,
  eval_elbo = 100L,
  draws = 1000L,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE)
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    save_cmdstan_config
  )
  save_latent_dynamics <- .newstan_flag(
    save_latent_dynamics,
    "save_latent_dynamics"
  )
  adapt_engaged <- .newstan_flag(adapt_engaged, "adapt_engaged")
  show_messages <- .newstan_flag(show_messages, "show_messages")
  show_exceptions <- .newstan_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  if (save_latent_dynamics) {
    stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
  }

  threads <- as.integer(threads %||% 1L)
  refresh <- as.integer(refresh)

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "variational",
      algorithm = algorithm,
      seed = as.integer(seed),
      id = 1L,
      init_radius = resolved_init$radius,
      iter = as.integer(iter),
      grad_samples = as.integer(grad_samples),
      elbo_samples = as.integer(elbo_samples),
      tol_rel_obj = as.double(tol_rel_obj),
      eta = as.double(eta),
      adapt_engaged = as.logical(adapt_engaged),
      adapt_iter = as.integer(adapt_iter),
      eval_elbo = as.integer(eval_elbo),
      output_samples = as.integer(draws),
      verbose = as.logical(show_messages),
      show_exceptions = as.logical(show_exceptions),
      num_threads = threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    list(draws = if (result$return_code == 0L) {
      posterior::as_draws_df(result$draws)
    })
  }

  res <- .newstan_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanVB$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "variational",
      threads = threads,
      show_exceptions = show_exceptions
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
#' @param opencl_ids (integer vector) `c(platform_id, device_id)` identifying
#'   the OpenCL platform/device to run on. Only meaningful for a model
#'   compiled with `use_opencl = TRUE` (see [stan_model()]); errors if the
#'   model was not compiled with OpenCL support. Defaults to `NULL`, meaning
#'   `select_opencl_device()` is never called and the platform/device baked
#'   in at compile time (0/0) is used.
#'
#' @return A [`StanPathfinder`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_pathfinder <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  init_alpha = 0.001,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  history_size = 5L,
  single_path_draws = 1000L,
  draws = 1000L,
  num_paths = 4,
  max_lbfgs_iters = 1000L,
  num_elbo_draws = 25L,
  save_single_paths = FALSE,
  psis_resample = TRUE,
  calculate_lp = TRUE,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE)
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    save_cmdstan_config
  )
  save_single_paths <- .newstan_flag(save_single_paths, "save_single_paths")
  psis_resample <- .newstan_flag(psis_resample, "psis_resample")
  calculate_lp <- .newstan_flag(calculate_lp, "calculate_lp")
  show_messages <- .newstan_flag(show_messages, "show_messages")
  show_exceptions <- .newstan_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }

  threads <- as.integer(threads %||% 1L)
  refresh <- as.integer(refresh)

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "pathfinder",
      seed = as.integer(seed),
      id = 1L,
      init_radius = resolved_init$radius,
      max_lbfgs_iters = as.integer(max_lbfgs_iters),
      history_size = as.integer(history_size),
      num_elbo_draws = as.integer(num_elbo_draws),
      num_draws = as.integer(single_path_draws),
      num_paths = as.integer(num_paths),
      num_psis_draws = as.integer(draws),
      init_alpha = as.double(init_alpha),
      tol_obj = as.double(tol_obj),
      tol_rel_obj = as.double(tol_rel_obj),
      tol_grad = as.double(tol_grad),
      tol_rel_grad = as.double(tol_rel_grad),
      tol_param = as.double(tol_param),
      save_single_paths = as.logical(save_single_paths),
      psis_resample = as.logical(psis_resample),
      calculate_lp = as.logical(calculate_lp),
      refresh = refresh,
      verbose = as.logical(show_messages),
      show_exceptions = as.logical(show_exceptions),
      num_threads = threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    if (result$return_code != 0) {
      list(draws = NULL)
    } else {
      draws_df <- posterior::as_draws_df(result$draws)

      # Separate special columns from parameters
      special_vars <- c("lp_approx__", "lp__", "path__")
      present_special <- special_vars[special_vars %in% colnames(result$draws)]

      if (length(present_special) > 0) {
        diagnostics <- posterior::subset_draws(draws_df, variable = present_special)
        draws_df <- posterior::subset_draws(
          draws_df,
          variable = setdiff(colnames(result$draws), present_special)
        )
      } else {
        diagnostics <- NULL
      }

      list(draws = draws_df, diagnostics = diagnostics)
    }
  }

  res <- .newstan_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanPathfinder$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "pathfinder",
      num_paths = as.integer(num_paths),
      threads = threads,
      show_exceptions = show_exceptions
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
#' @param num_threads (integer) The total number of threads to use.
#' @param opencl_ids (integer vector) `c(platform_id, device_id)` identifying
#'   the OpenCL platform/device to run on. Only meaningful for a model
#'   compiled with `use_opencl = TRUE` (see [stan_model()]); errors if the
#'   model was not compiled with OpenCL support. Defaults to `NULL`, meaning
#'   `select_opencl_device()` is never called and the platform/device baked
#'   in at compile time (0/0) is used.
#'
#' @return A [`StanGQ`] object containing the generated quantities.
#'
#' @seealso [`$sample()`][model-method-sample]
#'
NULL

stan_model_generate_quantities <- function(
  fitted_params,
  data = list(),
  seed = NULL,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  num_threads = getOption("mc.cores", 1),
  opencl_ids = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  .newstan_reject_backend_files(
    output_dir,
    output_basename,
    sig_figs,
    FALSE
  )
  show_messages <- .newstan_flag(show_messages, "show_messages")
  show_exceptions <- .newstan_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }

  num_threads <- as.integer(num_threads %||% 1L)

  input <- if (inherits(fitted_params, "StanFit")) {
    fitted_params$draws(format = "draws_matrix")
  } else {
    fitted_params
  }

  native_args_fn <- function(seed, resolved_init, model) {
    pars <- self$constrained_param_names(model)

    # Convert draws to matrix (rows=samples, columns=parameters)
    draws_matrix <- if (inherits(input, "draws")) {
      posterior::as_draws_matrix(posterior::subset_draws(
        input,
        variable = pars
      ))
    } else {
      as.matrix(input)
    }

    list(
      method = "generate_quantities",
      seed = as.integer(seed),
      verbose = as.logical(show_messages),
      show_exceptions = as.logical(show_exceptions),
      num_threads = num_threads,
      draws = draws_matrix
    )
  }

  payload_fn <- function(result) {
    list(draws = posterior::as_draws_df(result$samples))
  }

  res <- .newstan_run_service(
    self = self,
    data = data,
    seed = seed,
    init = NULL,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanGQ$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = NULL,
    elapsed = res$elapsed,
    metadata = list(
      method = "generate_quantities",
      num_threads = num_threads,
      show_exceptions = show_exceptions
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
  data = list(),
  seed = NULL,
  init = 2,
  output_dir = getOption("newstan_output_dir"),
  output_basename = NULL,
  epsilon = 1e-6,
  error = 1e-6
) {
  .newstan_reject_backend_files(output_dir, output_basename)

  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0) {
    stop("`epsilon` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(error) || length(error) != 1L || error <= 0) {
    stop("`error` must be a positive number.", call. = FALSE)
  }

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "diagnose",
      epsilon = as.double(epsilon),
      error = as.double(error),
      seed = as.integer(seed),
      id = 1L,
      init_radius = resolved_init$radius,
      verbose = TRUE,
      num_threads = 1L,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    n_failed <- as.integer(result$num_failed)

    # Parse output messages from Stan's test_gradients()
    parsed <- .newstan_parse_diagnose_output(result$output)

    if (n_failed == 0L) {
      message("[newstan] All gradient tests passed.")
    } else {
      message(sprintf(
        "[newstan] %d parameter(s) failed the gradient test.",
        n_failed
      ))
    }

    list(
      num_failed = n_failed,
      gradients = parsed$gradients,
      lp = parsed$lp
    )
  }

  res <- .newstan_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanDiagnose$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "diagnose",
      epsilon = epsilon,
      error = error
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
  withr::with_envvar(
    c(STAN_NUM_THREADS = args$num_threads),
    private$compiled_env_$run_model(model, args)
  )
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
