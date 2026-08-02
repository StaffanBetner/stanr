.compile_stan_model_environment <- function(
  file = NULL,
  code = NULL,
  model_name = NULL,
  include_directories = character(),
  external_cpp = NULL,
  verbose = FALSE,
  precompiled_headers = TRUE,
  force_recompile = FALSE
) {
  # Validate inputs
  if (is.null(file) && is.null(code)) {
    stop("Either 'file' or 'code' must be provided.")
  }
  if (!is.null(file) && !is.null(code)) {
    stop("Provide either 'file' or 'code', not both.")
  }
  if (
    !is.logical(precompiled_headers) ||
      length(precompiled_headers) != 1 ||
      is.na(precompiled_headers)
  ) {
    stop("`precompiled_headers` must be TRUE or FALSE.", call. = FALSE)
  }

  # Read Stan code
  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("File not found: ", file)
    }
    code <- paste(readLines(file, warn = FALSE), collapse = "\n")
  }

  # Determine model name
  if (is.null(model_name)) {
    if (!is.null(file)) {
      model_name <- sub("\\.stan$", "", basename(file))
    } else {
      model_name <- "model"
    }
  }

  if (verbose) {
    message("[newstan] Compiling '", model_name, "'...")
  }

  # Step 1: Stan -> C++ via stanc.js (QuickJSR)
  cpp_code <- stanc(
    code,
    include_directories = include_directories,
    external_cpp = external_cpp
  )
  # The generated wrapper is part of the translation unit.  Include it in the
  # cache key so changes to the native runner/model bridge cannot reuse a
  # sourceCpp artifact compiled against an older wrapper.
  model_support <- readLines(
    system.file("stan_model.cpp", package = "newstan", mustWork = TRUE)
  )
  model_hash <- digest::digest(
    c(
      cpp_code,
      as.character(utils::packageVersion("newstan")),
      .newstan_stan_version()
    ),
    algo = "xxhash64"
  )

  cpp_file <- file.path(tempdir(), paste0("stan_", model_hash, ".cpp"))
  if (!file.exists(cpp_file)) {
    writeLines(c(cpp_code, model_support), cpp_file)
  }

  cppflags <- paste(
    paste0("-I", system.file("include", package = "newstan", mustWork = TRUE)),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS -O3 -w"
  )
  base_cppflags <- cppflags
  pch_enabled <- FALSE
  if (precompiled_headers && length(external_cpp) == 0) {
    pch_flags <- .newstan_pch_flags(base_cppflags, verbose)
    pch_enabled <- nzchar(pch_flags)
    cppflags <- paste(pch_flags, base_cppflags)
  }

  env <- new.env()
  runtime_archive <- system.file(
    "lib",
    Sys.getenv("R_ARCH"),
    "libnewstan_runner.a",
    package = "newstan",
    mustWork = TRUE
  )

  tbb_libs <- utils::capture.output(RcppParallel::RcppParallelLibs())
  if (
    .Platform$OS.type == "windows" &&
      utils::packageVersion("RcppParallel") >= '6.0.0'
  ) {
    tbb_libs <- "-ltbb12 -ltbbmalloc"
  }

  libs <- paste(shQuote(runtime_archive), tbb_libs)

  compile_model <- function(compilation_cppflags) {
    withr::with_makevars(
      c(
        USE_CXX17 = 1,
        PKG_CPPFLAGS = compilation_cppflags,
        PKG_LIBS = libs
      ),
      Rcpp::sourceCpp(
        file = cpp_file,
        env = env,
        rebuild = force_recompile,
        verbose = verbose
      )
    )
  }

  tryCatch(
    compile_model(cppflags),
    error = function(error) {
      if (!pch_enabled || !.newstan_is_stale_pch_error(error)) {
        stop(error)
      }

      if (verbose) {
        message("[newstan] Recompiling stale precompiled model header...")
      }
      pch_flags <- .newstan_pch_flags(base_cppflags, verbose, rebuild = TRUE)
      if (!nzchar(pch_flags)) {
        stop(error)
      }
      compile_model(paste(pch_flags, base_cppflags))
    }
  )

  env
}

#' Create a Stan model object
#'
#' @description Create a new [`StanModel`] object from a Stan program file or
#'   from Stan code as a string. The [`StanModel`] object stores the Stan
#'   program source and compiled model, and provides methods for fitting the
#'   model using Stan's inference algorithms.
#'
#'   See the `compile` argument for control over whether and how compilation
#'   happens.
#'
#' @param stan_file (string) The path to a `.stan` file containing a Stan
#'   program. If `stan_file` is not specified then `code` must be specified.
#' @param code (string) A Stan program as a single string. If `code` is not
#'   specified then `stan_file` must be specified.
#' @param compile (logical) Should the model be compiled? The default is
#'   `TRUE`. If `FALSE` compilation can be done later via the
#'   [`$compile()`][model-method-compile] method.
#' @param model_name (string) The name to use for the model. If `NULL` (the
#'   default), the model name is derived from the Stan file name (if provided)
#'   or set to `"model"`.
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives.
#' @param user_header (string) Not yet supported. Use `external_cpp` instead.
#' @param cpp_options (list) C++ compilation options. Not yet supported by the
#'   in-process backend.
#' @param stanc_options (list) Stan-to-C++ transpiler options. Not yet supported.
#' @param force_recompile (logical) Should the model be recompiled even if it
#'   has not been modified? The default is `FALSE`, but can be set via the
#'   `newstan_force_recompile` option.
#' @param precompiled_headers (logical) Should precompiled headers be used to
#'   speed up compilation? The default is `TRUE`.
#' @param quiet (logical) Should verbose output from compilation be suppressed?
#'   The default is `TRUE`.
#' @param external_cpp (character vector) Paths to C++ files to prepend to the
#'   generated model code. Useful for defining custom functions.
#'
#' @return A [`StanModel`] object.
#'
#' @seealso [`StanModel`], [`$compile()`][model-method-compile],
#'   [`$sample()`][model-method-sample]
#'
#' @examples
#' \dontrun{
#' # Create a StanModel from Stan code
#' mod <- stan_model(
#'   code = "
#'     parameters {
#'       real theta;
#'     }
#'     model {
#'       theta ~ normal(0, 1);
#'     }
#'   "
#' )
#' mod$model_name()
#' mod$variables()
#'
#' # Run MCMC sampling
#' fit <- mod$sample(data = list(), chains = 2)
#' fit$summary()
#' }
#'
#' @export
stan_model <- function(
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
  external_cpp = NULL
) {
  StanModel$new(
    stan_file = stan_file,
    code = code,
    compile = compile,
    model_name = model_name,
    include_paths = include_paths,
    user_header = user_header,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile,
    precompiled_headers = precompiled_headers,
    quiet = quiet,
    external_cpp = external_cpp
  )
}
