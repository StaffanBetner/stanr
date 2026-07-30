#' Compile and load a Stan model
#'
#' @param file Path to a `.stan` file (or `NULL` if `code` is provided)
#' @param code Stan model code as a string (alternative to `file`)
#' @param model_name Override model name (default: basename of `file` without `.stan`)
#' @param include_directories Directories searched, in order, to resolve Stan
#'   `#include` directives.
#' @param external_cpp `NULL` or paths to C++ files prepended to the generated
#'   C++ before the model is compiled. See [stanc()].
#' @param precompiled_headers Whether to use a cached precompiled Stan model header.
#'   This substantially speeds up repeated model compilations, but the initial
#'   build is large and is stored in the user's cache directory. It is disabled
#'   automatically when `external_cpp` is supplied.
#' @param force_recompile Whether to always recompile, even if a cached model is found
#' @param verbose Print compilation progress
#'
#' @return An environment containing a `new_model()` function for instantiating a model
#' @export
stan_model <- function(
  file = NULL,
  code = NULL,
  model_name = NULL,
  include_directories = character(),
  external_cpp = NULL,
  verbose = FALSE,
  precompiled_headers = FALSE,
  force_recompile = FALSE
) {
  # Validate inputs
  if (is.null(file) && is.null(code)) {
    stop("Either 'file' or 'code' must be provided.")
  }
  if (!is.null(file) && !is.null(code)) {
    stop("Provide either 'file' or 'code', not both.")
  }
  if (!is.logical(precompiled_headers) || length(precompiled_headers) != 1 || is.na(precompiled_headers)) {
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
  model_hash <- digest::digest(c(cpp_code, model_support), algo = "xxhash64")

  cpp_file <- file.path(tempdir(), paste0("stan_", model_hash, ".cpp"))
  if (!file.exists(cpp_file)) {
    writeLines(c(cpp_code, model_support), cpp_file)
  }

  cppflags <- paste(
    paste0("-I", system.file("include", package = "newstan", mustWork = TRUE)),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS -O3 -w"
  )
  if (precompiled_headers && length(external_cpp) == 0) {
    cppflags <- paste(.newstan_pch_flags(cppflags, verbose), cppflags)
  }

  env <- new.env()
  runtime_archive <- system.file(
    "lib", Sys.getenv("R_ARCH"), "libnewstan_runner.a", package = "newstan", mustWork = TRUE
  )

  tbb_libs <- utils::capture.output(RcppParallel::RcppParallelLibs())
  if (.Platform$OS.type == "windows" && utils::packageVersion("RcppParallel") >= '6.0.0') {
    tbb_libs <- "-ltbb12 -ltbbmalloc"
  }

  libs <- paste(shQuote(runtime_archive), tbb_libs)

  withr::with_makevars(
    c(
      USE_CXX20 = 1,
      PKG_CPPFLAGS = cppflags,
      PKG_LIBS = libs
    ),
    Rcpp::sourceCpp(
      file = cpp_file,
      env = env,
      rebuild = force_recompile,
      verbose = verbose
    )
  )

  env
}
