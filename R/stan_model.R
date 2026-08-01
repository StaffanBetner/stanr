.compile_stan_model_environment <- function(
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
    "lib", Sys.getenv("R_ARCH"), "libnewstan_runner.a", package = "newstan", mustWork = TRUE
  )

  tbb_libs <- utils::capture.output(RcppParallel::RcppParallelLibs())
  if (.Platform$OS.type == "windows" && utils::packageVersion("RcppParallel") >= '6.0.0') {
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

#' Compile and load a Stan model
#'
#' @param stan_file Path to a Stan program. Supply either `stan_file` or `code`.
#' @param code Stan model code as a single string.
#' @param compile Whether to compile immediately. If `FALSE`, compilation is
#'   deferred until a service method or `$compile()` is called.
#' @param model_name Optional model name.
#' @param include_paths Directories used to resolve Stan includes.
#' @param user_header Optional C++ header to include in model compilation.
#' @param cpp_options Named C++ compilation options. Reserved for compilation
#'   options supported by the in-process backend.
#' @param stanc_options Named options passed to the bundled Stan compiler.
#' @param force_recompile Force native recompilation.
#' @param precompiled_headers Use newstan's cached precompiled Stan header.
#' @param quiet Suppress compilation progress.
#' @param external_cpp Paths to C++ files prepended to generated model code.
#'
#' @return A `StanModel` R6 object.
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
  precompiled_headers = FALSE,
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
