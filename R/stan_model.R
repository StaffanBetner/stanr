#' Compile and load a Stan model
#'
#' @param file Path to a `.stan` file (or `NULL` if `code` is provided)
#' @param code Stan model code as a string (alternative to `file`)
#' @param model_name Override model name (default: basename of `file` without `.stan`)
#' @param force_recompile Whether to always recompile, even if a cached model is found
#' @param verbose Print compilation progress
#'
#' @return An S3 object of class `"newstan_fit"` wrapping a compiled Stan model.
#'
#' @export
stan_model <- function(
  file = NULL,
  code = NULL,
  model_name = NULL,
  verbose = FALSE,
  force_recompile = FALSE
) {
  # Validate inputs
  if (is.null(file) && is.null(code)) {
    stop("Either 'file' or 'code' must be provided.")
  }
  if (!is.null(file) && !is.null(code)) {
    stop("Provide either 'file' or 'code', not both.")
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
  cpp_code <- stanc_process(code)
  model_hash <- digest::digest(cpp_code, algo = "xxhash64")

  cpp_file <- file.path(tempdir(), paste0("stan_", model_hash, ".cpp"))
  if (!file.exists(cpp_file)) {
    writeLines(c(cpp_code, readLines(system.file("stan_model.cpp", package = "newstan", mustWork = TRUE))), cpp_file)
  }

  cppflags <- paste(
    paste0("-I", system.file("include", package = "newstan", mustWork = TRUE)),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS -O3 -w"
  )

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
      USE_CXX17 = 1,
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
