# ─── Runtime compilation: C++ -> .so ──────────────────────────────

#' Compile Stan-generated C++ code to a shared library
#'
#' @param cache_dir Directory where the .so will be placed
#' @param cpp_code Stan-generated C++ source code string
#' @param model_name Model class name
#'
#' @keywords internal
compile_model <- function(cache_dir, cpp_code, model_name) {
  # Write C++ source to temp file
  tmpdir <- tempdir()
  cpp_file <- file.path(tmpdir, paste0(model_name, ".cpp"))
  writeLines(cpp_code, cpp_file)

  # Header paths
  newstan_stan <- file.path(system.file("", package = "newstan"),
                            "inst/include/newstan/stan")
  eigen_include <- system.file("include", package = "RcppEigen")
  boost_include <- system.file("include", package = "BH")
  tbb_include <- system.file("include", package = "RcppParallel")
  r_include <- file.path(R.home(), "include")

  # Build compilation command
  inc_flags <- paste0(
    "-I'", newstan_stan, "' ",
    "-I'", eigen_include, "' ",
    "-I'", boost_include, "' ",
    "-I'", tbb_include, "' ",
    "-I'", r_include, "' "
  )

  def_flags <- paste0(
    "-DBOOST_DISABLE_ASSERTS -D_REENTRANT -DSTAN_THREADS -DUSE_STANC3 ",
    "-std=c++17 "
  )

  so_file <- file.path(cache_dir, paste0(model_name, ".so"))
  cmd <- sprintf(
    "g++ -shared -fPIC -O2 %s%s-o '%s' '%s' 2>&1",
    inc_flags, def_flags, so_file, cpp_file
  )

  # Compile and capture output
  compile_output <- system(cmd, intern = TRUE, ignore.stderr = FALSE)

  if (length(compile_output) > 0) {
    cat(paste(compile_output, collapse = "\n"), "\n")
  }

  if (!file.exists(so_file)) {
    stop("Model compilation failed. See output above.")
  }

  # Clean up temp file
  unlink(cpp_file)

  return(invisible(so_file))
}
