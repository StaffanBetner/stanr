# Precompiled Stan model header support ---------------------------------------

#' Return the compiler configuration value used by R's build system.
#'
#' @keywords internal
.newstan_r_config <- function(variable) {
  output <- tryCatch(
    system2(R.home("bin/R"), c("CMD", "config", variable), stdout = TRUE,
      stderr = FALSE),
    error = function(e) character()
  )
  paste(output, collapse = "\n")
}

#' Compiler flags injected by sourceCpp's dependency attributes.
#'
#' @keywords internal
.newstan_dependency_cppflags <- function() {
  rcpp_parallel_flags <- tryCatch(
    system2(file.path(R.home("bin"), "Rscript"),
      c("-e", shQuote("RcppParallel::CxxFlags()")), stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )

  c(
    paste0("-I", shQuote(system.file("include", package = "Rcpp", mustWork = TRUE))),
    paste0("-I", shQuote(system.file("include", package = "RcppEigen", mustWork = TRUE))),
    paste0("-I", shQuote(system.file("include", package = "BH", mustWork = TRUE))),
    trimws(paste(rcpp_parallel_flags, collapse = " "))
  )
}

#' Create an R-toolchain Makefile for a precompiled header.
#'
#' @keywords internal
.newstan_pch_makefile <- function() {
  makeconf <- file.path(R.home("etc"), "Makeconf")
  makefile <- tempfile("newstan-pch-", fileext = ".mk")
  writeLines(c(
    paste("include", makeconf),
    ".PHONY: pch compiler",
    "compiler:",
    "\t@$(CXX) --version",
    "pch:",
    "\t@mkdir -p \"$(dir $(PCH))\"",
    "\t$(CXX) $(ALL_CPPFLAGS) $(CXXFLAGS) $(CXXPICFLAGS) -x c++-header \"$(HEADER)\" -o \"$(PCH)\""
  ), makefile)
  makefile
}

#' Return flags that make sourceCpp use a cached Stan PCH.
#'
#' @keywords internal
.newstan_pch_flags <- function(cppflags, verbose = FALSE, rebuild = FALSE) {
  header <- system.file("include", "stan", "model", "model_header.hpp",
    package = "newstan", mustWork = TRUE)
  dependency_flags <- .newstan_dependency_cppflags()
  pch_cppflags <- paste(c(cppflags, dependency_flags), collapse = " ")
  make <- Sys.which("make")
  if (!nzchar(make)) {
    warning("Precompiled headers require GNU make; compiling without one.", call. = FALSE)
    return("")
  }

  makefile <- .newstan_pch_makefile()
  on.exit(unlink(makefile), add = TRUE)
  compiler_info <- tryCatch(
    system2(make, c("-f", shQuote(makefile), "USE_CXX17=1", "compiler"),
      stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  compiler <- paste(compiler_info, collapse = "\n")
  compiler_type <- if (grepl("clang", compiler, ignore.case = TRUE)) {
    "clang"
  } else if (grepl("gcc|g\\+\\+", compiler, ignore.case = TRUE)) {
    "gcc"
  } else {
    warning("Precompiled headers are unsupported by this C++ compiler; compiling without one.",
      call. = FALSE)
    return("")
  }

  fingerprint <- digest::digest(list(
    newstan = as.character(utils::packageVersion("newstan")),
    r = R.version$version.string,
    arch = R.version$arch,
    compiler = compiler,
    makeconf = vapply(c("CXX", "CXXFLAGS", "CXXPICFLAGS", "CPPFLAGS"),
      .newstan_r_config, character(1)),
    cppflags = pch_cppflags,
    header = unname(tools::md5sum(header)),
    dependencies = vapply(c("Rcpp", "RcppEigen", "BH", "RcppParallel"),
      function(package) as.character(utils::packageVersion(package)), character(1))
  ), algo = "xxhash64")
  cache_dir <- file.path(tools::R_user_dir("newstan", "cache"), "pch", fingerprint)
  # GCC only discovers a PCH beside the header it resolves. An include overlay
  # keeps the PCH in the user cache rather than modifying an installed package.
  overlay_header <- file.path(cache_dir, "include", "stan", "model", "model_header.hpp")
  pch <- if (compiler_type == "gcc") {
    # GCC looks for a PCH at <resolved-header>.gch.  The overlay header is
    # deliberately first on the include path, so keep its PCH alongside it.
    paste0(overlay_header, ".gch")
  } else {
    file.path(cache_dir, "model_header.hpp.gch")
  }

  if (rebuild && file.exists(pch)) {
    unlink(pch)
  }

  if (!file.exists(pch)) {
    if (compiler_type == "gcc" && !file.exists(overlay_header)) {
      dir.create(dirname(overlay_header), recursive = TRUE, showWarnings = FALSE)
      if (!file.symlink(header, overlay_header)) {
        warning("Could not create the precompiled-header include overlay; compiling without one.",
          call. = FALSE)
        return("")
      }
    }
    if (verbose) {
      message("[newstan] Compiling precompiled model header...")
    }
    output <- tryCatch(
      system2(make, c("-f", shQuote(makefile), "USE_CXX17=1", shQuote(paste0("PCH=", pch)),
        shQuote(paste0("HEADER=", if (compiler_type == "gcc") overlay_header else header)),
        shQuote(paste0("PKG_CPPFLAGS=", pch_cppflags)), "pch"),
        stdout = TRUE, stderr = TRUE),
      error = function(e) conditionMessage(e)
    )
    if (!file.exists(pch)) {
      warning("Could not compile the precompiled model header; compiling without one.\n",
        paste(output, collapse = "\n"), call. = FALSE)
      return("")
    }
  }

  if (compiler_type == "clang") {
    paste("-include-pch", shQuote(pch))
  } else {
    paste0("-I", shQuote(file.path(cache_dir, "include")))
  }
}

#' Whether a compiler error indicates that the cached PCH is stale.
#'
#' @keywords internal
.newstan_is_stale_pch_error <- function(error) {
  grepl(
    "has been modified since the precompiled header",
    conditionMessage(error),
    fixed = TRUE
  )
}
