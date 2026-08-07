# Precompiled Stan model header support ---------------------------------------

.stanr_opt_flags <- function() {
  flags <- "-O3 -g0 -w"

  # Avoid FP-contraction inaccuracies at double precision on ARM64.
  if (R.version$arch == "aarch64") {
    flags <- paste(flags, "-ffp-contract=off")
  }

  flags
}


# Seams for testthat::local_mocked_bindings().
.stanr_system2 <- function(...) system2(...)
.stanr_rcmd <- function(...) tools::Rcmd(...)

# `R CMD config <variable>` via `.stanr_rcmd()` (Windows Rcmd.exe).
.stanr_r_config <- function(variable) {
  output <- tryCatch(
    .stanr_rcmd(c("config", variable), stdout = TRUE, stderr = FALSE),
    error = function(e) character(),
    warning = function(e) character()
  )
  paste(output, collapse = "\n")
}

# R's Makeconf; arch-suffixed on Windows.
.stanr_makeconf <- function() {
  if (nzchar(.Platform$r_arch)) {
    arch_makeconf <- file.path(R.home("etc"), .Platform$r_arch, "Makeconf")
    if (file.exists(arch_makeconf)) {
      return(arch_makeconf)
    }
  }
  file.path(R.home("etc"), "Makeconf")
}

# Rcpp/RcppEigen/BH/RcppParallel include flags. RcppParallel's vary by TBB
# usage, so they come from `RcppParallel::CxxFlags()`.
.stanr_dependency_cppflags <- function() {
  rcpp_parallel_flags <- tryCatch(
    utils::capture.output(RcppParallel::CxxFlags()),
    error = function(e) character()
  )

  c(
    paste0(
      "-I",
      shQuote(system.file("include", package = "Rcpp", mustWork = TRUE))
    ),
    paste0(
      "-I",
      shQuote(system.file("include", package = "RcppEigen", mustWork = TRUE))
    ),
    paste0(
      "-I",
      shQuote(system.file("include", package = "BH", mustWork = TRUE))
    ),
    trimws(paste(rcpp_parallel_flags, collapse = " "))
  )
}

# C++ compiler identity (from `--version`), for clang-vs-gcc PCH flags and
# the model cache key.
.stanr_compiler_identity <- function() {
  cxx20 <- .stanr_r_config("CXX20")
  if (!nzchar(cxx20)) {
    return("")
  }
  cxx20_words <- strsplit(cxx20, "\\s+")[[1]]
  output <- tryCatch(
    .stanr_system2(
      cxx20_words[[1]],
      c(cxx20_words[-1], "--version"),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) character()
  )
  paste(output, collapse = "\n")
}

# Flags making the model compile use a cached PCH, building it first if
# needed. clang names the `.gch` via `-include-pch`; GCC needs a stand-in
# header (symlink, or copy on Windows) staged next to its `.gch`.
.stanr_pch_flags <- function(cppflags, verbose = FALSE, rebuild = FALSE) {
  header <- system.file(
    "include",
    "stanr",
    "model_pch.hpp",
    package = "stanr",
    mustWork = TRUE
  )
  stanr_include_dir <- system.file(
    "include",
    package = "stanr",
    mustWork = TRUE
  )
  dependency_flags <- .stanr_dependency_cppflags()
  pch_cppflags <- paste(c(cppflags, dependency_flags), collapse = " ")
  make <- Sys.which("make")
  if (!nzchar(make)) {
    warning(
      "Precompiled headers require GNU make; compiling without one.",
      call. = FALSE
    )
    return("")
  }

  makefile <- system.file("pch.mk", package = "stanr", mustWork = TRUE)
  compiler <- .stanr_compiler_identity()
  compiler_type <- if (grepl("clang", compiler, ignore.case = TRUE)) {
    "clang"
  } else if (grepl("gcc|g\\+\\+", compiler, ignore.case = TRUE)) {
    "gcc"
  } else {
    warning(
      "Precompiled headers are unsupported by this C++ compiler; compiling without one.",
      call. = FALSE
    )
    return("")
  }

  pch_build_cxxflags <- .stanr_opt_flags()
  if (compiler_type == "clang") {
    # Instantiate the header's templates at PCH build time (clang-only).
    pch_build_cxxflags <- paste(
      pch_build_cxxflags,
      "-fpch-instantiate-templates"
    )
  }

  fingerprint <- digest::digest(
    list(
      stanr = as.character(utils::packageVersion("stanr")),
      r = R.version$version.string,
      arch = R.version$arch,
      compiler = compiler,
      # The CXX20* family, matching the variables the PCH recipe
      # (`inst/pch.mk`) and the model TU compile both resolve to.
      makeconf = vapply(
        c("CXX20", "CXX20STD", "CXX20FLAGS", "CXX20PICFLAGS", "CPPFLAGS"),
        .stanr_r_config,
        character(1)
      ),
      cppflags = pch_cppflags,
      opt_flags = pch_build_cxxflags,
      header = unname(tools::md5sum(header)),
      dependencies = vapply(
        c("Rcpp", "RcppEigen", "BH", "RcppParallel"),
        function(package) as.character(utils::packageVersion(package)),
        character(1)
      )
    ),
    algo = "xxhash64"
  )
  cache_dir <- file.path(
    getOption(
      "stanr_pch_dir",
      file.path(tools::R_user_dir("stanr", "cache"), "pch")
    ),
    fingerprint
  )
  cache_header <- file.path(cache_dir, "model_pch.hpp")
  pch <- paste0(cache_header, ".gch")

  if (rebuild && file.exists(pch)) {
    unlink(pch)
  }

  if (!file.exists(pch)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (compiler_type == "gcc" && !file.exists(cache_header)) {
      # Windows lacks usable symlinks; copy instead. The copy can't drift:
      # header md5 is in the fingerprint, so edits get a fresh cache dir.
      staged <- if (.Platform$OS.type == "windows") {
        file.copy(header, cache_header, overwrite = TRUE)
      } else {
        file.symlink(header, cache_header)
      }
      if (!isTRUE(staged)) {
        warning(
          "Could not stage the header in the precompiled-header cache; compiling without one.",
          call. = FALSE
        )
        return("")
      }
    }
    if (verbose) {
      message("[stanr] Compiling precompiled model header...")
    }
    # MAKEFILES pulls in R's Makeconf, defining the recipe's CXX20* vars.
    output <- tryCatch(
      withr::with_envvar(
        c(MAKEFILES = .stanr_makeconf()),
        .stanr_system2(
          make,
          c(
            "-f",
            shQuote(makefile),
            shQuote(paste0("PCH=", pch)),
            shQuote(paste0(
              "HEADER=",
              if (compiler_type == "gcc") cache_header else header
            )),
            shQuote(paste0("PKG_CPPFLAGS=", pch_cppflags)),
            shQuote(paste0("EXTRA_CXXFLAGS=", pch_build_cxxflags)),
            "pch"
          ),
          stdout = TRUE,
          stderr = TRUE
        )
      ),
      error = function(e) conditionMessage(e)
    )
    if (!file.exists(pch)) {
      warning(
        "Could not compile the precompiled model header; compiling without one.\n",
        paste(output, collapse = "\n"),
        call. = FALSE
      )
      return("")
    }
  }

  if (compiler_type == "clang") {
    paste("-include-pch", shQuote(pch))
  } else {
    # `-include` makes GCC use the sibling .gch; the extra -I keeps the
    # TU's own (now no-op) includes resolving.
    paste(
      "-include",
      shQuote(cache_header),
      paste0("-I", shQuote(stanr_include_dir))
    )
  }
}

# Compiles with a PCH, retrying once with a rebuilt PCH when the compiler
# reports a stale/mismatched one.
.stanr_compile_with_pch_retry <- function(
  compile_fn,
  cppflags,
  base_cppflags,
  pch_enabled,
  verbose = FALSE
) {
  tryCatch(
    compile_fn(cppflags),
    error = function(error) {
      if (!pch_enabled) {
        stop(error)
      }
      msg <- conditionMessage(error)
      pch_related <- grepl("PCH file", msg) ||
        grepl("precompiled header", msg) ||
        grepl(".hpp.gch", msg)
      if (!pch_related) {
        stop(error)
      }

      if (verbose) {
        message(
          "[stanr] Compile failed; rebuilding precompiled model header and retrying..."
        )
      }
      pch_flags <- .stanr_pch_flags(base_cppflags, verbose, rebuild = TRUE)
      if (!nzchar(pch_flags)) {
        stop(error)
      }
      compile_fn(paste(pch_flags, base_cppflags))
    }
  )
}
