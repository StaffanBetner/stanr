# Precompiled Stan model header support ---------------------------------------

# Appended via `+=` so these win last-flag-wins over Makeconf's own
# CXXFLAGS. Shared between the model TU compile (R/stan_model.R) and the PCH
# build below -- GCC/clang reject a PCH built under mismatched flags.
.stanr_opt_flags <- function() {
  flags <- "-O3 -g0 -w"

  # Floating-point contraction handling can cause numerical inaccuracies
  # at double-precision on ARM64
  if (R.version$arch == "aarch64") {
    flags <- paste(flags, "-ffp-contract=off")
  }

  flags
}


# Wrapper around `system2()`: a seam so tests can count/mock subprocess
# invocations via `testthat::local_mocked_bindings()`.
.stanr_system2 <- function(...) system2(...)

# Same seam for `tools::Rcmd()`, which shells out via `system2()` internally
# but not through `.stanr_system2()`.
.stanr_rcmd <- function(...) tools::Rcmd(...)

# `R CMD config <variable>`, memoized per session and run through
# `.stanr_rcmd()` so Windows' `Rcmd.exe` front-end is used.
.stanr_r_config <- function(variable) {
  memo_key <- paste0("r_config:", variable)
  cached <- .stanr_memo[[memo_key]]
  if (!is.null(cached)) {
    return(cached)
  }
  output <- tryCatch(
    .stanr_rcmd(c("config", variable), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  value <- paste(output, collapse = "\n")
  .stanr_memo[[memo_key]] <- value
  value
}

# Compiler flags injected by sourceCpp's dependency attributes (Rcpp,
# RcppEigen, BH, RcppParallel), memoized per session.
.stanr_dependency_cppflags <- function() {
  cached <- .stanr_memo$dependency_cppflags
  if (!is.null(cached)) {
    return(cached)
  }
  rcpp_parallel_flags <- tryCatch(
    utils::capture.output(RcppParallel::CxxFlags()),
    error = function(e) character()
  )

  flags <- c(
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
  .stanr_memo$dependency_cppflags <- flags
  flags
}

# Locate R's `Makeconf`. Windows keeps it under an arch subdirectory
# (`etc/x64/Makeconf`); falls back to the unsuffixed location if absent.
.stanr_makeconf <- function() {
  if (nzchar(.Platform$r_arch)) {
    arch_makeconf <- file.path(R.home("etc"), .Platform$r_arch, "Makeconf")
    if (file.exists(arch_makeconf)) {
      return(arch_makeconf)
    }
  }
  file.path(R.home("etc"), "Makeconf")
}

# Creates an R-toolchain Makefile for a precompiled header. Names the
# `CXX20*` variables explicitly: bare Makeconf variables would get
# `$(CXX)`'s default `-std`, and GCC/clang reject a PCH built under a
# mismatched one. Creates no directories itself (the caller does, in R)
# since GNU make on Windows falls back to a shell-less `cmd.exe` otherwise.
.stanr_pch_makefile <- function() {
  makefile <- tempfile("stanr-pch-", fileext = ".mk")
  writeLines(
    c(
      paste("include", .stanr_makeconf()),
      ".PHONY: pch",
      "pch:",
      "\t$(CXX20) $(CXX20STD) $(ALL_CPPFLAGS) $(CXX20FLAGS) $(CXX20PICFLAGS) -x c++-header \"$(HEADER)\" -o \"$(PCH)\" $(EXTRA_CXXFLAGS)"
    ),
    makefile
  )
  makefile
}

# Identity string for the active C++ compiler, memoized per session. Used
# to pick clang-vs-gcc PCH flags, and folded into `model_hash` so an
# in-place toolchain upgrade doesn't keep reloading a stale `.so`.
.stanr_compiler_identity <- function() {
  cached <- .stanr_memo$compiler_identity
  if (!is.null(cached)) {
    return(cached)
  }
  cxx20 <- .stanr_r_config("CXX20")
  identity <- if (!nzchar(cxx20)) {
    ""
  } else {
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
  .stanr_memo$compiler_identity <- identity
  identity
}

# The on-disk path of the PCH memoized for `cppflags`, or `NA_character_`.
# Shares `.stanr_pch_flags()`'s memo key so the staleness check in
# `.stanr_compile_with_pch_retry()` needn't duplicate its construction.
.stanr_pch_current <- function(cppflags) {
  memo_key <- paste0("pch_flags:", digest::digest(cppflags))
  .stanr_memo[[memo_key]]$pch %||% NA_character_
}

# Returns flags that make sourceCpp use a cached model PCH, precompiling
# `stanr/model_pch.hpp` (the full cold-compile preamble of a model TU) if
# needed. Memoized per session, keyed on `cppflags`; a memo hit still
# revalidates via `file.exists()`.
#
# clang and GCC discover the PCH differently: clang's `-include-pch <pch>`
# names the `.gch` directly, but GCC's `-include <file>` only picks up a
# PCH from a sibling `<file>.gch`, so GCC needs a stand-in for the header
# (symlink, or a plain copy on Windows) staged inside the cache dir.
.stanr_pch_flags <- function(cppflags, verbose = FALSE, rebuild = FALSE) {
  memo_key <- paste0("pch_flags:", digest::digest(cppflags))
  if (!rebuild) {
    cached <- .stanr_memo[[memo_key]]
    if (!is.null(cached) && (is.na(cached$pch) || file.exists(cached$pch))) {
      return(cached$flags)
    }
  }

  remember <- function(flags, pch = NA_character_) {
    .stanr_memo[[memo_key]] <- list(flags = flags, pch = pch)
    flags
  }

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
    return(remember(""))
  }

  makefile <- .stanr_pch_makefile()
  on.exit(unlink(makefile), add = TRUE)
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
    return(remember(""))
  }

  pch_build_cxxflags <- .stanr_opt_flags()
  if (compiler_type == "clang") {
    # Instantiate templates used by the header at PCH build time, so model
    # TUs consuming the PCH skip re-instantiating them. Clang-only; benign
    # for consumers (they don't need the flag).
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
      # (`.stanr_pch_makefile()`) and the model TU compile both resolve to.
      makeconf = vapply(
        c("CXX20", "CXX20STD", "CXX20FLAGS", "CXX20PICFLAGS", "CPPFLAGS"),
        .stanr_r_config,
        character(1)
      ),
      cppflags = pch_cppflags,
      opt_flags = pch_build_cxxflags,
      # md5 of model_pch.hpp alone is sufficient: its transitive includes
      # only change with the package or dependency versions hashed here.
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
    # The make recipe deliberately has no `mkdir` step (see
    # `.stanr_pch_makefile()`), so the output directory is created here.
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (compiler_type == "gcc" && !file.exists(cache_header)) {
      # Windows has no usable symlinks here -- `file.symlink()` requires
      # Developer Mode or an elevated process and fails outright on non-NTFS
      # volumes -- so stage a plain copy instead. The copy cannot drift from
      # the original: `model_pch.hpp`'s md5 is part of `fingerprint` above,
      # so any edit to it resolves to a different `cache_dir` entirely.
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
        return(remember(""))
      }
    }
    if (verbose) {
      message("[stanr] Compiling precompiled model header...")
    }
    output <- tryCatch(
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
      ),
      error = function(e) conditionMessage(e)
    )
    if (!file.exists(pch)) {
      warning(
        "Could not compile the precompiled model header; compiling without one.\n",
        paste(output, collapse = "\n"),
        call. = FALSE
      )
      return(remember(""))
    }
  }

  if (compiler_type == "clang") {
    remember(paste("-include-pch", shQuote(pch)), pch)
  } else {
    # `-include` makes GCC process `cache_header` as if it were `#include`d
    # first; the extra `-I` keeps the TU's own (now-redundant, header-guard
    # no-op) `#include <stanr/...>` / `#include <stan/model/...>` lines
    # resolving normally on the include path regardless.
    remember(
      paste(
        "-include",
        shQuote(cache_header),
        paste0("-I", shQuote(stanr_include_dir))
      ),
      pch
    )
  }
}

# Compiles with a PCH, retrying once with a rebuilt PCH on staleness.
# Shared by both compile paths. `compile_fn` is
# `function(compilation_cppflags)`, performing the actual compile.
.stanr_compile_with_pch_retry <- function(
  compile_fn,
  cppflags,
  base_cppflags,
  pch_enabled,
  verbose = FALSE
) {
  # PCH staleness is checked directly below (file mtimes) rather than
  # inferred from the compile error's message -- more robust than matching a
  # compiler- and locale-dependent diagnostic string.
  tryCatch(
    compile_fn(cppflags),
    error = function(error) {
      if (!pch_enabled) {
        stop(error)
      }

      pch_path <- .stanr_pch_current(base_cppflags)
      stale <- is.na(pch_path) ||
        !file.exists(pch_path) ||
        {
          deps <- c(
            system.file(
              "include",
              "stanr",
              "model_pch.hpp",
              package = "stanr",
              mustWork = TRUE
            ),
            vapply(
              c("Rcpp", "RcppEigen", "BH", "RcppParallel"),
              function(p) system.file("include", package = p),
              character(1)
            )
          )
          any(file.mtime(deps) > file.mtime(pch_path))
        }
      if (!stale) {
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
