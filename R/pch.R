# Precompiled Stan model header support ---------------------------------------

# Optimization/warning flags appended (via `+=`) after Makeconf's own
# CXXFLAGS/CXX17FLAGS so they win under last-flag-wins compiler precedence,
# instead of being silently overridden the way `-O3 -w` used to be when it
# lived in PKG_CPPFLAGS. Defined once and shared between the model TU compile
# (R/stan_model.R) and the precompiled header build below so the two stay
# byte-identical -- GCC/clang reject a PCH built with mismatched flags.
.newstan_opt_flags <- function() {
  flags <- "-O3 -g0 -w"

  # Floating-point contraction handling can cause numerical inaccuracies
  # at double-precision on ARM64
  if (.Platform$r_arch == "aarch64") {
    flags <- paste(flags, "-ffp-contract=off")
  }
}


#' Thin wrapper around `system2()`.
#'
#' All `system2()` call sites in this file are routed through this function
#' so tests can count/mock subprocess invocations via
#' `testthat::local_mocked_bindings()` without spawning real processes.
#'
#' @noRd
.newstan_system2 <- function(...) system2(...)

#' Thin wrapper around `tools::Rcmd()`.
#'
#' Gives tests the same mocking seam as `.newstan_system2()` above, for the
#' `R CMD config` calls made by `.newstan_r_config()` -- `tools::Rcmd()`
#' shells out via `system2()` internally, but not through `.newstan_system2`,
#' so it needs its own seam.
#'
#' @noRd
.newstan_rcmd <- function(...) tools::Rcmd(...)

#' Return the compiler configuration value used by R's build system.
#'
#' Memoized for the life of the R session: `R CMD config <variable>` is
#' session-stable, so the underlying subprocess only ever runs once per
#' `variable`. Delegates to `tools::Rcmd()` (via `.newstan_rcmd()`) rather
#' than constructing the `R CMD` invocation by hand, so the Windows
#' `Rcmd.exe` front-end is used correctly there too.
#'
#' @noRd
.newstan_r_config <- function(variable) {
  memo_key <- paste0("r_config:", variable)
  cached <- .newstan_memo[[memo_key]]
  if (!is.null(cached)) {
    return(cached)
  }
  output <- tryCatch(
    .newstan_rcmd(c("config", variable), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  value <- paste(output, collapse = "\n")
  .newstan_memo[[memo_key]] <- value
  value
}

#' Compiler flags injected by sourceCpp's dependency attributes.
#'
#' Memoized for the life of the R session (single key, this function takes
#' no arguments): the installed package versions and `RcppParallel::CxxFlags()`
#' output are session-stable. `RcppParallel::CxxFlags()` is captured
#' in-process (mirroring `RcppParallel::RcppParallelLibs()` in
#' `.compile_stan_model_environment()`, R/stan_model.R) instead of shelling
#' out to `Rscript`.
#'
#' @noRd
.newstan_dependency_cppflags <- function() {
  cached <- .newstan_memo$dependency_cppflags
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
  .newstan_memo$dependency_cppflags <- flags
  flags
}

#' Locate R's `Makeconf`.
#'
#' Windows keeps it under an architecture subdirectory (`etc/x64/Makeconf`);
#' the platforms where `.Platform$r_arch` is `""` keep it directly in
#' `etc/`. Falls back to the unsuffixed location if the arch-suffixed one
#' is absent.
#'
#' @noRd
.newstan_makeconf <- function() {
  if (nzchar(.Platform$r_arch)) {
    arch_makeconf <- file.path(R.home("etc"), .Platform$r_arch, "Makeconf")
    if (file.exists(arch_makeconf)) {
      return(arch_makeconf)
    }
  }
  file.path(R.home("etc"), "Makeconf")
}

#' Create an R-toolchain Makefile for a precompiled header.
#'
#' The recipe names the `CXX17*` variables explicitly rather than the plain
#' `CXX`/`CXXFLAGS`/`CXXPICFLAGS` ones. `USE_CXX17` is not a Makeconf switch --
#' R implements it in `tools:::.shlib_internal()`, which substitutes
#' `$(CXX17) $(CXX17STD)` etc. before invoking make -- so a bare `make -f`
#' against Makeconf would silently get `$(CXX)`'s own default standard
#' (`-std=gnu++20` as of R 4.6) while `sourceCpp()` compiles the model TU
#' (R/stan_model.R, which sets `USE_CXX17=1`) at `-std=gnu++17`. GCC/clang
#' reject a PCH built under a different `-std` than its consumer.
#'
#' The recipe does not create `$(dir $(PCH))`; the caller does that in R,
#' so the recipe needs no shell built-ins beyond the compiler itself (GNU
#' make on Windows falls back to `cmd.exe` when no `sh` is on `PATH`, where
#' `mkdir -p` would fail and abort the build).
#'
#' @noRd
.newstan_pch_makefile <- function() {
  makefile <- tempfile("newstan-pch-", fileext = ".mk")
  writeLines(
    c(
      paste("include", .newstan_makeconf()),
      ".PHONY: pch",
      "pch:",
      "\t$(CXX17) $(CXX17STD) $(ALL_CPPFLAGS) $(CXX17FLAGS) $(CXX17PICFLAGS) -x c++-header \"$(HEADER)\" -o \"$(PCH)\" $(EXTRA_CXXFLAGS)"
    ),
    makefile
  )
  makefile
}

#' Return an identity string for the active C++ compiler toolchain.
#'
#' Memoized for the life of the R session (single key -- the toolchain
#' cannot change mid-session). Used to select PCH flags in
#' `.newstan_pch_flags()` (clang vs gcc), and as a component of `model_hash`
#' (`.compile_stan_model_environment()`, R/stan_model.R): `Rcpp::sourceCpp()`'s
#' own cache has no notion of compiler identity, so an in-place toolchain
#' upgrade could otherwise leave a stale `.so` reloaded indefinitely.
#'
#' @noRd
.newstan_compiler_identity <- function() {
  cached <- .newstan_memo$compiler_identity
  if (!is.null(cached)) {
    return(cached)
  }
  cxx17 <- .newstan_r_config("CXX17")
  identity <- if (!nzchar(cxx17)) {
    ""
  } else {
    cxx17_words <- strsplit(cxx17, "\\s+")[[1]]
    output <- tryCatch(
      .newstan_system2(
        cxx17_words[[1]],
        c(cxx17_words[-1], "--version"),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) character()
    )
    paste(output, collapse = "\n")
  }
  .newstan_memo$compiler_identity <- identity
  identity
}

#' Return the on-disk path of the PCH currently memoized for `cppflags`.
#'
#' Reconstructs the exact memo key `.newstan_pch_flags()` uses for a given
#' `cppflags` (the key does not depend on `rebuild` -- the memo represents
#' steady-state resolved flags regardless of how they were resolved) and
#' returns the associated PCH path, if any. Used by
#' `.compile_stan_model_environment()` (R/stan_model.R) to check whether a
#' just-failed compile's PCH is stale (and therefore worth rebuilding) without
#' duplicating the digest key construction there.
#'
#' Returns `NA_character_` when no PCH is memoized for `cppflags` (either
#' `.newstan_pch_flags()` was never called with these flags in this session,
#' or it was and PCH was unavailable, e.g. no `make`).
#'
#' @noRd
.newstan_pch_current <- function(cppflags) {
  memo_key <- paste0("pch_flags:", digest::digest(cppflags))
  .newstan_memo[[memo_key]]$pch %||% NA_character_
}

#' Return flags that make sourceCpp use a cached model PCH.
#'
#' Precompiles `newstan/model_pch.hpp` (`src/include/model_pch.hpp`, mirrored
#' to the installed package as `inst/include/newstan/model_pch.hpp`), which
#' transitively covers `stan/model/model_header.hpp`, `Rcpp.h`, and the
#' newstan wrapper headers -- the full cold-compiled preamble of an assembled
#' model translation unit (`inst/stan_model.cpp`).
#' The resolved flags are memoized per-session, keyed on `cppflags` alone (not
#' `rebuild`). A memo hit still revalidates the cached PCH via
#' `file.exists()`; a miss falls through to recomputation.
#'
#' The two compiler families use different discovery mechanisms:
#' * clang: `-include-pch <pch>` names the compiled `.gch` file directly, so
#'   it can live anywhere with an arbitrary name.
#' * GCC: only supports `-include <file>`, which substitutes a sibling
#'   `<file>.gch` when one exists beside the exact (absolute) path given --
#'   so a stand-in for the header inside the cache dir (a symlink, or a plain
#'   copy on Windows, where symlinks are unavailable), with a `.gch` built
#'   beside it, is sufficient.
#'
#' @noRd
.newstan_pch_flags <- function(cppflags, verbose = FALSE, rebuild = FALSE) {
  memo_key <- paste0("pch_flags:", digest::digest(cppflags))
  if (!rebuild) {
    cached <- .newstan_memo[[memo_key]]
    if (!is.null(cached) && (is.na(cached$pch) || file.exists(cached$pch))) {
      return(cached$flags)
    }
  }

  remember <- function(flags, pch = NA_character_) {
    .newstan_memo[[memo_key]] <- list(flags = flags, pch = pch)
    flags
  }

  header <- system.file(
    "include",
    "newstan",
    "model_pch.hpp",
    package = "newstan",
    mustWork = TRUE
  )
  newstan_include_dir <- system.file(
    "include",
    package = "newstan",
    mustWork = TRUE
  )
  dependency_flags <- .newstan_dependency_cppflags()
  pch_cppflags <- paste(c(cppflags, dependency_flags), collapse = " ")
  make <- Sys.which("make")
  if (!nzchar(make)) {
    warning(
      "Precompiled headers require GNU make; compiling without one.",
      call. = FALSE
    )
    return(remember(""))
  }

  makefile <- .newstan_pch_makefile()
  on.exit(unlink(makefile), add = TRUE)
  compiler <- .newstan_compiler_identity()
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

  fingerprint <- digest::digest(
    list(
      newstan = as.character(utils::packageVersion("newstan")),
      r = R.version$version.string,
      arch = R.version$arch,
      compiler = compiler,
      # The CXX17* family, matching the variables the PCH recipe
      # (`.newstan_pch_makefile()`) and the model TU compile both resolve to.
      makeconf = vapply(
        c("CXX17", "CXX17STD", "CXX17FLAGS", "CXX17PICFLAGS", "CPPFLAGS"),
        .newstan_r_config,
        character(1)
      ),
      cppflags = pch_cppflags,
      opt_flags = .newstan_opt_flags,
      # md5 of model_pch.hpp alone (not each header it transitively
      # includes) is sufficient: the newstan wrapper headers it pulls in
      # only change on package reinstall, which is already covered by the
      # `newstan` package-version entry above, and Rcpp/Stan's own headers
      # are covered by the `dependencies` versions below / model_header.hpp
      # shipping inside the same newstan install.
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
      "newstan_pch_dir",
      file.path(tools::R_user_dir("newstan", "cache"), "pch")
    ),
    fingerprint
  )
  # GCC's `-include` discovers a sibling `.gch` beside the literal path it is
  # given (see the mechanism note in the roxygen block above), so a stand-in
  # for the real header placed inside the cache dir is enough -- no need to
  # replicate `include/newstan/...` structure the way implicit-inclusion PCH
  # discovery via `-I` would require.
  cache_header <- file.path(cache_dir, "model_pch.hpp")
  pch <- paste0(cache_header, ".gch")

  if (rebuild && file.exists(pch)) {
    unlink(pch)
  }

  if (!file.exists(pch)) {
    # The make recipe deliberately has no `mkdir` step (see
    # `.newstan_pch_makefile()`), so the output directory is created here.
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
      message("[newstan] Compiling precompiled model header...")
    }
    output <- tryCatch(
      .newstan_system2(
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
          shQuote(paste0("EXTRA_CXXFLAGS=", .newstan_opt_flags)),
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
    # no-op) `#include <newstan/...>` / `#include <stan/model/...>` lines
    # resolving normally on the include path regardless.
    remember(
      paste(
        "-include",
        shQuote(cache_header),
        paste0("-I", shQuote(newstan_include_dir))
      ),
      pch
    )
  }
}

#' Compile with a PCH, retrying once with a rebuilt PCH on staleness.
#'
#' Shared by both compile paths (model TU and functions TU:
#' `.compile_stan_model_environment()` in R/stan_model.R and
#' `.compile_standalone_functions_environment()` in R/expose.R), so it
#' lives here alongside the other PCH helpers it calls
#' (`.newstan_pch_current()`, `.newstan_pch_flags()`) rather than in either
#' caller. `compile_fn` is a one-argument function `function(compilation_cppflags)`
#' that performs the actual compile.
#'
#' @noRd
.newstan_compile_with_pch_retry <- function(
  compile_fn,
  cppflags,
  base_cppflags,
  pch_enabled,
  verbose = FALSE
) {
  # `Rcpp::sourceCpp()` never propagates the compiler's actual diagnostics --
  # it always raises a generic synthetic error -- so PCH staleness must be
  # checked directly below rather than inferred from the error message.
  tryCatch(
    compile_fn(cppflags),
    error = function(error) {
      if (!pch_enabled) {
        stop(error)
      }

      pch_path <- .newstan_pch_current(base_cppflags)
      stale <- is.na(pch_path) ||
        !file.exists(pch_path) ||
        {
          deps <- c(
            system.file(
              "include",
              "newstan",
              "model_pch.hpp",
              package = "newstan",
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
          "[newstan] Compile failed; rebuilding precompiled model header and retrying..."
        )
      }
      pch_flags <- .newstan_pch_flags(base_cppflags, verbose, rebuild = TRUE)
      if (!nzchar(pch_flags)) {
        stop(error)
      }
      compile_fn(paste(pch_flags, base_cppflags))
    }
  )
}
