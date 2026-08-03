# Precompiled Stan model header support ---------------------------------------

# Optimization/warning flags appended (via `+=`) after Makeconf's own
# CXXFLAGS/CXX17FLAGS so they win under last-flag-wins compiler precedence,
# instead of being silently overridden the way `-O3 -w` used to be when it
# lived in PKG_CPPFLAGS. Defined once and shared between the model TU compile
# (R/stan_model.R) and the precompiled header build below so the two stay
# byte-identical -- GCC/clang reject a PCH built with mismatched flags.
.newstan_opt_flags <- "-O3 -g0 -w"

#' Thin wrapper around `system2()`.
#'
#' All `system2()` call sites in this file are routed through this function
#' so tests can count/mock subprocess invocations via
#' `testthat::local_mocked_bindings()` without spawning real processes.
#'
#' @noRd
.newstan_system2 <- function(...) system2(...)

#' Return the compiler configuration value used by R's build system.
#'
#' Memoized for the life of the R session: `R CMD config <variable>` is
#' session-stable, so the underlying subprocess only ever runs once per
#' `variable`.
#'
#' @noRd
.newstan_r_config <- function(variable) {
  memo_key <- paste0("r_config:", variable)
  cached <- .newstan_memo[[memo_key]]
  if (!is.null(cached)) {
    return(cached)
  }
  output <- tryCatch(
    .newstan_system2(
      R.home("bin/R"),
      c("CMD", "config", variable),
      stdout = TRUE,
      stderr = FALSE
    ),
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

#' Create an R-toolchain Makefile for a precompiled header.
#'
#' @noRd
.newstan_pch_makefile <- function() {
  makeconf <- file.path(R.home("etc"), "Makeconf")
  makefile <- tempfile("newstan-pch-", fileext = ".mk")
  writeLines(
    c(
      paste("include", makeconf),
      ".PHONY: pch compiler",
      "compiler:",
      "\t@$(CXX) --version",
      "pch:",
      "\t@mkdir -p \"$(dir $(PCH))\"",
      "\t$(CXX) $(ALL_CPPFLAGS) $(CXXFLAGS) $(CXXPICFLAGS) -x c++-header \"$(HEADER)\" -o \"$(PCH)\" $(EXTRA_CXXFLAGS)"
    ),
    makefile
  )
  makefile
}

#' Return an identity string for the active C++ compiler toolchain.
#'
#' Runs `$(CXX) --version` through the same `make`-based probe used to pick
#' PCH flags below, and memoizes the result for the life of the R session
#' (single key -- the toolchain cannot change mid-session).
#'
#' Used for two purposes: selecting PCH flags in `.newstan_pch_flags()`
#' (clang vs gcc), and as a component of `model_hash`
#' (`.compile_stan_model_environment()`, R/stan_model.R). The latter exists
#' because `Rcpp::sourceCpp()`'s own on-disk cache is keyed purely on the
#' source file's path/content identity plus whether a previously built
#' shared object still exists at its recorded path (see
#' `Rcpp:::.sourceCppFindCacheEntryIndex()`) -- it has no notion of compiler
#' identity or version. An in-place toolchain upgrade that leaves
#' `R.version$platform` unchanged (e.g. an Xcode Command Line Tools or
#' system gcc update) would therefore be invisible to sourceCpp's cache, and
#' -- now that `.compile_stan_model_environment()` uses a *persistent*
#' cross-session cache dir instead of a per-session `tempdir()` -- a stale
#' `.so` built by the old toolchain could be `dyn.load`ed indefinitely.
#' Folding compiler identity into `model_hash` gives such an upgrade a new
#' cache entry instead.
#'
#' Returns `""` if `make` is unavailable, mirroring `.newstan_pch_flags()`'s
#' own degrade path (PCH is unavailable under the same condition).
#'
#' @noRd
.newstan_compiler_identity <- function() {
  cached <- .newstan_memo$compiler_identity
  if (!is.null(cached)) {
    return(cached)
  }
  make <- Sys.which("make")
  identity <- if (!nzchar(make)) {
    ""
  } else {
    makefile <- .newstan_pch_makefile()
    on.exit(unlink(makefile), add = TRUE)
    output <- tryCatch(
      .newstan_system2(
        make,
        c("-f", shQuote(makefile), "USE_CXX17=1", "compiler"),
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
#' `cppflags` (always with `rebuild = FALSE`, matching that function's own
#' key -- the memo represents steady-state resolved flags regardless of how
#' they were resolved) and returns the associated PCH path, if any. Used by
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
  memo_key <- paste0(
    "pch_flags:",
    digest::digest(list(cppflags, rebuild = FALSE))
  )
  .newstan_memo[[memo_key]]$pch %||% NA_character_
}

#' Return flags that make sourceCpp use a cached model PCH.
#'
#' Precompiles `newstan/model_pch.hpp` (`src/include/model_pch.hpp`, mirrored
#' to the installed package as `inst/include/newstan/model_pch.hpp`), which
#' transitively covers `stan/model/model_header.hpp`, `Rcpp.h`, and the
#' newstan wrapper headers -- the full cold-compiled preamble of an assembled
#' model translation unit (`inst/stan_model.cpp`).
#'
#' The entire resolved flag string is memoized for the life of the R
#' session, keyed on `digest::digest(list(cppflags, rebuild = FALSE))` (the
#' key always uses `rebuild = FALSE`: the memo represents the steady-state
#' resolved flags, and `rebuild = TRUE` calls bypass the memo lookup
#' entirely, then refresh the memo entry on success). On a memo hit,
#' `file.exists()` is still checked for the associated PCH file before
#' returning it -- the user could have cleared the cache dir mid-session --
#' and a missing file falls through to full recomputation instead of
#' returning stale flags. The `make`-based compiler probe this function runs
#' is likewise memoized (single key: it depends only on the session-stable
#' toolchain), while the actual PCH-build `make` invocation is not (it has
#' real side effects -- writing the `.gch` file -- and is already gated by
#' `file.exists(pch)` checks).
#'
#' The two compiler families use different discovery mechanisms:
#' * clang: `-include-pch <pch>` names the compiled `.gch`/`.pch` file
#'   directly, so it can live anywhere (here, the user cache dir) with an
#'   arbitrary name.
#' * GCC: only supports `-include <file>`, which acts as if `#include
#'   "<file>"` were the first line of the translation unit, and -- per
#'   <https://gcc.gnu.org/onlinedocs/gcc/Precompiled-Headers.html> -- GCC
#'   automatically substitutes `<file>.gch` for `<file>` "if suitable" when
#'   such a sibling file exists. Because `<file>` here is the exact
#'   (absolute) path given to `-include`, not a path resolved by searching
#'   `-I` directories, this sibling lookup happens beside whatever path we
#'   pass -- so a symlink to the installed header placed inside the user
#'   cache dir, together with a `.gch` built beside that symlink, is
#'   sufficient.
#'
#' @noRd
.newstan_pch_flags <- function(cppflags, verbose = FALSE, rebuild = FALSE) {
  memo_key <- paste0(
    "pch_flags:",
    digest::digest(list(cppflags, rebuild = FALSE))
  )
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
      makeconf = vapply(
        c("CXX", "CXXFLAGS", "CXXPICFLAGS", "CPPFLAGS"),
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
  # given (see the mechanism note in the roxygen block above), so a symlink
  # to the real header placed inside the cache dir is enough -- no need to
  # replicate `include/newstan/...` structure the way implicit-inclusion PCH
  # discovery via `-I` would require.
  cache_header <- file.path(cache_dir, "model_pch.hpp")
  pch <- paste0(cache_header, ".gch")

  if (rebuild && file.exists(pch)) {
    unlink(pch)
  }

  if (!file.exists(pch)) {
    if (compiler_type == "gcc" && !file.exists(cache_header)) {
      dir.create(
        dirname(cache_header),
        recursive = TRUE,
        showWarnings = FALSE
      )
      if (!file.symlink(header, cache_header)) {
        warning(
          "Could not create the precompiled-header cache symlink; compiling without one.",
          call. = FALSE
        )
        return(remember(""))
      }
    }
    message("[newstan] Compiling precompiled model header...")
    output <- tryCatch(
      .newstan_system2(
        make,
        c(
          "-f",
          shQuote(makefile),
          "USE_CXX17=1",
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
