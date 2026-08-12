# Precompiled Stan model header support

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

# The makefiles vector `R CMD SHLIB` passes to make (see
# tools:::.shlib_internal()): Makeconf, site Makevars, platform shlib
# makefile, user Makevars. Passing the user Makevars is essential so the PCH
# is built with the same CXX20* flags (e.g. -march=native) as the model TU.
.stanr_makefiles <- function() {
  rarch <- if (nzchar(.Platform$r_arch)) paste0("/", .Platform$r_arch) else ""
  c(
    file.path(paste0(R.home("etc"), rarch), "Makeconf"),
    tools::makevars_site(),
    file.path(
      R.home("share"),
      "make",
      if (.Platform$OS.type == "windows") "winshlib.mk" else "shlib.mk"
    ),
    tools::makevars_user()
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

# Retain the two most recently used, successfully built PCHs.  The cache
# directory is entirely owned by stanr, and each fingerprint gets its own
# subdirectory, so removing an old entry cannot affect a PCH in use.
.stanr_prune_pch_cache <- function(cache_root, keep = 2L) {
  entries <- list.dirs(cache_root, full.names = TRUE, recursive = FALSE)
  if (!length(entries)) {
    return(invisible(NULL))
  }

  pchs <- file.path(entries, "model_pch.hpp.gch")
  entries <- entries[file.exists(pchs)]
  pchs <- pchs[file.exists(pchs)]
  if (length(entries) <= keep) {
    return(invisible(NULL))
  }

  mtime <- file.info(pchs)$mtime
  old_entries <- entries[order(mtime, na.last = FALSE)][seq_len(length(entries) - keep)]
  unlink(old_entries, recursive = TRUE, force = TRUE)
  invisible(NULL)
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
  pch_cppflags <- paste(cppflags, collapse = " ")
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

  fingerprint <- .stanr_hash(
    list(
      stanr = as.character(utils::packageVersion("stanr")),
      r = R.version$version.string,
      arch = R.version$arch,
      compiler = compiler,
      # The CXX20* family, matching what the PCH recipe (`inst/pch.mk`)
      # and the model TU compile both resolve to.
      makeconf = vapply(
        c("CXX20", "CXX20STD", "CXX20FLAGS", "CXX20PICFLAGS", "CPPFLAGS"),
        .stanr_r_config,
        character(1)
      ),
      cppflags = pch_cppflags,
      opt_flags = pch_build_cxxflags,
      header = unname(tools::md5sum(header))
    )
  )
  cache_root <- getOption(
    "stanr_pch_dir",
    file.path(tools::R_user_dir("stanr", "cache"), "pch")
  )
  cache_dir <- file.path(cache_root, fingerprint)
  cache_header <- file.path(cache_dir, "model_pch.hpp")
  pch <- paste0(cache_header, ".gch")

  if (rebuild && file.exists(pch)) {
    unlink(pch)
  }

  if (!file.exists(pch)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (compiler_type == "gcc" && !file.exists(cache_header)) {
      # Windows lacks usable symlinks; copy instead. Header md5 is in the
      # fingerprint, so edits get a fresh cache dir.
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
    message("[stanr] Compiling precompiled model header...")
    # Pass the same makefiles vector as `R CMD SHLIB` via -f so the PCH is
    # built with the same CXX20* flags (e.g. -march=native) as the model TU.
    output <- tryCatch(
      .stanr_system2(
        make,
        c(
          unlist(lapply(.stanr_makefiles(), function(f) c("-f", shQuote(f)))),
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
      return("")
    }
  }

  # Use the PCH's modification time as an LRU timestamp.  This is deliberately
  # best-effort: a failure to update it must not prevent model compilation.
  try(Sys.setFileTime(pch, Sys.time()), silent = TRUE)
  .stanr_prune_pch_cache(cache_root)

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
  is_pch_diagnostic <- function(message) {
    grepl(
      "PCH file|precompiled header|\\.hpp\\.gch|\\.gch",
      message,
      ignore.case = TRUE
    )
  }

  tryCatch(
    compile_fn(cppflags),
    error = function(error) {
      if (!pch_enabled) {
        stop(error)
      }
      msg <- conditionMessage(error)
      if (!is_pch_diagnostic(msg)) {
        stop(error)
      }

      message(
        "[stanr] Compile failed; rebuilding precompiled model header and retrying..."
      )
      pch_flags <- .stanr_pch_flags(base_cppflags, verbose, rebuild = TRUE)
      if (!nzchar(pch_flags)) {
        # The failed attempt's diagnostic was about the PCH, not the model.
        # Continue without a PCH instead of exposing that stale-cache error.
        return(compile_fn(base_cppflags))
      }
      tryCatch(
        compile_fn(paste(pch_flags, base_cppflags)),
        error = function(retry_error) {
          if (is_pch_diagnostic(conditionMessage(retry_error))) {
            # A second PCH-specific failure should likewise fall back to a
            # regular model compile. Any model diagnostic from that compile
            # remains visible to the user.
            return(compile_fn(base_cppflags))
          }
          stop(retry_error)
        }
      )
    }
  )
}
