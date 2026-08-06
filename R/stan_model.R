# Applies an ordered list of `list(name, op, value)` assignments (as
# returned by `.stanr_parse_cpp_options()`) to a named `base` Makevars
# vector, in order: an `"="` assignment replaces `base[[name]]` outright, and
# a `"+="` assignment appends to it (space-joined) -- or, if `name` isn't yet
# in `base`, is equivalent to `"="`, matching how an unset Makefile variable
# behaves under `+=`. Applying assignments here (rather than folding them
# into a single flat named vector ahead of time and handing that to
# `withr::with_makevars()`) also sidesteps a `withr` pitfall: it silently
# drops all but one same-named entry when the caller already has a personal
# Makevars file on disk, so a vector with two "CXXFLAGS" entries wouldn't
# reliably apply both.
.stanr_apply_makevars <- function(base, assignments) {
  for (a in assignments) {
    if (identical(a$op, "+=") && a$name %in% names(base)) {
      base[[a$name]] <- paste(base[[a$name]], a$value)
    } else {
      base[a$name] <- a$value
    }
  }
  base
}

# Shared by both compile paths (model TU and functions TU). `+=` appends
# these after Makeconf's own CXXFLAGS/CXX20FLAGS (which are brought in ahead
# of the package Makevars file R writes for this call), so our -O3 -g0 wins
# the last-flag-wins compiler precedence instead of being silently
# overridden by Makeconf's -g -O2. USE_CXX20/PKG_CPPFLAGS/PKG_LIBS are not
# set by Makeconf, so `+=` on them is equivalent to `=`.
.stanr_sourcecpp <- function(
  cpp_file,
  env,
  cppflags,
  libs,
  extra_assignments,
  rebuild,
  cache_dir,
  verbose
) {
  withr::with_envvar(
    c(USE_CXX20 = "1"),
    withr::with_makevars(
      .stanr_apply_makevars(
        c(
          PKG_CPPFLAGS = cppflags,
          PKG_LIBS = libs,
          CXXFLAGS = .stanr_opt_flags(),
          CXX20FLAGS = .stanr_opt_flags()
        ),
        extra_assignments
      ),
      assignment = "+=",
      Rcpp::sourceCpp(
        file = cpp_file,
        env = env,
        rebuild = rebuild,
        cacheDir = cache_dir,
        verbose = verbose
      )
    )
  )
}

.stanr_tbb_libs <- function() {
  tbb_libs <- utils::tail(
    utils::capture.output(RcppParallel::RcppParallelLibs()),
    1
  )
  if (!length(tbb_libs)) {
    tbb_libs <- ""
  }
  if (
    .Platform$OS.type == "windows" &&
      utils::packageVersion("RcppParallel") >= '6.0.0'
  ) {
    tbb_libs <- "-ltbb12 -ltbbmalloc"
  }
  tbb_libs
}

# Shared by every compile path (model TU, functions-only TU): both need
# RcppEigen/BH on top of base R's compiler toolchain.
.stanr_require_compile_packages <- function() {
  for (pkg in c("RcppEigen", "BH")) {
    if (!nzchar(system.file(package = pkg))) {
      stop(
        "Package `",
        pkg,
        "` must be installed to compile Stan code.",
        call. = FALSE
      )
    }
  }
}

# Non-OpenCL cppflags shared by every compile path; a model compile appends
# OpenCL-specific flags on top of this when `use_opencl = TRUE`.
.stanr_base_cppflags <- function() {
  paste(
    paste0(
      "-I",
      shQuote(system.file("include", package = "stanr", mustWork = TRUE))
    ),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS"
  )
}

# A `cpp_options` assignment to a compiler-facing Makevars variable changes
# flags the PCH was not built with; GCC then quietly falls back to the plain
# header while clang can reject the PCH outright (e.g. a `-std=` or `-D`
# override), so PCH is skipped for such compiles.
.stanr_cpp_options_block_pch <- function(assignments) {
  compiler_vars <- c(
    "CPPFLAGS",
    "PKG_CPPFLAGS",
    "CXX",
    "CXXFLAGS",
    "CXXPICFLAGS",
    "CXX20",
    "CXX20STD",
    "CXX20FLAGS",
    "CXX20PICFLAGS"
  )
  any(vapply(
    assignments,
    function(a) a$name %in% compiler_vars,
    logical(1)
  ))
}

# Stable sort by name: reordering unrelated `cpp_options` entries must not
# change the hash; reordering two assignments to the *same* name must.
.stanr_cpp_options_hash_component <- function(assignments) {
  if (length(assignments)) {
    assignment_names <- vapply(assignments, `[[`, character(1), "name")
    ord <- order(assignment_names)
    vapply(
      assignments[ord],
      function(a) paste(a$name, a$op, a$value),
      character(1)
    )
  } else {
    character()
  }
}

#' Normalized paths of every shared library currently mapped into this session.
#'
#' @noRd
.stanr_loaded_dll_paths <- function() {
  vapply(
    getLoadedDLLs(),
    function(dll) {
      normalizePath(dll[["path"]], winslash = "/", mustWork = FALSE)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# A completed `sourceCpp()` build is always compiled into a fresh, unique
# scratch directory (`tempfile()`), never a shared/reused one -- so a forced
# recompile can never overwrite a `.so` a live fit still holds pointers into,
# and there is nothing to redirect/alias the way the old central-cache
# registry had to.
.stanr_build_scratch_dir <- function() {
  dir <- tempfile("stanr_build_")
  dir.create(dir)
  dir
}

#' Persistent single-file cache path for a translation unit's compiled build.
#'
#' Lives next to `stan_file` when the model has one -- named only from its own
#' basename plus `suffix` (never the hash), so there is exactly one cache file
#' per source file and an edit invalidates it by content hash rather than by
#' filename -- or under `tempdir()` (named by `key_hash`, since there is no
#' source path to derive a stable name from and several models may coexist in
#' one session) when the model was created from a `code` string, or when
#' `stan_file`'s directory turns out not to be writable.
#'
#' @noRd
.stanr_build_cache_file <- function(stan_file, key_hash, suffix = "") {
  if (length(stan_file)) {
    dir <- dirname(stan_file)
    if (file.access(dir, 2) == 0) {
      stem <- tools::file_path_sans_ext(basename(stan_file))
      return(file.path(dir, paste0(".", stem, suffix, ".stanrc")))
    }
  }
  file.path(tempdir(), paste0("stanr_", key_hash, suffix, ".stanrc"))
}

#' Archive a completed `sourceCpp()` build into a single relocatable file.
#'
#' `Rcpp::sourceCpp()`'s generated R wrapper hardcodes the absolute path it
#' `dyn.load()`s the shared library from (verified by inspecting a build:
#' `` `.sourceCpp_1_DLLInfo` <- dyn.load('/abs/path/.../sourceCpp_2.so')
#' ``), so that literal is replaced with a `{{STANR_SO}}` placeholder before
#' archiving -- filled back in with wherever the archive gets extracted to at
#' `.stanr_restore_build_cache()` time, which is never the same path twice.
#' The `.so` path is read out of that same `dyn.load()` literal rather than
#' globbed from `build_dir` -- by the time `Rcpp::sourceCpp()` has returned,
#' it has already `dyn.load()`ed the library from that exact path itself (how
#' `env` got populated), so parsing its own text is both sufficient and, on
#' filesystems where a directory listing can briefly lag a `stat()` of a file
#' that was just written, more reliable than re-discovering it via
#' `list.files()`.
#'
#' @param build_dir The `buildDirectory` a `Rcpp::sourceCpp()` call returned.
#' @return Nothing useful; failures are swallowed (this only means the build
#'   can't be cached to disk, not that the compile itself failed).
#' @noRd
.stanr_write_build_cache <- function(
  cache_file,
  key_hash,
  build_dir,
  cpp_file
) {
  tryCatch(
    {
      wrapper_file <- file.path(build_dir, paste0(basename(cpp_file), ".R"))
      if (!file.exists(wrapper_file)) {
        return(invisible(NULL))
      }
      wrapper_text <- paste(
        readLines(wrapper_file, warn = FALSE),
        collapse = "\n"
      )
      # Match dyn.load() with either single or double quotes -- Rcpp may
      # use different quote styles on different platforms.
      so_match <- regmatches(
        wrapper_text,
        regexpr("dyn\\.load\\(['\"][^'\"]+['\"]\\)", wrapper_text)
      )
      if (!length(so_match)) {
        return(invisible(NULL))
      }
      so_file <- sub("^dyn\\.load\\(['\"]([^'\"]+)['\"]\\)$", "\\1", so_match)
      if (!file.exists(so_file)) {
        return(invisible(NULL))
      }
      templated <- gsub(so_file, "{{STANR_SO}}", wrapper_text, fixed = TRUE)
      if (identical(templated, wrapper_text)) {
        # The wrapper's dyn.load() path didn't look as expected -- skip
        # caching rather than archive something that can't be restored.
        return(invisible(NULL))
      }

      stage_dir <- tempfile("stanr_pack_")
      dir.create(stage_dir)
      on.exit(unlink(stage_dir, recursive = TRUE))
      writeLines(key_hash, file.path(stage_dir, "HASH"))
      writeLines(templated, file.path(stage_dir, "wrapper.R"))
      file.copy(
        so_file,
        file.path(stage_dir, paste0("lib.", .Platform$dynlib.ext))
      )

      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      tmp_out <- paste0(cache_file, ".tmp", Sys.getpid())
      withr::with_dir(
        stage_dir,
        utils::tar(
          tmp_out,
          files = list.files("."),
          compression = "gzip",
          tar = "internal"
        )
      )
      file.rename(tmp_out, cache_file)
      invisible(NULL)
    },
    error = function(e) invisible(NULL)
  )
}

#' Restore a previously archived build into a fresh location and bind its
#' exports into `env`.
#'
#' @return `TRUE` on a hash-matching, structurally valid restore (`env` is
#'   now populated and the library is loaded), `FALSE` otherwise (no cache
#'   file, a stale hash, or a malformed archive) -- always a cache miss to
#'   fall through to, never an error.
#' @noRd
.stanr_restore_build_cache <- function(cache_file, key_hash, env) {
  if (!file.exists(cache_file)) {
    return(FALSE)
  }
  extract_dir <- tempfile("stanr_load_")
  dir.create(extract_dir)
  ok <- tryCatch(
    {
      utils::untar(cache_file, exdir = extract_dir, tar = "internal")
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  hash_file <- file.path(extract_dir, "HASH")
  wrapper_file <- file.path(extract_dir, "wrapper.R")
  so_file <- file.path(extract_dir, paste0("lib.", .Platform$dynlib.ext))
  if (
    !ok ||
      !file.exists(hash_file) ||
      !file.exists(wrapper_file) ||
      !file.exists(so_file) ||
      !identical(readLines(hash_file, warn = FALSE), key_hash)
  ) {
    unlink(extract_dir, recursive = TRUE)
    return(FALSE)
  }
  wrapper_text <- paste(readLines(wrapper_file, warn = FALSE), collapse = "\n")
  wrapper_text <- gsub("{{STANR_SO}}", so_file, wrapper_text, fixed = TRUE)
  # Write to a file and source it rather than using textConnection(), which
  # would interpret backslashes in Windows paths (e.g., C:\Users\...) as
  # escape sequences and trigger parse errors like '\U' hex digit errors.
  writeLines(wrapper_text, wrapper_file)
  source(wrapper_file, local = env)
  TRUE
}

#' Per-session memo of the `env` already compiled/restored for a given hash,
#' so repeat compiles of an unchanged model within one session reuse the same
#' loaded library instead of `dyn.load()`ing a fresh copy from the on-disk
#' archive every time.
#'
#' @noRd
.stanr_env_memo <- function() {
  if (is.null(.stanr_memo$compiled_envs)) {
    .stanr_memo$compiled_envs <- new.env(parent = emptyenv())
  }
  .stanr_memo$compiled_envs
}

# Kept in sync by hand with the `// [[Rcpp::export]]` function names in
# inst/stan_model.cpp -- reserved so a combined-TU expose can't silently
# shadow one of them.
.stanr_model_support_exports <- c(
  "new_model",
  "run_model",
  "constrained_param_names",
  "new_base_rng",
  "model_num_upars",
  "model_param_metadata",
  "model_constrained_names",
  "model_unconstrained_names",
  "model_log_prob",
  "model_grad_log_prob",
  "model_hessian",
  "model_unconstrain",
  "model_unconstrain_matrix",
  "model_constrain",
  "model_constrain_matrix",
  "model_constrain_variables",
  "model_variable_skeleton",
  "select_opencl_device"
)

.compile_stan_model_environment <- function(
  code,
  model_name,
  stan_file = NULL,
  external_cpp = NULL,
  verbose = FALSE,
  precompiled_headers = TRUE,
  force_recompile = FALSE,
  use_opencl = FALSE,
  cpp_options = list(),
  standalone_functions = FALSE
) {
  .stanr_require_compile_packages()

  # stanc() is the expensive step, so its inputs are hashed (with the same
  # discriminating power as hashing its output) to let a warm cache skip it.
  model_support <- readLines(
    system.file("stan_model.cpp", package = "stanr", mustWork = TRUE)
  )
  # external_cpp is hashed by content, not path: stanc splices file contents
  # into the generated C++, so the hash must depend on content, not location.
  external_cpp_contents <- .stanr_external_cpp_contents(external_cpp)
  # OPENCL_LIBS is consumed for link flags separately from the other
  # `cpp_options` assignments (avoiding double-applying it), and pinned to
  # `""` when OpenCL is off so it can't perturb the cache key.
  cpp_option_assignments <- .stanr_parse_cpp_options(cpp_options)
  is_opencl_libs <- vapply(
    cpp_option_assignments,
    function(a) identical(a$name, "OPENCL_LIBS"),
    logical(1)
  )
  opencl_assignments <- cpp_option_assignments[is_opencl_libs]
  extra_assignments <- cpp_option_assignments[!is_opencl_libs]
  opencl_libs <- if (use_opencl) {
    opencl_default <- if (Sys.info()[["sysname"]] == "Darwin") {
      "-framework OpenCL"
    } else {
      "-lOpenCL"
    }
    .stanr_apply_makevars(
      c(OPENCL_LIBS = opencl_default),
      opencl_assignments
    )[["OPENCL_LIBS"]]
  } else {
    ""
  }
  # R.version$platform and compiler identity are included because
  # Rcpp::sourceCpp()'s own cache can't detect an in-place toolchain
  # upgrade, so a toolchain upgrade must still produce a new cache entry.
  model_hash <- digest::digest(
    c(
      code,
      as.character(standalone_functions),
      external_cpp_contents,
      model_support,
      as.character(utils::packageVersion("stanr")),
      .stanr_stan_version(),
      R.version$platform,
      .stanr_compiler_identity(),
      as.character(use_opencl),
      opencl_libs,
      .stanr_cpp_options_hash_component(extra_assignments)
    ),
    algo = "xxhash64"
  )

  memo <- .stanr_env_memo()
  if (!force_recompile && !is.null(memo[[model_hash]])) {
    return(memo[[model_hash]])
  }

  cache_file <- .stanr_build_cache_file(stan_file, model_hash)
  if (!force_recompile) {
    env <- new.env()
    if (.stanr_restore_build_cache(cache_file, model_hash, env)) {
      memo[[model_hash]] <- env
      return(env)
    }
  }

  build_dir <- .stanr_build_scratch_dir()
  cpp_file <- file.path(build_dir, paste0("stan_", model_hash, ".cpp"))
  if (verbose) {
    message("[stanr] Compiling '", model_name, "'...")
  }
  cpp_code <- stanc(
    code,
    external_cpp = external_cpp,
    use_opencl = use_opencl
  )
  if (standalone_functions) {
    # external_cpp = NULL: its content is already in `cpp_code` above (the
    # model stanc call already prepended it) -- prepending it a second
    # time would duplicate it verbatim in the file. allow_undefined is
    # therefore set explicitly rather than left to stanc()'s own
    # external_cpp-implies-allow-undefined inference, which only fires
    # when `external_cpp` is actually passed.
    functions_out <- stanc(
      code,
      standalone_functions = TRUE,
      use_opencl = use_opencl,
      allow_undefined = length(external_cpp) > 0
    )
    processed <- .stanr_process_standalone_cpp(
      functions_out,
      c(
        .stanr_model_support_exports,
        "stanr_exposed_functions",
        "stanr_rng_set_seed"
      )
    )
    wrapper_section <- processed$wrapper_section
    if (length(external_cpp) > 0) {
      # stanc's standalone-functions codegen always has a wrapper call
      # `model_namespace::<fn>(...)`, assuming <fn> is defined inside
      # model_namespace -- true for ordinary Stan functions, but not for
      # one implemented via `external_cpp`: that implementation is
      # prepended by the *first* stanc() call above *before*
      # `namespace model_namespace {` opens (i.e. at file/global scope),
      # so `model_namespace::<fn>` doesn't exist there and would fail to
      # link. Detected by comparing each exposed function's first
      # occurrence in `cpp_code` against where model_namespace opens: an
      # external_cpp implementation's first (and only) occurrence is the
      # prepended definition, strictly before that point, while an
      # ordinary Stan function's every occurrence is inside
      # model_namespace. Unqualifying the call is safe either way: from
      # within `model_namespace`, the model's own generated code already
      # calls these external functions unqualified (relying on ordinary
      # scope fallback to file/global scope) -- see `cpp_code` itself --
      # and this wrapper lives at file scope too, so the same unqualified
      # call resolves identically.
      namespace_pos <- regexpr(
        "namespace model_namespace",
        cpp_code,
        fixed = TRUE
      )[[1]]
      for (fn_name in processed$functions$name) {
        first_pos <- regexpr(paste0("\\b", fn_name, "\\b"), cpp_code)[[1]]
        if (first_pos > 0 && first_pos < namespace_pos) {
          wrapper_section <- gsub(
            paste0("model_namespace::", fn_name, "("),
            paste0(fn_name, "("),
            wrapper_section,
            fixed = TRUE
          )
        }
      }
    }
    writeLines(c(cpp_code, model_support, wrapper_section), cpp_file)
  } else {
    writeLines(c(cpp_code, model_support), cpp_file)
  }

  cppflags <- .stanr_base_cppflags()
  if (use_opencl) {
    # Platform/device are pinned to 0/0 at compile time only to satisfy
    # opencl_context.hpp's `#error`-style guards -- they don't constrain
    # which device is used at runtime; real selection happens later, via
    # `select_opencl_device()`, so one cached binary serves any device.
    cppflags <- paste(
      cppflags,
      "-DSTAN_OPENCL -DOPENCL_PLATFORM_ID=0 -DOPENCL_DEVICE_ID=0",
      "-DCL_HPP_TARGET_OPENCL_VERSION=120 -DCL_HPP_MINIMUM_OPENCL_VERSION=120",
      "-DCL_HPP_ENABLE_EXCEPTIONS -DINTEGRATED_OPENCL=0 -Wno-ignored-attributes"
    )
  }
  base_cppflags <- cppflags
  pch_enabled <- FALSE
  if (
    precompiled_headers &&
      length(external_cpp) == 0 &&
      !.stanr_cpp_options_block_pch(extra_assignments)
  ) {
    pch_flags <- .stanr_pch_flags(base_cppflags, verbose)
    pch_enabled <- nzchar(pch_flags)
    cppflags <- paste(pch_flags, base_cppflags)
  }

  env <- new.env()
  runtime_archive <- system.file(
    "lib",
    Sys.getenv("R_ARCH"),
    "libstanr_runner.a",
    package = "stanr",
    mustWork = TRUE
  )

  tbb_libs <- .stanr_tbb_libs()

  # libstanr_runner.a is always compiled without STAN_OPENCL, while an
  # OpenCL-enabled model TU defines it; safe only as long as services touch
  # the model solely through the stan::model::model_base virtual interface.
  libs <- paste(shQuote(runtime_archive), tbb_libs)
  if (use_opencl) {
    libs <- paste(libs, opencl_libs)
  }

  compile_model <- function(compilation_cppflags) {
    .stanr_sourcecpp(
      cpp_file = cpp_file,
      env = env,
      cppflags = compilation_cppflags,
      libs = libs,
      extra_assignments = extra_assignments,
      rebuild = FALSE,
      cache_dir = build_dir,
      verbose = verbose
    )
  }

  result <- .stanr_compile_with_pch_retry(
    compile_model,
    cppflags,
    base_cppflags,
    pch_enabled,
    verbose
  )
  .stanr_write_build_cache(
    cache_file,
    model_hash,
    result$buildDirectory,
    cpp_file
  )

  memo[[model_hash]] <- env
  env
}

#' Create a Stan model object
#'
#' @description Create a new [`StanModel`] object from a Stan program file or
#'   from Stan code as a string. The [`StanModel`] object stores the Stan
#'   program source and compiled model, and provides methods for fitting the
#'   model using Stan's inference algorithms.
#'
#'   See the `compile` argument for control over whether and how compilation
#'   happens.
#'
#' @param stan_file (string) The path to a `.stan` file containing a Stan
#'   program. If `stan_file` is not specified then `code` must be specified.
#' @param code (string) A Stan program as a single string. If `code` is not
#'   specified then `stan_file` must be specified.
#' @param compile (logical) Should the model be compiled? The default is
#'   `TRUE`. If `FALSE` compilation can be done later via the
#'   [`$compile()`][model-method-compile] method.
#' @param compile_standalone (logical) Should the Stan program's `functions`
#'   block be exposed as part of this compilation? The default is `FALSE`.
#'   When `TRUE`, `$functions` is already populated when `stan_model()`
#'   returns -- equivalent to `FALSE` here plus calling
#'   [`$expose_stan_functions()`][model-method-expose-stan-functions]
#'   immediately after, but without a second compile. See
#'   [`$expose_stan_functions()`][model-method-expose-stan-functions] for
#'   what gets exposed and how.
#' @param model_name (string) The name to use for the model. If `NULL` (the
#'   default), the model name is derived from the Stan file name (if provided)
#'   or set to `"model"`.
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives.
#' @param user_header (string) Not yet supported. Use `external_cpp` instead.
#' @param cpp_options (list) C++ compilation options, merged into the
#'   Makevars flags used to compile the model. Each element is either:
#'   * A named element, e.g. `list(CXX = "g++")`. This *overrides* any value
#'     computed internally for that name (e.g. `CXXFLAGS`), replacing it
#'     rather than adding to it.
#'   * An unnamed string of the form `"<NAME> = <value>"`, e.g.
#'     `"CXX = g++"`. Equivalent to the named form above -- also overrides.
#'   * An unnamed string of the form `"<NAME> += <value>"`, e.g.
#'     `"CXXFLAGS += -Wno-psabi"`. *Appends* `<value>` to any existing value
#'     for `<NAME>` instead of replacing it (space-separated), matching how
#'     `+=` behaves in a Makevars file.
#'
#'   Entries are applied in list order, so the same name may appear more
#'   than once, e.g. `list(CXXFLAGS = "-O3", "CXXFLAGS += -Wall")` first
#'   overrides `CXXFLAGS`, then appends to it. `OPENCL_LIBS` is a special
#'   name: overriding or appending to it changes the OpenCL link flags used
#'   when `use_opencl = TRUE` -- see `use_opencl` below -- rather than a
#'   real Makevars variable.
#' @param stanc_options (list) Stan-to-C++ transpiler options. Not yet supported.
#' @param force_recompile (logical) Should the model be recompiled even if it
#'   has not been modified? The default is `FALSE`, but can be set via the
#'   `stanr_force_recompile` option.
#' @param precompiled_headers (logical) Should precompiled headers be used to
#'   speed up compilation? The default is `TRUE`. Automatically disabled when
#'   `cpp_options` overrides compiler flags (e.g. `CXXFLAGS`), since the
#'   precompiled header is not built with those flags.
#' @param quiet (logical) Should verbose output from compilation be suppressed?
#'   The default is `TRUE`.
#' @param external_cpp (character vector) Paths to C++ files to prepend to the
#'   generated model code. Useful for defining custom functions.
#' @param use_opencl (logical) Should the model be compiled with OpenCL
#'   support? The default is `FALSE`. When `TRUE`, `stanc` generates
#'   OpenCL-accelerated code for the functions that support it (most notably
#'   `bernoulli_logit_glm` and other GLM likelihoods), and the compiled model
#'   can run its computation on an OpenCL device. Which platform/device is
#'   used at runtime is controlled by the `opencl_ids` argument of the fit
#'   methods (e.g. [`$sample()`][model-method-sample]), not by this argument;
#'   `opencl_ids = NULL` (the default there) means `select_opencl_device()`
#'   is simply never called, so the platform/device baked in at compile time
#'   (0/0) is used. The library used to link OpenCL support can be overridden
#'   via `cpp_options = list(OPENCL_LIBS = ...)` -- the default is
#'   `"-framework OpenCL"` on macOS and `"-lOpenCL"` elsewhere. This override
#'   is particularly relevant on Apple Silicon: the system OpenCL framework
#'   exposes no double-precision (`fp64`) device there, and Stan requires
#'   doubles, so real OpenCL testing on Apple Silicon requires an
#'   OpenCL implementation with `fp64` support (e.g. POCL) and pointing
#'   `OPENCL_LIBS` at its ICD loader instead of the system framework.
#'
#' @return A [`StanModel`] object.
#'
#'   The compiled model is cached persistently on disk as a single file next
#'   to `stan_file` (named after it, e.g. `mymodel.stan` gets a sibling
#'   `.mymodel.stanrc`), or under [tempdir()] when the model was created from
#'   a `code` string, or when `stan_file`'s directory isn't writable. The
#'   cache file embeds a hash of the generated C++, so it's reused across R
#'   sessions as long as the Stan program, `include_paths`, `external_cpp`,
#'   and installed stanr/Stan versions are unchanged, and is silently
#'   recompiled and overwritten otherwise -- there is no separate cache-clearing
#'   step. On a cache hit, the Stan-to-C++ transpiler is skipped entirely, so
#'   any transpiler warnings (e.g. from pedantic mode or deprecated syntax)
#'   are only surfaced the first time a given model is compiled, not on
#'   subsequent cache hits.
#'
#' @seealso [`StanModel`], [`$compile()`][model-method-compile],
#'   [`$sample()`][model-method-sample]
#'
#' @examples
#' \dontrun{
#' # Create a StanModel from Stan code
#' mod <- stan_model(
#'   code = "
#'     parameters {
#'       real theta;
#'     }
#'     model {
#'       theta ~ normal(0, 1);
#'     }
#'   "
#' )
#' mod$model_name()
#' mod$variables()
#'
#' # Run MCMC sampling
#' fit <- mod$sample(data = list(), chains = 2)
#' fit$summary()
#' }
#'
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
  force_recompile = getOption("stanr_force_recompile", FALSE),
  precompiled_headers = TRUE,
  quiet = TRUE,
  external_cpp = NULL,
  use_opencl = FALSE,
  compile_standalone = FALSE
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
    external_cpp = external_cpp,
    use_opencl = use_opencl,
    compile_standalone = compile_standalone
  )
}
