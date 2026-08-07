# Applies `list(name, op, value)` assignments to a Makevars vector ("="
# replaces, "+=" appends). Folded one at a time rather than pre-merged into
# a flat vector: `withr::with_makevars()` drops duplicate names if the
# caller has a personal Makevars file on disk.
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

# Appends a plain C entry point returning the cache key, so a compiled .so
# is self-describing (no sidecar file). Reachable only via
# getNativeSymbolInfo("stanr_build_key", dll).
.stanr_append_build_key <- function(cpp_file, key_hash) {
  cat(
    "\nextern \"C\" SEXP stanr_build_key(void) {\n",
    "  return Rf_ScalarString(Rf_mkChar(\"", key_hash, "\"));\n",
    "}\n",
    file = cpp_file,
    append = TRUE,
    sep = ""
  )
}

# R CMD SHLIB on the model TU, run from its own directory with a
# relative filename (mirroring sourceCpp()): GNU make chokes on paths with
# spaces (e.g. a Windows user profile), which an absolute path risks.
.stanr_compile_shlib <- function(cpp_file, verbose) {
  lib_name <- paste0(
    tools::file_path_sans_ext(basename(cpp_file)),
    .Platform$dynlib.ext
  )
  output <- withr::with_dir(
    dirname(cpp_file),
    tryCatch(
      .stanr_rcmd(
        c("SHLIB", "-o", shQuote(lib_name), shQuote(basename(cpp_file))),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) conditionMessage(e)
    )
  )
  lib_file <- file.path(dirname(cpp_file), lib_name)
  if (!file.exists(lib_file)) {
    # Carries the compiler's actual diagnostics, unlike sourceCpp()'s generic error.
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  if (verbose) {
    cat(output, sep = "\n")
  }
  lib_file
}

# Shared by both compile paths. `assignment = "+="` appends after
# Makeconf's own CXX20FLAGS, so -O3 -g0 wins last-flag-wins precedence over
# Makeconf's -g -O2. With `USE_CXX20=1`, `R CMD SHLIB` uses CXX20FLAGS (not
# CXXFLAGS), so that's the only flag override needed.
.stanr_compile <- function(
  cpp_file,
  cppflags,
  libs,
  extra_assignments,
  verbose
) {
  withr::with_envvar(
    c(USE_CXX20 = "1"),
    withr::with_makevars(
      .stanr_apply_makevars(
        c(
          PKG_CPPFLAGS = paste(
            c(cppflags, .stanr_dependency_cppflags()),
            collapse = " "
          ),
          PKG_LIBS = libs,
          CXX20FLAGS = .stanr_opt_flags()
        ),
        extra_assignments
      ),
      assignment = "+=",
      .stanr_compile_shlib(cpp_file, verbose)
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
      utils::packageVersion("RcppParallel") >= '6.0.0' &&
      utils::packageVersion("RcppParallel") < '6.2.0'
  ) {
    tbb_libs <- "-ltbb12 -ltbbmalloc"
  }
  tbb_libs
}

# Shared by every compile path (model TU, functions-only TU): both need
# RcppEigen/BH on top of base R's compiler toolchain. Rcpp is loaded too:
# the model TU uses Rcpp types (XPtr, List, ...) whose callables are only
# registered once the Rcpp namespace is loaded.
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
  loadNamespace("Rcpp")
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
# flags the PCH wasn't built with -- GCC quietly falls back to the plain
# header, clang can reject the PCH outright -- so PCH is skipped for those.
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

# Normalized paths of every shared library currently mapped into this session.
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

# Always a fresh, unique scratch dir (`tempfile()`), never shared/reused: a
# forced recompile can then never overwrite a `.so` a live fit still holds
# pointers into.
.stanr_build_scratch_dir <- function() {
  dir <- tempfile("stanr_build_")
  dir.create(dir)
  dir
}

# Persistent cache path for a translation unit's compiled build. Lives next
# to `stan_file`, named from its basename + `suffix` only (never the hash) --
# so there's one cache file per source, invalidated by content hash rather
# than filename -- or under `tempdir()` (named by `key_hash`) when there's no
# source path or its directory isn't writable. The cache file *is* the
# compiled library; its own hash lives inside it (see
# `.stanr_append_build_key()`).
.stanr_build_cache_file <- function(stan_file, key_hash, suffix = "") {
  if (length(stan_file)) {
    dir <- dirname(stan_file)
    if (file.access(dir, 2) == 0) {
      stem <- tools::file_path_sans_ext(basename(stan_file))
      return(file.path(dir, paste0(".", stem, suffix, .Platform$dynlib.ext)))
    }
  }
  file.path(tempdir(), paste0("stanr_", key_hash, suffix, .Platform$dynlib.ext))
}

# Copies a completed build to the persistent cache file. Failures are
# swallowed: caching failure isn't a compile failure.
.stanr_write_build_cache <- function(cache_file, lib_file) {
  tryCatch(
    {
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      tmp_out <- paste0(cache_file, ".tmp", Sys.getpid())
      file.copy(lib_file, tmp_out, overwrite = TRUE)
      # file.rename() will not replace an existing file on Windows.
      unlink(cache_file)
      file.rename(tmp_out, cache_file)
      invisible(NULL)
    },
    error = function(e) invisible(NULL)
  )
}

# dyn.load()s `so_file`, reads the embedded build key (see
# `.stanr_append_build_key()`), and binds its exports into `env`. Any
# error propagates -- `.stanr_restore_build_cache()` treats it as a miss.
.stanr_load_build <- function(so_file, env) {
  dll <- dyn.load(so_file)
  for (name in .stanr_model_support_exports) {
    sym <- getNativeSymbolInfo(name, dll)
    env[[name]] <- local({
      addr <- sym$address
      function(...) .Call(addr, ...)
    })
  }
  .Call(getNativeSymbolInfo("stanr_build_key", dll)$address)
}

# Restores the cached library into a fresh location and binds its exports.
# Never dyn.load()s `cache_file` directly -- each attempt gets a private
# copy, so a later recompile that overwrites `cache_file` never touches a
# library a live fit still holds pointers into (Windows locks a loaded DLL
# against that; POSIX is unsafe too). Returns TRUE on a hash-matching
# restore, FALSE on any kind of miss -- never errors.
.stanr_restore_build_cache <- function(cache_file, key_hash, env) {
  if (!file.exists(cache_file)) {
    return(FALSE)
  }
  so_copy <- file.path(
    .stanr_build_scratch_dir(),
    paste0("lib", .Platform$dynlib.ext)
  )
  if (!isTRUE(file.copy(cache_file, so_copy))) {
    return(FALSE)
  }
  hash <- tryCatch(.stanr_load_build(so_copy, env), error = function(e) NULL)
  if (identical(hash, key_hash)) {
    return(TRUE)
  }
  try(dyn.unload(so_copy), silent = TRUE)
  FALSE
}

# Per-session memo of the `env` already compiled/restored for a given hash,
# so repeat compiles of an unchanged model reuse the same loaded library
# instead of `dyn.load()`ing a fresh copy from the on-disk archive.
.stanr_env_memo <- function() {
  if (is.null(.stanr_memo$compiled_envs)) {
    .stanr_memo$compiled_envs <- new.env(parent = emptyenv())
  }
  .stanr_memo$compiled_envs
}

# Kept in sync by hand with the `extern "C"` function names in
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
  cpp_options = list()
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
  cpp_option_assignments <- .stanr_parse_cpp_options(cpp_options)
  extra_assignments <- cpp_option_assignments
  # R.version$platform and compiler identity are included so an in-place
  # toolchain upgrade still produces a new cache entry, rather than reusing
  # a .so built by a compiler no longer on this machine.
  model_hash <- digest::digest(
    c(
      code,
      external_cpp_contents,
      model_support,
      as.character(utils::packageVersion("stanr")),
      .stanr_stan_version(),
      R.version$platform,
      .stanr_compiler_identity(),
      as.character(use_opencl),
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
  writeLines(c(cpp_code, model_support), cpp_file)

  .stanr_append_build_key(cpp_file, model_hash)

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
    opencl_default <- if (Sys.info()[["sysname"]] == "Darwin") {
      "-framework OpenCL"
    } else {
      "-lOpenCL"
    }
    libs <- paste(libs, opencl_default)
  }

  compile_model <- function(compilation_cppflags) {
    .stanr_compile(
      cpp_file = cpp_file,
      cppflags = compilation_cppflags,
      libs = libs,
      extra_assignments = extra_assignments,
      verbose = verbose
    )
  }

  lib_file <- .stanr_compile_with_pch_retry(
    compile_model,
    cppflags,
    base_cppflags,
    pch_enabled,
    verbose
  )
  .stanr_load_build(lib_file, env)
  .stanr_write_build_cache(cache_file, lib_file)

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
#'   overrides `CXXFLAGS`, then appends to it.
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
#'   (0/0) is used. The default OpenCL link flags are `"-framework OpenCL"`
#'   on macOS and `"-lOpenCL"` elsewhere.
#'
#' @return A [`StanModel`] object.
#'
#'   The compiled model is cached persistently on disk as the compiled shared
#'   library itself, next to `stan_file` (named after it, e.g. `mymodel.stan`
#'   gets a sibling `.mymodel.so`/`.mymodel.dll`), or under [tempdir()] when
#'   the model was created from a `code` string, or when `stan_file`'s
#'   directory isn't writable. The library embeds a hash of the generated
#'   C++, so it's reused across R sessions as long as the Stan program,
#'   `include_paths`, `external_cpp`, and installed stanr/Stan versions are
#'   unchanged, and is silently recompiled and overwritten otherwise --
#'   there is no separate cache-clearing step. On a cache hit, the
#'   Stan-to-C++ transpiler is skipped entirely, so any transpiler warnings
#'   (e.g. from pedantic mode or deprecated syntax) are only surfaced the
#'   first time a given model is compiled, not on subsequent cache hits.
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
