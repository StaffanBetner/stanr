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
# these after Makeconf's own CXXFLAGS/CXX17FLAGS (which are brought in ahead
# of the package Makevars file R writes for this call), so our -O3 -g0 wins
# the last-flag-wins compiler precedence instead of being silently
# overridden by Makeconf's -g -O2. USE_CXX17/PKG_CPPFLAGS/PKG_LIBS are not
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
    c(USE_CXX17 = "1"),
    withr::with_makevars(
      .stanr_apply_makevars(
        c(
          PKG_CPPFLAGS = cppflags,
          PKG_LIBS = libs,
          CXXFLAGS = .stanr_opt_flags(),
          CXX17FLAGS = .stanr_opt_flags()
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
    paste0("-I", shQuote(system.file("include", package = "stanr", mustWork = TRUE))),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS"
  )
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

# sourceCpp's own default cacheDir is a per-session temp directory, so
# without an explicit on-disk cache_dir every session would recompile every
# model from scratch instead of reusing the .so across sessions.
.stanr_models_cache_dir <- function() {
  cache_dir <- getOption(
    "stanr_cache_dir",
    file.path(tools::R_user_dir("stanr", "cache"), "models")
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.access(cache_dir, 2) != 0) {
    # Soft degrade: unwritable cache dir falls back to the session temp dir.
    cache_dir <- tempdir()
  }
  cache_dir
}

#' Normalized paths of every shared library currently mapped into this session.
#'
#' @noRd
.stanr_loaded_dll_paths <- function() {
  vapply(
    getLoadedDLLs(),
    function(dll) normalizePath(dll[["path"]], winslash = "/", mustWork = FALSE),
    character(1),
    USE.NAMES = FALSE
  )
}

#' Per-session record of the artifacts each cached translation unit produced.
#'
#' Entries are `list(cpp_file =, dynlibs =, alias =)`: the translation unit
#' this session compiles that entry from, the shared libraries it has loaded
#' for it, and how many forced rebuilds it has been redirected through.
#' Consumed by `.stanr_forced_rebuild_target()`.
#'
#' @noRd
.stanr_dynlib_registry <- function() {
  if (is.null(.stanr_memo$dynlibs)) {
    .stanr_memo$dynlibs <- new.env(parent = emptyenv())
  }
  .stanr_memo$dynlibs
}

#' Registry key: the canonical translation unit's path on disk.
#'
#' `model_hash` alone is not enough: it does not cover `cache_dir`, and the
#' same program under a different `stanr_cache_dir` is a different set of
#' artifacts with nothing of its own mapped, so it must not inherit another
#' cache's redirects.
#'
#' @noRd
.stanr_registry_key <- function(model_hash, cache_dir) {
  normalizePath(
    file.path(cache_dir, paste0("stan_", model_hash, ".cpp")),
    winslash = "/",
    mustWork = FALSE
  )
}

#' Marker recording that a `model_hash`'s canonical cache entry is superseded.
#'
#' Written when a forced rebuild is redirected to an alias translation unit.
#' Without it, the *next* session would load the superseded artifact from the
#' canonical entry and silently undo the forced rebuild.
#'
#' @noRd
.stanr_stale_marker <- function(model_hash, cache_dir) {
  file.path(cache_dir, paste0("stan_", model_hash, ".stale"))
}

#' Decide which translation unit a (possibly forced) compile should build.
#'
#' `Rcpp::sourceCpp(rebuild = TRUE)` replaces a translation unit's shared
#' library *in place*: it `dyn.unload()`s and deletes the previous one before
#' building. Everything built from that library dies with it -- live fits'
#' `model_ptr_`/`rng_ptr_` external pointers, the vtable their native calls
#' dispatch through, and the `Rcpp::XPtr` finalizers R runs over them at the
#' next garbage collection (not at process exit), which is where the session
#' segfaults. Because the generated C++ is cached by content hash, two
#' unrelated `StanModel` objects over the same program share one shared
#' library, so one model's forced recompile can invalidate another model's
#' live fits.
#'
#' A shared library must therefore never be unloaded while it is mapped. When
#' one is, the forced rebuild is redirected to an alias translation unit,
#' `stan_<hash>_r<N>.cpp`. `sourceCpp()` keys its build cache on the source
#' path, so the alias gets its own build directory and loads an *additional*
#' library; the mapped one stays mapped for the rest of the session. Later
#' compiles of a redirected hash stay on the newest alias. The redirect
#' target is always a translation unit this session has never loaded (`alias`
#' only moves forward), so the caller can pass `rebuild = force_recompile`
#' through to `sourceCpp()` unchanged: on the alias it can unload nothing
#' mapped, and it guarantees a real rebuild even when a previous session left
#' a byte-identical alias behind.
#'
#' @return `list(cpp_file =, alias =)`, where `alias` is 0 for the canonical
#'   translation unit and the redirect counter otherwise.
#' @noRd
.stanr_forced_rebuild_target <- function(
  model_hash,
  cache_dir,
  force_recompile
) {
  canonical <- .stanr_registry_key(model_hash, cache_dir)
  entry <- .stanr_dynlib_registry()[[canonical]]
  cpp_file <- entry$cpp_file %||%
    file.path(cache_dir, paste0("stan_", model_hash, ".cpp"))
  alias <- entry$alias %||% 0L
  mapped <- any(
    (entry$dynlibs %||% character()) %in% .stanr_loaded_dll_paths()
  )
  if (!force_recompile || !mapped) {
    return(list(cpp_file = cpp_file, alias = alias))
  }
  alias <- alias + 1L
  list(
    cpp_file = file.path(
      cache_dir,
      sprintf("stan_%s_r%d.cpp", model_hash, alias)
    ),
    alias = alias
  )
}

#' Record what a completed compile built and loaded.
#'
#' @noRd
.stanr_register_dynlibs <- function(
  model_hash,
  cache_dir,
  cpp_file,
  alias,
  dynlibs
) {
  registry <- .stanr_dynlib_registry()
  key <- .stanr_registry_key(model_hash, cache_dir)
  entry <- registry[[key]] %||% list(dynlibs = character())
  entry$cpp_file <- cpp_file
  entry$alias <- alias
  entry$dynlibs <- union(entry$dynlibs, dynlibs)
  registry[[key]] <- entry
  invisible(entry)
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
  "select_opencl_device"
)

.compile_stan_model_environment <- function(
  code,
  model_name,
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

  cache_dir <- .stanr_models_cache_dir()

  stale_marker <- .stanr_stale_marker(model_hash, cache_dir)
  # A forced rebuild redirected to an alias translation unit leaves the
  # canonical cache entry holding the superseded artifact, so it is marked
  # stale on disk. Honour that marker once per session, on the first compile
  # of this hash -- the only point at which nothing has been built from the
  # canonical entry yet, so rebuilding it in place is safe.
  if (
    !force_recompile &&
      is.null(
        .stanr_dynlib_registry()[[
          .stanr_registry_key(model_hash, cache_dir)
        ]]
      ) &&
      file.exists(stale_marker)
  ) {
    force_recompile <- TRUE
  }

  target <- .stanr_forced_rebuild_target(
    model_hash,
    cache_dir,
    force_recompile
  )
  cpp_file <- target$cpp_file
  # Only invoked on a cache miss (nothing on disk matches `model_hash`).
  # `force_recompile` also forces regeneration here, since `sourceCpp(rebuild
  # = force_recompile)` below would otherwise silently reuse a stale `.cpp`.
  if (force_recompile || !file.exists(cpp_file)) {
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
  if (precompiled_headers && length(external_cpp) == 0) {
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
      rebuild = force_recompile,
      cache_dir = cache_dir,
      verbose = verbose
    )
  }

  # Diffed around the compile rather than derived from `cache_dir`, which is
  # shared by every model: this has to attribute the shared library to *this*
  # hash. A warm cache still loads its library here, so the first compile of a
  # hash in a session always registers one.
  loaded_before <- .stanr_loaded_dll_paths()
  .stanr_compile_with_pch_retry(
    compile_model,
    cppflags,
    base_cppflags,
    pch_enabled,
    verbose
  )
  .stanr_register_dynlibs(
    model_hash,
    cache_dir,
    cpp_file,
    target$alias,
    setdiff(.stanr_loaded_dll_paths(), loaded_before)
  )

  if (target$alias > 0L) {
    file.create(stale_marker, showWarnings = FALSE)
  } else if (force_recompile) {
    unlink(stale_marker)
  }

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
#'   speed up compilation? The default is `TRUE`.
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
#'   Compiled models are cached persistently on disk (keyed on a hash of the
#'   generated C++, so the cache is reused across R sessions as long as the
#'   Stan program, `include_paths`, `external_cpp`, and installed stanr/Stan
#'   versions are unchanged) under `getOption("stanr_cache_dir")`, which
#'   defaults to a subdirectory of [tools::R_user_dir()]. Use
#'   [stanr_clear_cache()] to remove the cached models and precompiled
#'   headers. On a cache hit, the Stan-to-C++ transpiler is skipped entirely,
#'   so any transpiler warnings (e.g. from pedantic mode or deprecated
#'   syntax) are only surfaced the first time a given model is compiled, not
#'   on subsequent cache hits.
#'
#' @seealso [`StanModel`], [`$compile()`][model-method-compile],
#'   [`$sample()`][model-method-sample], [stanr_clear_cache()]
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

#' Paths of DLLs currently mapped into this session from under `dir`.
#'
#' Windows refuses to unlink a DLL while it is mapped into the process, so
#' `unlink(recursive = TRUE)` over a cache tree removes everything *except*
#' the loaded model artifacts and the directories holding them. This reports
#' which files those are, so `stanr_clear_cache()` can name them rather
#' than appear to have succeeded. POSIX unlinks a mapped file without
#' complaint, so in practice this is only ever non-empty on Windows.
#'
#' @noRd
.stanr_loaded_dlls_under <- function(dir) {
  if (!dir.exists(dir)) {
    return(character())
  }
  loaded <- .stanr_loaded_dll_paths()
  root <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  loaded[startsWith(loaded, paste0(root, "/"))]
}

#' Clear stanr's persistent compilation caches
#'
#' @description Deletes stanr's on-disk caches of compiled Stan models and
#'   precompiled model headers (PCH), freeing disk space and forcing the next
#'   compilation(s) to rebuild from scratch.
#'
#'   This always clears the package's own default cache root under
#'   [tools::R_user_dir("stanr", "cache")][tools::R_user_dir()] -- i.e. its
#'   `models` and `pch` subdirectories -- regardless of whether
#'   `getOption("stanr_cache_dir")` or `getOption("stanr_pch_dir")` has
#'   been set to point compilation at different directories. An overridden
#'   `stanr_cache_dir`/`stanr_pch_dir` is deliberately left untouched:
#'   stanr does not own that directory (it may be shared with other
#'   software or point outside the user cache root entirely), so this
#'   function only ever removes paths it created by default.
#'
#' @return An invisible named character vector with elements `models` and
#'   `pch`, giving the cache paths targeted for removal (present whether or
#'   not they existed beforehand).
#'
#' @section Models compiled in this session:
#'   Windows will not delete a DLL that is still mapped into the running
#'   process, so a model compiled earlier in this session keeps its own
#'   compiled artifact (and the directories containing it) alive: everything
#'   else is removed and those files remain. This function warns, naming
#'   what survived -- restart R and call it again to reclaim the rest.
#'   Models are *not* unloaded automatically, because any [`StanModel`] still
#'   referring to one would be left calling into unmapped memory. POSIX
#'   unlinks a mapped file without complaint, so this does not arise there.
#'
#' @seealso [stan_model()]
#'
#' @export
stanr_clear_cache <- function() {
  cache_root <- tools::R_user_dir("stanr", "cache")
  models_dir <- file.path(cache_root, "models")
  pch_dir <- file.path(cache_root, "pch")
  targets <- c(models_dir, pch_dir)

  for (target in targets) {
    if (dir.exists(target)) {
      unlink(target, recursive = TRUE)
    }
  }

  # `unlink()` signals failure only through its return code, and on Windows
  # it partially succeeds -- everything comes out except DLLs still mapped
  # into this session. Report that instead of returning as though the cache
  # were gone (see the "Models compiled in this session" section above).
  leftover <- targets[dir.exists(targets)]
  if (length(leftover)) {
    blocking <- unlist(lapply(leftover, .stanr_loaded_dlls_under))
    warning(
      "Could not fully clear the stanr cache. Still present:\n",
      paste0("  ", leftover, collapse = "\n"),
      if (length(blocking)) {
        paste0(
          "\nStill loaded in this R session (restart R to release):\n",
          paste0("  ", blocking, collapse = "\n")
        )
      },
      call. = FALSE
    )
  }

  invisible(c(models = models_dir, pch = pch_dir))
}
