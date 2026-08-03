# Applies an ordered list of `list(name, op, value)` assignments (as
# returned by `.newstan_parse_cpp_options()`) to a named `base` Makevars
# vector, in order: an `"="` assignment replaces `base[[name]]` outright, and
# a `"+="` assignment appends to it (space-joined) -- or, if `name` isn't yet
# in `base`, is equivalent to `"="`, matching how an unset Makefile variable
# behaves under `+=`. Applying assignments here (rather than folding them
# into a single flat named vector ahead of time and handing that to
# `withr::with_makevars()`) also sidesteps a `withr` pitfall: it silently
# drops all but one same-named entry when the caller already has a personal
# Makevars file on disk, so a vector with two "CXXFLAGS" entries wouldn't
# reliably apply both.
.newstan_apply_makevars <- function(base, assignments) {
  for (a in assignments) {
    if (identical(a$op, "+=") && a$name %in% names(base)) {
      base[[a$name]] <- paste(base[[a$name]], a$value)
    } else {
      base[a$name] <- a$value
    }
  }
  base
}

.compile_stan_model_environment <- function(
  code,
  model_name,
  include_directories = character(),
  external_cpp = NULL,
  verbose = FALSE,
  precompiled_headers = TRUE,
  force_recompile = FALSE,
  use_opencl = FALSE,
  cpp_options = list()
) {
  if (verbose) {
    message("[newstan] Compiling '", model_name, "'...")
  }

  # The generated wrapper is part of the translation unit.  Include it in the
  # cache key so changes to the native runner/model bridge (including
  # dev-time edits to inst/stan_model.cpp) cannot reuse a sourceCpp artifact
  # compiled against an older wrapper.
  model_support <- readLines(
    system.file("stan_model.cpp", package = "newstan", mustWork = TRUE)
  )
  # Step 1: Compute the cache key *before* invoking stanc(). stanc() (a JS
  # engine call into the bundled stanc.js) is by far the most expensive part
  # of this function, so on a warm cache we want to skip it entirely. This is
  # sound because stanc is a deterministic function of `code`,
  # `include_directories` (already spliced into `code` by the caller), and
  # `external_cpp` (spliced in by content, not path -- see below) -- and the
  # bundled stanc.js itself can only change with a newstan version bump,
  # which is already an input. So hashing these inputs has the same
  # discriminating power as hashing stanc()'s actual output, without paying
  # for it on a cache hit.
  #
  # `external_cpp` is hashed by file *contents* rather than path: stanc()
  # splices the contents of each file into the generated C++ (see
  # R/stanc.R), so identical content living at a different path must hash
  # identically, and different content living at the same path must hash
  # differently.
  external_cpp_contents <- vapply(
    external_cpp,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  # A named `cpp_options` entry (or an unnamed `"NAME = value"` string)
  # overrides the internally computed value for that name; an unnamed
  # `"NAME += value"` string appends to it instead. See
  # `.newstan_parse_cpp_options()` / `.newstan_apply_makevars()`.
  cpp_option_assignments <- .newstan_parse_cpp_options(cpp_options)
  is_opencl_libs <- vapply(
    cpp_option_assignments,
    function(a) identical(a$name, "OPENCL_LIBS"),
    logical(1)
  )
  opencl_assignments <- cpp_option_assignments[is_opencl_libs]
  # Any other `cpp_options` entries are generic Makefile variable/flag
  # overrides (e.g. `list(CXXFLAGS = "-march=native")`): merged into the
  # Makevars used to build the model further down. `OPENCL_LIBS` is excluded
  # here because it's consumed separately below -- it applies against the
  # OpenCL link flags default, not `libs`, so folding it in here too would
  # double it up.
  extra_assignments <- cpp_option_assignments[!is_opencl_libs]
  # OpenCL link flags are resolved here (rather than down near `libs` below)
  # because they are also a cache-key input: a `.so` linked against an
  # `OPENCL_LIBS` cpp_option override (e.g. POCL's ICD loader) is a genuinely
  # different artifact from one linked against the default. Only resolved
  # when `use_opencl` is TRUE -- when it's FALSE, `opencl_libs` is fixed to
  # `""` so `cpp_options`' `OPENCL_LIBS` entries are a no-op cache-key input
  # for non-OpenCL models (their value is irrelevant when OpenCL isn't in
  # play).
  opencl_libs <- if (use_opencl) {
    opencl_default <- if (Sys.info()[["sysname"]] == "Darwin") {
      "-framework OpenCL"
    } else {
      "-lOpenCL"
    }
    .newstan_apply_makevars(
      c(OPENCL_LIBS = opencl_default),
      opencl_assignments
    )[["OPENCL_LIBS"]]
  } else {
    ""
  }
  # R.version$platform and compiler identity are included because the model
  # cache is now persistent across sessions (see cache_dir below):
  # Rcpp::sourceCpp()'s own cache doesn't detect an in-place toolchain
  # upgrade (see .newstan_compiler_identity(), R/pch.R), so without these
  # inputs a stale .so built by an older/different compiler could be
  # dyn.load()ed indefinitely instead of triggering a rebuild.
  model_hash <- digest::digest(
    c(
      code,
      external_cpp_contents,
      model_support,
      as.character(utils::packageVersion("newstan")),
      .newstan_stan_version(),
      R.version$platform,
      .newstan_compiler_identity(),
      as.character(use_opencl),
      opencl_libs,
      # Stably sorted by name: `order()` preserves the relative order of
      # same-named assignments (ties), which is the only relative order that
      # affects the outcome of `.newstan_apply_makevars()` (assignments to
      # different names don't interact) -- so this keeps the hash stable
      # against reordering of unrelated `cpp_options` entries while still
      # varying if two assignments to the *same* name are reordered.
      if (length(extra_assignments)) {
        extra_names <- vapply(extra_assignments, `[[`, character(1), "name")
        ord <- order(extra_names)
        vapply(
          extra_assignments[ord],
          function(a) paste(a$name, a$op, a$value),
          character(1)
        )
      } else {
        character()
      }
    ),
    algo = "xxhash64"
  )

  # Persistent, cross-session cache: `model_hash` is stable across R
  # sessions for identical model source, but sourceCpp's own default
  # `cacheDir` is a per-session temp directory, so without an explicit
  # `cacheDir` every new session would recompile every model from scratch.
  # Resolving and passing a real on-disk `cache_dir` (below) lets sourceCpp
  # reuse a previously built shared object -- and skip the compiler
  # entirely -- across sessions.
  cache_dir <- getOption(
    "newstan_cache_dir",
    file.path(tools::R_user_dir("newstan", "cache"), "models")
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.access(cache_dir, 2) != 0) {
    # Soft degrade: unwritable/uncreatable cache dir falls back to the
    # session temp dir silently. Not worth alarming the user about -- the
    # only consequence is losing cross-session reuse for this session.
    cache_dir <- tempdir()
  }

  cpp_file <- file.path(cache_dir, paste0("stan_", model_hash, ".cpp"))
  # Step 2: Stan -> C++ via stanc.js (QuickJSR). Only invoked on a cache
  # miss -- i.e. when nothing on disk already matches `model_hash` -- which
  # is the whole point of hashing the pre-stanc() inputs above.
  # `force_recompile` also forces regeneration: `Rcpp::sourceCpp(rebuild =
  # force_recompile)` below always recompiles the object code from
  # `cpp_file`, but would otherwise silently reuse a stale on-disk `.cpp`
  # file if we didn't also regenerate it here.
  if (force_recompile || !file.exists(cpp_file)) {
    cpp_code <- stanc(
      code,
      include_directories = include_directories,
      external_cpp = external_cpp,
      use_opencl = use_opencl
    )
    writeLines(c(cpp_code, model_support), cpp_file)
  }

  cppflags <- paste(
    paste0("-I", system.file("include", package = "newstan", mustWork = TRUE)),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS"
  )
  if (use_opencl) {
    # Platform/device are pinned to 0/0 at compile time purely to satisfy the
    # hard `#error OPENCL_PLATFORM_ID_NOT_SET`-style guards in
    # opencl_context.hpp -- they do not constrain which device is actually
    # used at runtime. Real device selection happens later, per fit call, via
    # `select_opencl_device()` (see `.newstan_select_opencl` in
    # classes-model.R), so a single cached OpenCL-flavored binary serves
    # every device/platform a caller might select at runtime.
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
    pch_flags <- .newstan_pch_flags(base_cppflags, verbose)
    pch_enabled <- nzchar(pch_flags)
    cppflags <- paste(pch_flags, base_cppflags)
  }

  env <- new.env()
  runtime_archive <- system.file(
    "lib",
    Sys.getenv("R_ARCH"),
    "libnewstan_runner.a",
    package = "newstan",
    mustWork = TRUE
  )

  tbb_libs <- utils::capture.output(RcppParallel::RcppParallelLibs())
  if (
    .Platform$OS.type == "windows" &&
      utils::packageVersion("RcppParallel") >= '6.0.0'
  ) {
    tbb_libs <- "-ltbb12 -ltbbmalloc"
  }

  libs <- paste(shQuote(runtime_archive), tbb_libs)
  if (use_opencl) {
    libs <- paste(libs, opencl_libs)
  }

  compile_model <- function(compilation_cppflags) {
    # `+=` appends these after Makeconf's own CXXFLAGS/CXX17FLAGS (which are
    # brought in ahead of the package Makevars file R writes for this call),
    # so our -O3 -g0 wins the last-flag-wins compiler precedence instead of
    # being silently overridden by Makeconf's -g -O2. USE_CXX17/PKG_CPPFLAGS/
    # PKG_LIBS are not set by Makeconf, so `+=` on them is equivalent to `=`.
    # Remaining `cpp_options` (already resolved into `extra_assignments`,
    # `OPENCL_LIBS` aside) are applied in order, overriding or appending as
    # each entry requests -- see `.newstan_apply_makevars()`.
    withr::with_makevars(
      .newstan_apply_makevars(
        c(
          USE_CXX17 = "1",
          PKG_CPPFLAGS = compilation_cppflags,
          PKG_LIBS = libs,
          CXXFLAGS = .newstan_opt_flags,
          CXX17FLAGS = .newstan_opt_flags
        ),
        extra_assignments
      ),
      assignment = "+=",
      Rcpp::sourceCpp(
        file = cpp_file,
        env = env,
        rebuild = force_recompile,
        cacheDir = cache_dir,
        verbose = verbose
      )
    )
  }

  # `Rcpp::sourceCpp()` never propagates the compiler's actual diagnostic
  # text into the R condition it throws on a build failure -- it always
  # raises a generic synthetic message (e.g. "Error 1 occurred building
  # shared library."), regardless of what the compiler really said. That
  # means a genuinely stale on-disk PCH (e.g. after `R CMD INSTALL`
  # re-copies `inst/include/` and bumps the installed header's mtime) is
  # indistinguishable, from here, from any other compile failure by message
  # text alone. Instead of sniffing the message, check the PCH's staleness
  # directly: if it's missing, or older than any of the headers it covers,
  # attempt a single "rebuild the PCH, then retry once" recovery. If the PCH
  # looks fresh, the failure is almost certainly a genuine model C++ error
  # (e.g. a bad `external_cpp` file, which stanc() doesn't validate) --
  # rebuilding the PCH in that case would cost 30-60s before the same real
  # error surfaces unchanged, so skip straight to `stop(error)` instead.
  tryCatch(
    compile_model(cppflags),
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
      compile_model(paste(pch_flags, base_cppflags))
    }
  )

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
#'   `newstan_force_recompile` option.
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
#' @details `libnewstan_runner.a` (the static archive with the sampling/
#'   optimize/etc. service runners, linked into every model regardless of
#'   `use_opencl`) is always compiled without `STAN_OPENCL`, while an
#'   OpenCL-enabled model translation unit defines it; this is believed safe
#'   because services only touch the model through the
#'   `stan::model::model_base` virtual interface (no `matrix_cl` types cross
#'   that boundary), but is a latent ODR (One Definition Rule) hazard if
#'   that assumption is ever found to not hold.
#'
#' @return A [`StanModel`] object.
#'
#'   Compiled models are cached persistently on disk (keyed on a hash of the
#'   generated C++, so the cache is reused across R sessions as long as the
#'   Stan program, `include_paths`, `external_cpp`, and installed newstan/Stan
#'   versions are unchanged) under `getOption("newstan_cache_dir")`, which
#'   defaults to a subdirectory of [tools::R_user_dir()]. Use
#'   [newstan_clear_cache()] to remove the cached models and precompiled
#'   headers. On a cache hit, the Stan-to-C++ transpiler is skipped entirely,
#'   so any transpiler warnings (e.g. from pedantic mode or deprecated
#'   syntax) are only surfaced the first time a given model is compiled, not
#'   on subsequent cache hits.
#'
#' @seealso [`StanModel`], [`$compile()`][model-method-compile],
#'   [`$sample()`][model-method-sample], [newstan_clear_cache()]
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
  force_recompile = getOption("newstan_force_recompile", FALSE),
  precompiled_headers = TRUE,
  quiet = TRUE,
  external_cpp = NULL,
  use_opencl = FALSE
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
    use_opencl = use_opencl
  )
}

#' Clear newstan's persistent compilation caches
#'
#' @description Deletes newstan's on-disk caches of compiled Stan models and
#'   precompiled model headers (PCH), freeing disk space and forcing the next
#'   compilation(s) to rebuild from scratch.
#'
#'   This always clears the package's own default cache root under
#'   [tools::R_user_dir("newstan", "cache")][tools::R_user_dir()] -- i.e. its
#'   `models` and `pch` subdirectories -- regardless of whether
#'   `getOption("newstan_cache_dir")` has been set to point [stan_model()] at
#'   a different models cache directory. An overridden `newstan_cache_dir` is
#'   deliberately left untouched: newstan does not own that directory (it may
#'   be shared with other software or point outside the user cache root
#'   entirely), so this function only ever removes paths it created by
#'   default.
#'
#' @return An invisible named character vector with elements `models` and
#'   `pch`, giving the cache paths targeted for removal (present whether or
#'   not they existed beforehand).
#'
#' @seealso [stan_model()]
#'
#' @export
newstan_clear_cache <- function() {
  cache_root <- tools::R_user_dir("newstan", "cache")
  models_dir <- file.path(cache_root, "models")
  pch_dir <- file.path(cache_root, "pch")

  if (dir.exists(models_dir)) {
    unlink(models_dir, recursive = TRUE)
  }
  if (dir.exists(pch_dir)) {
    unlink(pch_dir, recursive = TRUE)
  }

  invisible(c(models = models_dir, pch = pch_dir))
}
