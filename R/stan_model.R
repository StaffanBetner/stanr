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
  for (pkg in c("RcppEigen", "BH")) {
    if (!nzchar(system.file(package = pkg))) {
      stop(
        "Package `",
        pkg,
        "` must be installed to compile Stan models.",
        call. = FALSE
      )
    }
  }

  # stanc() is the expensive step, so its inputs are hashed (with the same
  # discriminating power as hashing its output) to let a warm cache skip it.
  model_support <- readLines(
    system.file("stan_model.cpp", package = "newstan", mustWork = TRUE)
  )
  # external_cpp is hashed by content, not path: stanc splices file contents
  # into the generated C++, so the hash must depend on content, not location.
  external_cpp_contents <- .newstan_external_cpp_contents(external_cpp)
  # OPENCL_LIBS is consumed for link flags separately from the other
  # `cpp_options` assignments (avoiding double-applying it), and pinned to
  # `""` when OpenCL is off so it can't perturb the cache key.
  cpp_option_assignments <- .newstan_parse_cpp_options(cpp_options)
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
    .newstan_apply_makevars(
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
      external_cpp_contents,
      model_support,
      as.character(utils::packageVersion("newstan")),
      .newstan_stan_version(),
      R.version$platform,
      .newstan_compiler_identity(),
      as.character(use_opencl),
      opencl_libs,
      # Stable sort by name: reordering unrelated `cpp_options` entries must
      # not change the hash; reordering two assignments to the *same* name
      # must.
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

  # sourceCpp's own default cacheDir is a per-session temp directory, so
  # without an explicit on-disk cache_dir every session would recompile
  # every model from scratch instead of reusing the .so across sessions.
  cache_dir <- getOption(
    "newstan_cache_dir",
    file.path(tools::R_user_dir("newstan", "cache"), "models")
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.access(cache_dir, 2) != 0) {
    # Soft degrade: unwritable cache dir falls back to the session temp dir.
    cache_dir <- tempdir()
  }

  cpp_file <- file.path(cache_dir, paste0("stan_", model_hash, ".cpp"))
  # Only invoked on a cache miss (nothing on disk matches `model_hash`).
  # `force_recompile` also forces regeneration here, since `sourceCpp(rebuild
  # = force_recompile)` below would otherwise silently reuse a stale `.cpp`.
  if (force_recompile || !file.exists(cpp_file)) {
    if (verbose) {
      message("[newstan] Compiling '", model_name, "'...")
    }
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

  # libnewstan_runner.a is always compiled without STAN_OPENCL, while an
  # OpenCL-enabled model TU defines it; safe only as long as services touch
  # the model solely through the stan::model::model_base virtual interface.
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

  # `Rcpp::sourceCpp()` never propagates the compiler's actual diagnostics --
  # it always raises a generic synthetic error -- so PCH staleness must be
  # checked directly below rather than inferred from the error message.
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
#'   `getOption("newstan_cache_dir")` or `getOption("newstan_pch_dir")` has
#'   been set to point compilation at different directories. An overridden
#'   `newstan_cache_dir`/`newstan_pch_dir` is deliberately left untouched:
#'   newstan does not own that directory (it may be shared with other
#'   software or point outside the user cache root entirely), so this
#'   function only ever removes paths it created by default.
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
