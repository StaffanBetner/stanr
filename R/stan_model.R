.compile_stan_model_environment <- function(
  code,
  model_name,
  include_directories = character(),
  external_cpp = NULL,
  verbose = FALSE,
  precompiled_headers = TRUE,
  force_recompile = FALSE
) {
  if (verbose) {
    message("[newstan] Compiling '", model_name, "'...")
  }

  # Step 1: Stan -> C++ via stanc.js (QuickJSR)
  cpp_code <- stanc(
    code,
    include_directories = include_directories,
    external_cpp = external_cpp
  )
  # The generated wrapper is part of the translation unit.  Include it in the
  # cache key so changes to the native runner/model bridge (including
  # dev-time edits to inst/stan_model.cpp) cannot reuse a sourceCpp artifact
  # compiled against an older wrapper.
  model_support <- readLines(
    system.file("stan_model.cpp", package = "newstan", mustWork = TRUE)
  )
  # R.version$platform and compiler identity are included because the model
  # cache is now persistent across sessions (see cache_dir below):
  # Rcpp::sourceCpp()'s own cache doesn't detect an in-place toolchain
  # upgrade (see .newstan_compiler_identity(), R/pch.R), so without these
  # inputs a stale .so built by an older/different compiler could be
  # dyn.load()ed indefinitely instead of triggering a rebuild.
  model_hash <- digest::digest(
    c(
      cpp_code,
      model_support,
      as.character(utils::packageVersion("newstan")),
      .newstan_stan_version(),
      R.version$platform,
      .newstan_compiler_identity()
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
  if (!file.exists(cpp_file)) {
    writeLines(c(cpp_code, model_support), cpp_file)
  }

  cppflags <- paste(
    paste0("-I", system.file("include", package = "newstan", mustWork = TRUE)),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS"
  )
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

  compile_model <- function(compilation_cppflags) {
    # `+=` appends these after Makeconf's own CXXFLAGS/CXX17FLAGS (which are
    # brought in ahead of the package Makevars file R writes for this call),
    # so our -O3 -g0 wins the last-flag-wins compiler precedence instead of
    # being silently overridden by Makeconf's -g -O2. USE_CXX17/PKG_CPPFLAGS/
    # PKG_LIBS are not set by Makeconf, so `+=` on them is equivalent to `=`.
    withr::with_makevars(
      c(
        USE_CXX17 = 1,
        PKG_CPPFLAGS = compilation_cppflags,
        PKG_LIBS = libs,
        CXXFLAGS = .newstan_opt_flags,
        CXX17FLAGS = .newstan_opt_flags
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
  # indistinguishable, from here, from any other compile failure -- there is
  # no message text to sniff. So instead of trying to detect *which*
  # failure occurred, always attempt a single "rebuild the PCH, then retry
  # once" recovery whenever a PCH is in use. If the original failure was
  # unrelated to the PCH (a real syntax error, a linker error, etc.), this
  # costs one extra rebuild-and-recompile cycle before the same real error
  # surfaces unchanged (the retried `compile_model()` call below has no
  # further `tryCatch` around it) -- cheap insurance either way.
  tryCatch(
    compile_model(cppflags),
    error = function(error) {
      if (!pch_enabled) {
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
#' @param cpp_options (list) C++ compilation options. Not yet supported by the
#'   in-process backend.
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
#'
#' @return A [`StanModel`] object.
#'
#'   Compiled models are cached persistently on disk (keyed on a hash of the
#'   generated C++, so the cache is reused across R sessions as long as the
#'   Stan program, `include_paths`, `external_cpp`, and installed newstan/Stan
#'   versions are unchanged) under `getOption("newstan_cache_dir")`, which
#'   defaults to a subdirectory of [tools::R_user_dir()]. Use
#'   [newstan_clear_cache()] to remove the cached models and precompiled
#'   headers.
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
  external_cpp = NULL
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
    external_cpp = external_cpp
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
