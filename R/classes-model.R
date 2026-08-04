# StanModel class definition ---------------------------------------------------

#' StanModel objects
#'
#' @name StanModel
#' @description A `StanModel` object is an [R6][R6::R6Class] object created by
#'   [stan_model()]. The object stores the Stan program source code and compiled
#'   model environment, and provides methods for fitting the model using Stan's
#'   inference algorithms.
#'
#' @section Methods: `StanModel` objects have the following associated methods,
#'   many of which have their own (linked) documentation pages:
#'
#'  ## Stan code
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$code()`][model-method-model-info] | Return Stan program as a string. |
#'  [`$print()`][model-method-model-info] | Print the Stan program. |
#'
#'  ## Model information
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$model_name()`][model-method-model-info] | Return the model name. |
#'  [`$stan_file()`][model-method-model-info] | Return the path to the Stan file. |
#'  [`$has_stan_file()`][model-method-model-info] | Check whether the model was created with a Stan file. |
#'  [`$include_paths()`][model-method-model-info] | Return the Stan include paths. |
#'  [`$stan_version()`][model-method-model-info] | Return the Stan version used by the package. |
#'  [`$variables()`][model-method-variables] | Return input and output variable information. |
#'  [`$cpp_options()`][model-method-model-info] | Return the C++ options associated with the model. |
#'  [`$stanc_options()`][model-method-model-info] | Return the stanc options associated with the model. |
#'
#'  ## Compilation
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$check_syntax()`][model-method-check-syntax] | Check the Stan program's syntax without compiling. |
#'  [`$format()`][model-method-format] | Reformat the Stan program using `stanc`'s auto-formatter. |
#'  [`$compile()`][model-method-compile] | Compile the Stan program. |
#'  [`$is_compiled()`][model-method-model-info] | Check whether the model has been compiled. |
#'
#'  ## Diagnostics
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$diagnose()`][model-method-diagnose] | Run Stan's `"diagnose"` method to test gradients, return [`StanDiagnose`] object. |
#'
#'  ## Function exposure
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$expose_stan_functions()`][model-method-expose-stan-functions] | Expose the program's `functions` block as R functions. |
#'  [`$functions`][model-method-expose-stan-functions] | Environment holding the exposed functions. |
#'
#'  ## Model fitting
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$sample()`][model-method-sample] | Run Stan's `"sample"` method (HMC/NUTS MCMC), return [`StanMCMC`] object. |
#'  [`$optimize()`][model-method-optimize] | Run Stan's `"optimize"` method, return [`StanMLE`] object. |
#'  [`$laplace()`][model-method-laplace] | Run Stan's `"laplace"` method, return [`StanLaplace`] object. |
#'  [`$variational()`][model-method-variational] | Run Stan's `"variational"` method (ADVI), return [`StanVB`] object. |
#'  [`$pathfinder()`][model-method-pathfinder] | Run Stan's `"pathfinder"` method, return [`StanPathfinder`] object. |
#'  [`$generate_quantities()`][model-method-generate-quantities] | Run Stan's `"generate quantities"` method, return [`StanGQ`] object. |
#'
NULL

StanModel <- R6Class(
  "StanModel",
  public = list(
    functions = NULL,
    initialize = function(
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
      compile <- .stanr_flag(compile, "compile")
      force_recompile <- .stanr_flag(force_recompile, "force_recompile")
      precompiled_headers <- .stanr_flag(
        precompiled_headers,
        "precompiled_headers"
      )
      quiet <- .stanr_flag(quiet, "quiet")
      use_opencl <- .stanr_flag(use_opencl, "use_opencl")
      compile_standalone <- .stanr_flag(
        compile_standalone,
        "compile_standalone"
      )
      if (is.null(stan_file) == is.null(code)) {
        stop("Supply exactly one of `stan_file` and `code`.", call. = FALSE)
      }
      if (!is.null(stan_file)) {
        if (
          !is.character(stan_file) ||
            length(stan_file) != 1L ||
            is.na(stan_file) ||
            !file.exists(stan_file)
        ) {
          stop("`stan_file` must name an existing Stan file.", call. = FALSE)
        }
        stan_file <- normalizePath(stan_file, mustWork = TRUE)
        code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")
      }
      if (!is.character(code) || length(code) != 1L || is.na(code)) {
        stop("`code` must be a single non-missing string.", call. = FALSE)
      }
      if (is.null(model_name)) {
        model_name <- if (is.null(stan_file)) {
          "model"
        } else {
          sub("\\.stan$", "", basename(stan_file))
        }
      }
      if (
        !is.character(model_name) ||
          length(model_name) != 1L ||
          is.na(model_name) ||
          !nzchar(model_name)
      ) {
        stop("`model_name` must be a single non-empty string.", call. = FALSE)
      }
      include_paths <- include_paths %||% character()
      if (!is.character(include_paths) || anyNA(include_paths)) {
        stop("`include_paths` must be a character vector.", call. = FALSE)
      }
      if (length(include_paths)) {
        include_paths <- normalizePath(include_paths, mustWork = TRUE)
      }
      # Validated (and, for the raw-string form, syntax-checked) up front so
      # a malformed `cpp_options` entry fails fast at construction rather
      # than at compile time; the parsed assignments themselves are
      # re-derived from the stored raw list in `.compile_stan_model_environment()`.
      .stanr_parse_cpp_options(cpp_options)
      if (!is.list(stanc_options)) {
        stop("`stanc_options` must be a list.", call. = FALSE)
      }
      if (length(stanc_options)) {
        stop(
          "Non-empty `stanc_options` are not yet supported by `stan_model()`.",
          call. = FALSE
        )
      }
      if (!is.null(user_header)) {
        stop(
          "`user_header` is not yet supported; use `external_cpp`.",
          call. = FALSE
        )
      }

      private$stan_file_ <- stan_file
      private$code_ <- code
      private$model_name_ <- model_name
      private$include_paths_ <- include_paths
      private$cpp_options_ <- cpp_options
      private$stanc_options_ <- stanc_options
      private$force_recompile_ <- force_recompile
      private$precompiled_headers_ <- precompiled_headers
      private$quiet_ <- quiet
      private$external_cpp_ <- external_cpp
      private$use_opencl_ <- use_opencl
      private$compile_standalone_ <- compile_standalone
      self$functions <- new.env(parent = emptyenv())
      if (compile) {
        self$compile()
      }
      invisible(self)
    }
  ),
  private = list(
    stan_file_ = NULL,
    code_ = NULL,
    model_name_ = NULL,
    include_paths_ = NULL,
    cpp_options_ = NULL,
    stanc_options_ = NULL,
    force_recompile_ = FALSE,
    precompiled_headers_ = TRUE,
    quiet_ = TRUE,
    external_cpp_ = NULL,
    use_opencl_ = FALSE,
    compile_standalone_ = FALSE,
    compiled_env_ = NULL,
    functions_compiled_env_ = NULL,
    compile_generation_ = 0L,
    variables_ = NULL,
    resolved_code_ = NULL,
    ensure_compiled = function() {
      if (is.null(private$compiled_env_)) {
        self$compile()
      }
      invisible(NULL)
    },
    # `code_` is immutable after `initialize()` (never reassigned), so this
    # needs no invalidation logic: once resolved, it is valid for the
    # object's whole lifetime. Shared between `$compile()` and `$variables()`
    # so `#include` resolution -- a recursive file-system walk -- happens at
    # most once per model, however many times either method is called.
    resolved_code = function() {
      if (is.null(private$resolved_code_)) {
        private$resolved_code_ <- resolve_stan_includes(
          private$code_,
          private$include_paths_
        )
      }
      private$resolved_code_
    }
  ),
  cloneable = FALSE
)

# StanModel information methods ------------------------------------------------

#' Access information from a `StanModel` object
#'
#' @name model-method-model-info
#' @family StanModel methods
#'
#' @description These methods access information stored in a [`StanModel`]
#'   object and print its Stan program.
#'
#'   ```
#'   stan_file()
#'   has_stan_file()
#'   code()
#'   print()
#'   model_name()
#'   include_paths()
#'   stan_version()
#'   is_compiled()
#'   cpp_options()
#'   stanc_options()
#'   use_opencl()
#'   ```
#'
#' @return
#' * `$stan_file()` returns a path as a string, or `character()` if the model
#'   was created from code (not a file).
#' * `$has_stan_file()` returns `TRUE` if the model was created with a Stan file
#'   and `FALSE` otherwise.
#' * `$code()` returns the Stan program as a single string.
#' * `$print()` prints the Stan program and returns the [`StanModel`] object
#'   invisibly.
#' * `$model_name()` returns the model name as a string.
#' * `$include_paths()` returns a character vector of absolute paths.
#' * `$stan_version()` returns the Stan version bundled with the package as a
#'   string.
#' * `$is_compiled()` returns `TRUE` if the model has been compiled.
#' * `$cpp_options()` returns the `cpp_options` list the model was created
#'   with (see [stan_model()]).
#' * `$stanc_options()` returns a named list of stanc options.
#' * `$use_opencl()` returns `TRUE` if the model was created with
#'   `use_opencl = TRUE` and `FALSE` otherwise.
#'
#' @seealso [`$compile()`][model-method-compile] and [stan_model()]
#'
NULL

stan_model_stan_file <- function() private$stan_file_ %||% character()
StanModel$set("public", "stan_file", stan_model_stan_file)

stan_model_has_stan_file <- function() !is.null(private$stan_file_)
StanModel$set("public", "has_stan_file", stan_model_has_stan_file)

stan_model_code <- function() private$code_
StanModel$set("public", "code", stan_model_code)

stan_model_print <- function() {
  cat(private$code_, "\n", sep = "")
  invisible(self)
}
StanModel$set("public", "print", stan_model_print)

stan_model_model_name <- function() private$model_name_
StanModel$set("public", "model_name", stan_model_model_name)

stan_model_include_paths <- function() private$include_paths_
StanModel$set("public", "include_paths", stan_model_include_paths)

stan_model_stan_version <- function() .stanr_stan_version()
StanModel$set("public", "stan_version", stan_model_stan_version)

stan_model_is_compiled <- function() !is.null(private$compiled_env_)
StanModel$set("public", "is_compiled", stan_model_is_compiled)

stan_model_compile_generation <- function() private$compile_generation_
StanModel$set("public", "compile_generation", stan_model_compile_generation)

stan_model_cpp_options <- function() private$cpp_options_
StanModel$set("public", "cpp_options", stan_model_cpp_options)

stan_model_stanc_options <- function() private$stanc_options_
StanModel$set("public", "stanc_options", stan_model_stanc_options)

stan_model_use_opencl <- function() private$use_opencl_
StanModel$set("public", "use_opencl", stan_model_use_opencl)

#' Input and output variables of a Stan program
#'
#' @name model-method-variables
#' @aliases variables
#' @family StanModel methods
#'
#' @description The `$variables()` method of a [`StanModel`] object returns
#'   a list, each element representing a Stan model block: `data`, `parameters`,
#'   `transformed_parameters` and `generated_quantities`.
#'
#'   Each element contains a list of variables, with each variable represented
#'   as a list with information on its scalar type (`real` or `int`) and
#'   number of dimensions.
#'
#'   The number of dimensions reported is the number of indexing dimensions in
#'   the declared Stan variable, equivalently the number of indices needed to
#'   access one scalar element. This means a scalar has 0 dimensions, a vector
#'   or one-dimensional array has 1, and a matrix or two-dimensional array has
#'   2. Array dimensions are added to any vector or matrix dimensions, so
#'   `array[J] matrix[N, K]` has 3 dimensions.
#'
#'   `transformed data` is not included, as variables in that block are not
#'   part of the model's input or output.
#'
#' @return A list with information on input and output variables for each of
#'   the Stan model blocks.
#'
#' @examples
#' \dontrun{
#' mod <- stan_model(
#'   code = "
#'   data {
#'     int N;
#'     array[2, 3] int y;
#'   }
#'   parameters {
#'     real alpha;
#'     vector[N] beta;
#'     array[2] matrix[3, 4] theta;
#'   }
#'   ",
#'   compile = FALSE
#' )
#'
#' vars <- mod$variables()
#' str(vars)
#' }
#'
NULL

stan_model_variables <- function() {
  if (is.null(private$variables_)) {
    private$variables_ <- model_variables(
      model_code = private$resolved_code(),
      include_directories = character(),
      allow_undefined = length(private$external_cpp_) > 0
    )
  }
  private$variables_
}
StanModel$set("public", "variables", stan_model_variables)

# StanModel compilation methods ------------------------------------------------

#' Compile a Stan program
#'
#' @name model-method-compile
#' @aliases compile
#' @family StanModel methods
#'
#' @description The `$compile()` method of a [`StanModel`] object compiles the
#'   Stan program using the in-process backend. In most cases the user does not
#'   need to explicitly call `$compile()` as compilation occurs automatically
#'   when calling [stan_model()]. However it is possible to set `compile = FALSE`
#'   in the call to `stan_model()` and subsequently call `$compile()` directly.
#'
#' @param force_recompile (logical) Should the model be recompiled even if it
#'   has not been modified since it was last compiled? The default is `FALSE`.
#' @param quiet (logical) Should verbose output from compilation be suppressed?
#'   The default is `TRUE`.
#' @param compile_standalone (logical) Should the Stan program's `functions`
#'   block be exposed as part of this compilation? Defaults to whatever was
#'   set at [stan_model()] construction time. See
#'   [`$expose_stan_functions()`][model-method-expose-stan-functions].
#'
#' @return The [`StanModel`] object, invisibly.
#'
#' @seealso [`$is_compiled()`][model-method-model-info] and [stan_model()]
#'
NULL

stan_model_compile <- function(
  force_recompile = private$force_recompile_,
  quiet = private$quiet_,
  compile_standalone = private$compile_standalone_
) {
  force_recompile <- .stanr_flag(force_recompile, "force_recompile")
  quiet <- .stanr_flag(quiet, "quiet")
  compile_standalone <- .stanr_flag(compile_standalone, "compile_standalone")
  # Incremented unconditionally, before compilation runs, so the generation
  # always reflects "a compile was attempted" -- fits use this (via
  # `$compile_generation()`) to know whether their cached native pointer
  # might now be stale, even if `.compile_stan_model_environment()` below
  # throws partway through.
  private$compile_generation_ <- private$compile_generation_ + 1L
  private$compiled_env_ <- .compile_stan_model_environment(
    code = private$resolved_code(),
    model_name = private$model_name_,
    external_cpp = private$external_cpp_,
    verbose = !quiet,
    precompiled_headers = private$precompiled_headers_,
    force_recompile = force_recompile,
    use_opencl = private$use_opencl_,
    cpp_options = private$cpp_options_,
    standalone_functions = compile_standalone
  )
  if (compile_standalone) {
    # cmdstanr parity: compile_standalone exposes without a separate
    # $expose_stan_functions() call. Unconditional on every $compile() run
    # (not just the first) -- $compile() can be re-run via
    # force_recompile, and rebuilding from the fresh compiled env keeps
    # the function objects pointing at live (not stale/freed) symbols.
    private$functions_compiled_env_ <- private$compiled_env_
    .stanr_build_functions_env(
      private$compiled_env_,
      self$functions,
      global = FALSE
    )
  }
  invisible(self)
}
StanModel$set("public", "compile", stan_model_compile)

#' Check Stan program syntax
#'
#' @name model-method-check-syntax
#' @aliases check_syntax
#' @family StanModel methods
#'
#' @description The `$check_syntax()` method of a [`StanModel`] object runs
#'   the Stan program through `stanc` without compiling the generated C++. It
#'   is a cheap way to validate a program before calling
#'   [`$compile()`][model-method-compile].
#'
#' @param pedantic (logical) Should `stanc`'s pedantic-mode warnings be
#'   requested? The default is `FALSE`.
#' @param quiet (logical) Should the success message be suppressed? The
#'   default is `FALSE`.
#'
#' @return `TRUE`, invisibly. Errors if the program has a syntax error.
#'
#' @seealso [`$compile()`][model-method-compile]
#'
NULL

stan_model_check_syntax <- function(pedantic = FALSE, quiet = FALSE) {
  pedantic <- .stanr_flag(pedantic, "pedantic")
  quiet <- .stanr_flag(quiet, "quiet")
  stanc(
    private$resolved_code(),
    external_cpp = private$external_cpp_,
    use_opencl = private$use_opencl_,
    warn_pedantic = pedantic
  )
  if (!quiet) {
    message("[stanr] Stan program is syntactically correct.")
  }
  invisible(TRUE)
}
StanModel$set("public", "check_syntax", stan_model_check_syntax)

#' Format a Stan program
#'
#' @name model-method-format
#' @aliases format
#' @family StanModel methods
#'
#' @description The `$format()` method of a [`StanModel`] object reformats
#'   the Stan program using `stanc`'s auto-formatter and returns the result as
#'   a string. Unlike [`$check_syntax()`][model-method-check-syntax], it does
#'   not work on programs with unresolved `#include` directives, since
#'   formatting would inline their contents.
#'
#' @param overwrite_file (logical) Should the formatted code be written back
#'   to [`$stan_file()`][model-method-model-info]? The default is `FALSE`.
#'   Requires a model created with `stan_file`, and does not update this
#'   object's in-memory code -- construct a new [`StanModel`] to pick up the
#'   change.
#' @param canonicalize (logical or character) `FALSE` (the default) formats
#'   without canonicalizing, `TRUE` also canonicalizes deprecated syntax, and
#'   a character vector requests specific canonicalizations (e.g.
#'   `c("braces", "parentheses")`).
#' @param backup (logical) When `overwrite_file = TRUE`, should the original
#'   file be backed up first? The default is `TRUE`.
#' @param max_line_length (integer) Maximum output line width. The default,
#'   `NULL`, uses `stanc`'s default.
#' @param quiet (logical) Should the backup message be suppressed? The
#'   default is `FALSE`.
#'
#' @return The formatted Stan code as a string.
#'
#' @seealso [`$check_syntax()`][model-method-check-syntax], [`$code()`][model-method-model-info]
#'
NULL

stan_model_format <- function(
  overwrite_file = FALSE,
  canonicalize = FALSE,
  backup = TRUE,
  max_line_length = NULL,
  quiet = FALSE
) {
  overwrite_file <- .stanr_flag(overwrite_file, "overwrite_file")
  backup <- .stanr_flag(backup, "backup")
  quiet <- .stanr_flag(quiet, "quiet")
  if (
    !isFALSE(canonicalize) &&
      !isTRUE(canonicalize) &&
      !is.character(canonicalize)
  ) {
    stop(
      "`canonicalize` must be FALSE, TRUE, or a character vector.",
      call. = FALSE
    )
  }
  if (!is.null(max_line_length)) {
    max_line_length <- .stanr_int(max_line_length, "max_line_length", min = 1L)
  }
  if (overwrite_file) {
    if (!self$has_stan_file()) {
      stop(
        "`overwrite_file = TRUE` requires a model created with `stan_file`.",
        call. = FALSE
      )
    }
    if (grepl("#include", private$code_, fixed = TRUE)) {
      stop(
        "`overwrite_file = TRUE` is not supported for programs with ",
        "`#include` directives, since formatting inlines them.",
        call. = FALSE
      )
    }
  }

  formatted <- stanc_format(
    private$resolved_code(),
    canonicalize = canonicalize,
    max_line_length = max_line_length
  )

  if (overwrite_file) {
    if (backup) {
      backup_file <- paste0(
        private$stan_file_,
        ".bak-",
        format(Sys.time(), "%Y%m%d%H%M%S")
      )
      file.copy(private$stan_file_, backup_file)
      if (!quiet) {
        message("[stanr] Old version of the model stored to ", backup_file)
      }
    }
    # `formatted` already ends in "\n"; writeLines() would add a second one.
    writeLines(formatted, private$stan_file_, sep = "")
  }

  formatted
}
StanModel$set("public", "format", stan_model_format)

# Selects the OpenCL platform/device for the model's native computations to
# run on. Triggers lazy compilation via `native_function()` if needed, and
# runs on the R thread before `new_model()` -- with `use_opencl` codegen,
# data transfer to the OpenCL device happens inside the model constructor
# itself, so device selection must happen first. Errors cleanly (via the
# C++ stub's `Rcpp::stop()`) if the compiled model was not built with
# `use_opencl = TRUE`.
stan_model_select_opencl <- function(opencl_ids) {
  ids <- as.integer(opencl_ids)
  if (length(ids) != 2L || anyNA(ids) || any(ids < 0L)) {
    stop("`opencl_ids` must be c(platform_id, device_id).", call. = FALSE)
  }
  self$native_function("select_opencl_device")(ids[[1]], ids[[2]])
  invisible(NULL)
}
StanModel$set("private", "select_opencl", stan_model_select_opencl)

# StanModel function-exposure methods -------------------------------------------

#' Expose Stan functions as R functions
#'
#' @name model-method-expose-stan-functions
#' @aliases expose_stan_functions expose_functions
#' @family StanModel methods
#'
#' @description The `$expose_stan_functions()` method of a [`StanModel`]
#'   object compiles the functions declared in the Stan program's `functions`
#'   block and makes them callable from R. `$expose_functions()` is an alias
#'   (cmdstanr's name for the same method).
#'
#'   ```
#'   expose_stan_functions(global = FALSE, verbose = FALSE)
#'   expose_functions(global = FALSE, verbose = FALSE)
#'   ```
#'
#'   Exposed functions are always assigned into the `$functions` member
#'   environment (e.g. `mod$functions$my_fun(...)`); `global = TRUE`
#'   additionally assigns each one into the global environment, so it can
#'   also be called directly (`my_fun(...)`). Repeat calls are cheap: once a
#'   model's functions have been compiled -- whether by an earlier
#'   `$expose_stan_functions()` call, or automatically via
#'   `compile_standalone = TRUE` at [stan_model()] time -- a later call just
#'   performs the requested `$functions`/global assignments, without
#'   recompiling.
#'
#'   A Stan function whose name ends in `_rng` gets a trailing `seed = NULL`
#'   argument on its R wrapper. All exposed `_rng` functions share one
#'   underlying RNG, seeded once per `$expose_stan_functions()` call from
#'   R's own RNG stream (so calling `set.seed()` before exposing makes draws
#'   reproducible); passing `seed` explicitly to a call reseeds the
#'   generator immediately before that call.
#'
#'   Stan `tuple(...)` arguments/returns map to/from **unnamed** R lists (one
#'   element per slot; arrays of tuples become lists of such lists), and
#'   `complex` / `complex_vector` / `complex_row_vector` / `complex_matrix`
#'   map to/from R's native complex type. An overloaded Stan function (same
#'   name, different signature) exposes only the first-defined overload, with
#'   a warning.
#'
#' @param global (logical) Should the exposed functions also be assigned
#'   into the global environment? The default, `FALSE`, only populates
#'   `$functions`.
#' @param verbose (logical) Should compiler progress messages be printed?
#'   No compilation happens (so this has no effect) if the functions were
#'   already compiled, e.g. via `compile_standalone = TRUE`.
#'
#' @return The [`StanModel`] object's `$functions` environment, invisibly.
#'
#' @section Caching: Compiled functions are cached on disk the same way
#'   compiled models are (see [stan_model()]) -- a warm cache skips
#'   recompilation entirely, including across R sessions.
#'
#' @section Serialization: Exposed function objects in `$functions` are
#'   compiled bindings and, like any compiled function from this package, do
#'   not survive `saveRDS()`/`readRDS()`. After restoring a saved
#'   [`StanModel`] or [`StanFit`], call `$expose_stan_functions()` again to
#'   repopulate `$functions` -- this rebuilds from the on-disk cache and
#'   does not recompile.
#'
#' @seealso [stan_model()] for the `compile_standalone` argument, which
#'   exposes functions as part of the model's own compilation.
#'
NULL

stan_model_expose_stan_functions <- function(global = FALSE, verbose = FALSE) {
  global <- .stanr_flag(global, "global")
  verbose <- .stanr_flag(verbose, "verbose")

  if (is.null(private$functions_compiled_env_)) {
    if (
      !is.null(private$compiled_env_) &&
        !is.null(private$compiled_env_$stanr_exposed_functions)
    ) {
      private$functions_compiled_env_ <- private$compiled_env_
    } else {
      # Deliberately does not go through `private$ensure_compiled()` /
      # `self$native_function()`: either would trigger a full `self$compile()`
      # as a side effect on a never-compiled model, but exposing functions
      # must work on a `compile = FALSE` model without compiling it.
      private$functions_compiled_env_ <- .compile_standalone_functions_environment(
        code = private$resolved_code(),
        external_cpp = private$external_cpp_,
        cpp_options = private$cpp_options_,
        verbose = verbose,
        precompiled_headers = private$precompiled_headers_
      )
    }
  }

  .stanr_build_functions_env(
    private$functions_compiled_env_,
    self$functions,
    global
  )
  invisible(self$functions)
}
StanModel$set(
  "public",
  "expose_stan_functions",
  stan_model_expose_stan_functions
)
StanModel$set("public", "expose_functions", stan_model_expose_stan_functions)

# StanModel fitting methods ----------------------------------------------------

#' Run MCMC sampling
#'
#' @name model-method-sample
#' @aliases sample
#' @family StanModel methods
#'
#' @description The `$sample()` method of a [`StanModel`] object runs Stan's
#'   `"sample"` method, which uses Hamiltonian Monte Carlo (HMC) with the
#'   No-U-Turn Sampler (NUTS) to draw from the posterior distribution.
#'   Returns a [`StanMCMC`] object.
#'
#' @template param-data
#' @param seed (integer) The random seed for reproducibility.
#' @param refresh (integer) How often (in iterations) to print progress.
#' @template param-init
#' @param chains (integer) The number of MCMC chains.
#' @param chain_ids (integer vector) The IDs for each chain.
#' @param num_threads (integer) The total number of threads to use across all
#'   chains. Defaults to `RcppParallel::defaultNumThreads()` (all available threads).
#' @param iter_warmup (integer) The number of warmup iterations.
#' @param iter_sampling (integer) The number of sampling iterations.
#' @param save_warmup (logical) Should warmup samples be saved? Ignored when
#'   `fixed_param = TRUE`, since that mode runs no warmup and so has nothing
#'   to save.
#' @param thin (integer) Thin interval.
#' @param max_treedepth (integer) Maximum tree depth for NUTS.
#' @param adapt_engaged (logical) Should step size adaptation be used?
#' @param adapt_delta (number) Target acceptance statistic for HMC.
#' @param step_size (number) Step size for HMC.
#' @param metric Character string indicating the metric to use: `"diag_e"`,
#'   `"dense_e"`, or `"unit_e"`.
#' @param inv_metric (numeric) Initial inverse mass matrix values.
#' @param init_buffer (integer) Adaptation phase: initial buffer length.
#' @param term_buffer (integer) Adaptation phase: terminal buffer length.
#' @param window (integer) Adaptation phase: window length.
#' @param fixed_param (logical) Treat all parameters as fixed (no adaptation).
#' @param show_messages (logical) When `TRUE` (the default), print progress
#'   and informational output (e.g. iteration and timing messages) to the
#'   console. Set to `FALSE` to silence it. Suppressed output is still
#'   recorded and available via the fit's `$output()` method.
#' @param show_exceptions (logical) When `TRUE` (the default), print
#'   informational messages about numerical exceptions -- e.g. Metropolis
#'   proposal rejections and rejected initial values. Set to `FALSE` to
#'   silence them. Suppressed messages are still recorded and available via
#'   the fit's `$output()` method.
#' @param diagnostics (character vector) Which sampler diagnostics to check
#'   immediately after sampling, printing a warning message for any problems
#'   found -- see [`$diagnostic_summary()`][fit-method-mcmc]. One or more of
#'   `"divergences"`, `"treedepth"`, and `"ebfmi"`. The default checks all
#'   three; `NULL` or `""` skips the check entirely. Ignored when
#'   `fixed_param = TRUE`.
#' @param engine (string) The sampling engine: `"nuts"` or `"static"`.
#' @param int_time (number) Integration time for static HMC.
#' @param step_size_jitter (number) Jitter for step size after adaptation.
#' @param adapt_gamma (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_kappa (number) Adaptation hyperparameter for dual averaging.
#' @param adapt_t0 (number) Adaptation hyperparameter for dual averaging.
#' @template param-opencl_ids
#'
#' @return A [`StanMCMC`] object containing posterior draws and diagnostics.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_sample <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  save_latent_dynamics = FALSE,
  chains = 4,
  chain_ids = seq_len(chains),
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  iter_warmup = 1000L,
  iter_sampling = 1000L,
  save_warmup = FALSE,
  thin = 1L,
  max_treedepth = 10L,
  adapt_engaged = TRUE,
  adapt_delta = 0.8,
  step_size = 1,
  metric = "diag_e",
  metric_file = NULL,
  inv_metric = NULL,
  init_buffer = 75L,
  term_buffer = 50L,
  window = 25L,
  fixed_param = FALSE,
  show_messages = TRUE,
  show_exceptions = TRUE,
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  engine = "nuts",
  int_time = 2 * pi,
  step_size_jitter = 0,
  adapt_gamma = 0.05,
  adapt_kappa = 0.75,
  adapt_t0 = 10
) {
  save_latent_dynamics <- .stanr_flag(
    save_latent_dynamics,
    "save_latent_dynamics"
  )
  save_warmup <- .stanr_flag(save_warmup, "save_warmup")
  adapt_engaged <- .stanr_flag(adapt_engaged, "adapt_engaged")
  fixed_param <- .stanr_flag(fixed_param, "fixed_param")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (is.null(diagnostics) || identical(diagnostics, "")) {
    diagnostics <- character()
  } else {
    diagnostics <- match.arg(
      diagnostics,
      choices = c("divergences", "treedepth", "ebfmi"),
      several.ok = TRUE
    )
  }
  if (fixed_param && save_warmup) {
    warning(
      "`save_warmup` is ignored when `fixed_param = TRUE`.",
      call. = FALSE
    )
    save_warmup <- FALSE
  }
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  if (save_latent_dynamics) {
    stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
  }
  if (!is.null(metric_file)) {
    stop(
      "`metric_file` is not yet supported; supply `inv_metric` in memory.",
      call. = FALSE
    )
  }
  if (!engine %in% c("nuts", "static")) {
    stop("`engine` must be one of \"nuts\", \"static\".", call. = FALSE)
  }
  if (!metric %in% c("diag_e", "dense_e", "unit_e")) {
    stop(
      "`metric` must be one of \"diag_e\", \"dense_e\", \"unit_e\".",
      call. = FALSE
    )
  }
  ids <- .stanr_validate_chains(chains, chain_ids)
  chains <- ids$chains
  chain_ids <- ids$chain_ids
  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  iter_warmup <- .stanr_int(iter_warmup, "iter_warmup")
  iter_sampling <- .stanr_int(iter_sampling, "iter_sampling")
  thin <- .stanr_int(thin, "thin", min = 1L)
  # Calculate in double precision so adding two valid integer iteration counts
  # cannot overflow to NA before the explicit R-size guard below.
  num_saved_draws <- ceiling(as.double(iter_sampling) / as.double(thin))
  if (save_warmup) {
    num_saved_draws <- num_saved_draws +
      ceiling(as.double(iter_warmup) / as.double(thin))
  }
  if (num_saved_draws > .Machine$integer.max) {
    stop("Requested number of saved draws is too large.", call. = FALSE)
  }
  if (!fixed_param && engine == "static" && chains > 1L) {
    stop(
      "Static HMC only supports a single chain. Set `chains` = 1.",
      call. = FALSE
    )
  }
  inv_metric <- .stanr_normalize_inv_metric(
    inv_metric = inv_metric,
    metric = metric,
    chains = chains
  )
  refresh <- .stanr_int(refresh, "refresh")
  max_treedepth <- .stanr_int(max_treedepth, "max_treedepth")
  init_buffer <- .stanr_int(init_buffer, "init_buffer")
  term_buffer <- .stanr_int(term_buffer, "term_buffer")
  window <- .stanr_int(window, "window")

  diagnostic_vars <- if (!fixed_param && engine == "static") {
    c("accept_stat__", "stepsize__", "int_time__")
  } else {
    c(
      "accept_stat__",
      "stepsize__",
      "treedepth__",
      "n_leapfrog__",
      "divergent__",
      "energy__"
    )
  }

  native_args_fn <- function(seed, resolved_init, model) {
    native <- list(
      method = "sample",
      algorithm = if (fixed_param) "fixed_param" else "hmc",
      engine = if (fixed_param) "nuts" else engine,
      metric = metric,
      adapt_engaged = adapt_engaged,
      seed = seed,
      id = chain_ids[[1]],
      num_chains = chains,
      init_radius = resolved_init$radius,
      num_warmup = iter_warmup,
      num_samples = iter_sampling,
      thin = thin,
      save_warmup = save_warmup,
      refresh = refresh,
      stepsize = as.double(step_size),
      stepsize_jitter = as.double(step_size_jitter),
      max_depth = max_treedepth,
      int_time = as.double(int_time),
      delta = as.double(adapt_delta),
      gamma = as.double(adapt_gamma),
      kappa = as.double(adapt_kappa),
      t0 = as.double(adapt_t0),
      init_buffer = init_buffer,
      term_buffer = term_buffer,
      window = window,
      init = resolved_init$values,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      diagnostic_names = diagnostic_vars
    )
    if (!is.null(inv_metric)) {
      native$inv_metric <- inv_metric
    }
    native
  }

  payload_fn <- function(result) {
    if (result$return_code != 0) {
      list(draws = NULL, diagnostics = NULL)
    } else {
      draws <- posterior::as_draws_array(result$samples)
      diagnostics <- if (dim(result$diagnostics)[3] > 0) {
        posterior::as_draws_array(result$diagnostics)
      } else {
        posterior::draws_df(
          "stepsize__" = NA,
          "treedepth__" = NA,
          "n_leapfrog__" = NA,
          "divergent__" = NA,
          "energy__" = NA
        )
      }

      list(
        draws = draws,
        diagnostics = diagnostics,
        inv_metric = result$inv_metric,
        step_size = result$step_size
      )
    }
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanMCMC$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "sample",
      chains = ids$chains,
      chain_ids = ids$chain_ids,
      num_threads = num_threads,
      show_exceptions = show_exceptions,
      save_warmup = save_warmup,
      fixed_param = fixed_param,
      diagnostics = diagnostics
    )
  )
}
StanModel$set("public", "sample", stan_model_sample)

#' Run optimization
#'
#' @name model-method-optimize
#' @aliases optimize
#' @family StanModel methods
#'
#' @description The `$optimize()` method of a [`StanModel`] object runs Stan's
#'   `"optimize"` method to find the maximum a posteriori (MAP) estimate or
#'   maximum likelihood estimate (MLE). Returns a [`StanMLE`] object.
#'
#' @template param-data
#' @template param-init
#' @param algorithm (string) The optimization algorithm: `"lbfgs"`, `"bfgs"`,
#'   or `"newton"`.
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacobian of the inverse transformation?
#' @param init_alpha (number) Initial step size for LBFGS.
#' @param iter (integer) Maximum number of optimization iterations.
#' @param tol_obj (number) Absolute tolerance for changes in objective value.
#' @param tol_rel_obj (number) Relative tolerance for changes in objective value.
#' @param tol_grad (number) Absolute tolerance for the norm of the gradient.
#' @param tol_rel_grad (number) Relative tolerance for the norm of the gradient.
#' @param tol_param (number) Absolute tolerance for changes in parameter values.
#' @param history_size (integer) Number of corrections in LBFGS approximation.
#' @param save_iterations (logical) Should optimization iterations be saved?
#'   When `TRUE`, [`$draws()`][fit-method-draws] on the returned [`StanMLE`]
#'   exposes the full optimization path (one row per saved iteration,
#'   including the initial point) instead of a single row for the final
#'   estimate. [`$mle()`][fit-method-mle] is unaffected either way and always
#'   reflects the final iteration.
#' @param num_threads (integer) The total number of threads to use across all
#'   chains. Defaults to `RcppParallel::defaultNumThreads()` (all available threads).
#' @template param-opencl_ids
#'
#' @return A [`StanMLE`] object containing the point estimate.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$laplace()`][model-method-laplace]
#'
NULL

stan_model_optimize <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  algorithm = "lbfgs",
  jacobian = FALSE,
  init_alpha = 0.001,
  iter = 2000L,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  history_size = 5L,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_iterations = FALSE
) {
  jacobian <- .stanr_flag(jacobian, "jacobian")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  save_iterations <- .stanr_flag(save_iterations, "save_iterations")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  if (!algorithm %in% c("lbfgs", "bfgs", "newton")) {
    stop(
      "`algorithm` must be one of \"lbfgs\", \"bfgs\", \"newton\".",
      call. = FALSE
    )
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  refresh <- .stanr_int(refresh, "refresh")
  iter <- .stanr_int(iter, "iter")
  history_size <- .stanr_int(history_size, "history_size")

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "optimize",
      algorithm = algorithm,
      seed = seed,
      id = 1L,
      init_radius = resolved_init$radius,
      iter = iter,
      init_alpha = as.double(init_alpha),
      tol_obj = as.double(tol_obj),
      tol_rel_obj = as.double(tol_rel_obj),
      tol_grad = as.double(tol_grad),
      tol_rel_grad = as.double(tol_rel_grad),
      tol_param = as.double(tol_param),
      history_size = history_size,
      save_iterations = save_iterations,
      jacobian = jacobian,
      refresh = refresh,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    par_mat <- result$par
    par_vec <- if (is.matrix(par_mat) && nrow(par_mat) > 0) {
      par_mat[nrow(par_mat), , drop = TRUE]
    } else {
      numeric(0)
    }
    payload <- list(par = par_vec, value = result$value)
    if (save_iterations) {
      payload$iterations <- par_mat
    }
    payload
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanMLE$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "optimize",
      jacobian = jacobian,
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
StanModel$set("public", "optimize", stan_model_optimize)

#' Run Laplace approximation
#'
#' @name model-method-laplace
#' @aliases laplace
#' @family StanModel methods
#'
#' @description The `$laplace()` method of a [`StanModel`] object runs Stan's
#'   `"laplace"` method to draw from a Gaussian approximation to the posterior
#'   centered at the mode. Returns a [`StanLaplace`] object.
#'
#' @template param-data
#' @template param-init
#' @param mode A numeric vector of parameter values at the mode, or a
#'   [`StanMLE`] object from [`$optimize()`][model-method-optimize]. If `NULL`,
#'   optimization is run first.
#' @param opt_args (list) Additional arguments to pass to
#'   [`$optimize()`][model-method-optimize] when finding the mode.
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacobian of the inverse transformation?
#' @param draws (integer) The number of draws from the Laplace approximation.
#' @param calculate_lp (logical) Should the log density of the Laplace
#'   approximation be calculated?
#' @param num_threads (integer) The total number of threads to use across all
#'   chains. Defaults to `RcppParallel::defaultNumThreads()` (all available threads).
#' @template param-opencl_ids
#'
#' @return A [`StanLaplace`] object containing approximate posterior draws.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_laplace <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  mode = NULL,
  opt_args = NULL,
  jacobian = TRUE,
  draws = 1000L,
  show_messages = TRUE,
  show_exceptions = TRUE,
  calculate_lp = TRUE
) {
  if (!is.null(mode) && !is.null(opt_args)) {
    stop("`mode` and `opt_args` cannot both be supplied.", call. = FALSE)
  }
  reserved <- intersect(
    names(opt_args),
    c("data", "seed", "init", "jacobian", "show_messages", "show_exceptions")
  )
  if (length(reserved)) {
    stop(
      "`opt_args` cannot override: ",
      paste(reserved, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  jacobian <- .stanr_flag(jacobian, "jacobian")
  calculate_lp <- .stanr_flag(calculate_lp, "calculate_lp")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  # Seed is resolved once so the internal mode-finding optimize() run (if
  # any) and the laplace run itself share it.
  resolved_seed <- .stanr_seed(seed)

  mode_fit <- NULL
  if (is.null(mode)) {
    mode_fit <- do.call(
      self$optimize,
      c(
        list(
          data = data,
          seed = resolved_seed,
          init = init,
          jacobian = jacobian,
          show_messages = show_messages,
          show_exceptions = show_exceptions
        ),
        opt_args %||% list()
      )
    )
    mode_val <- mode_fit$mle()
  } else if (inherits(mode, "StanMLE")) {
    mode_fit <- mode
    mode_val <- mode_fit$mle()
  } else {
    # Numeric mode vector (stanr extension)
    mode_val <- mode
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  refresh <- .stanr_int(refresh, "refresh")
  draws <- .stanr_int(draws, "draws")

  if (!is.numeric(mode_val) || is.null(names(mode_val))) {
    stop(
      "mode must be a named numeric vector or an optimization result.",
      call. = FALSE
    )
  }

  native_args_fn <- function(seed, resolved_init, model) {
    pars <- self$constrained_param_names(model)
    mode_val <- mode_val[.stanr_bracket_names(pars)]
    if (anyNA(mode_val)) {
      stop(
        "mode must contain every constrained model parameter.",
        call. = FALSE
      )
    }
    list(
      method = "laplace",
      mode = as.double(mode_val),
      jacobian = jacobian,
      num_draws = draws,
      calculate_lp = calculate_lp,
      seed = seed,
      refresh = refresh,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads
    )
  }

  payload_fn <- function(result) {
    list(draws = posterior::as_draws_matrix(result$draws))
  }

  # `init` is not part of laplace's native args (the Laplace approximation is
  # centered at `mode`, not resolved via init), so the resolved default is
  # simply unused here.
  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = resolved_seed,
    init = NULL,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanLaplace$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = resolved_seed,
    init = init,
    elapsed = res$elapsed,
    mode = mode_fit %||% mode,
    metadata = list(
      method = "laplace",
      jacobian = jacobian,
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
StanModel$set("public", "laplace", stan_model_laplace)

#' Run variational inference (ADVI)
#'
#' @name model-method-variational
#' @aliases variational
#' @family StanModel methods
#'
#' @description The `$variational()` method of a [`StanModel`] object runs
#'   Stan's `"variational"` method, which uses Automatic Differentiation
#'   Variational Inference (ADVI) to approximate the posterior distribution.
#'   Returns a [`StanVB`] object.
#'
#' @template param-data
#' @template param-init
#' @param algorithm (string) The variational inference algorithm: `"meanfield"`
#'   or `"fullrank"`.
#' @param iter (integer) The number of ADVI iterations.
#' @param grad_samples (integer) Number of samples to use for gradient estimation.
#' @param elbo_samples (integer) Number of samples for ELBO evaluation.
#' @param eta (number) Learning rate for ADVI.
#' @param adapt_engaged (logical) Should the learning rate be adapted?
#' @param adapt_iter (integer) Number of iterations for learning rate adaptation.
#' @param tol_rel_obj (number) Relative tolerance for ELBO convergence.
#' @param eval_elbo (integer) How often to evaluate the ELBO.
#' @param draws (integer) The number of draws from the variational approximation.
#' @param num_threads (integer) The total number of threads to use across all
#'   chains. Defaults to `RcppParallel::defaultNumThreads()` (all available threads).
#' @template param-opencl_ids
#'
#' @return A [`StanVB`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$pathfinder()`][model-method-pathfinder]
#'
NULL

stan_model_variational <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  save_latent_dynamics = FALSE,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  algorithm = "meanfield",
  iter = 10000L,
  grad_samples = 1L,
  elbo_samples = 100L,
  eta = 1,
  adapt_engaged = TRUE,
  adapt_iter = 50L,
  tol_rel_obj = 0.01,
  eval_elbo = 100L,
  draws = 1000L,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  save_latent_dynamics <- .stanr_flag(
    save_latent_dynamics,
    "save_latent_dynamics"
  )
  adapt_engaged <- .stanr_flag(adapt_engaged, "adapt_engaged")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  if (save_latent_dynamics) {
    stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
  }
  if (!algorithm %in% c("meanfield", "fullrank")) {
    stop(
      "`algorithm` must be one of \"meanfield\", \"fullrank\".",
      call. = FALSE
    )
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  refresh <- .stanr_int(refresh, "refresh")
  iter <- .stanr_int(iter, "iter")
  grad_samples <- .stanr_int(grad_samples, "grad_samples")
  elbo_samples <- .stanr_int(elbo_samples, "elbo_samples")
  adapt_iter <- .stanr_int(adapt_iter, "adapt_iter")
  eval_elbo <- .stanr_int(eval_elbo, "eval_elbo")
  draws <- .stanr_int(draws, "draws")

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "variational",
      algorithm = algorithm,
      seed = seed,
      id = 1L,
      init_radius = resolved_init$radius,
      iter = iter,
      grad_samples = grad_samples,
      elbo_samples = elbo_samples,
      tol_rel_obj = as.double(tol_rel_obj),
      eta = as.double(eta),
      adapt_engaged = adapt_engaged,
      adapt_iter = adapt_iter,
      eval_elbo = eval_elbo,
      output_samples = draws,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    list(
      draws = if (result$return_code == 0L) {
        posterior::as_draws_matrix(result$draws)
      }
    )
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanVB$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "variational",
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
StanModel$set("public", "variational", stan_model_variational)

#' Run Pathfinder
#'
#' @name model-method-pathfinder
#' @aliases pathfinder
#' @family StanModel methods
#'
#' @description The `$pathfinder()` method of a [`StanModel`] object runs
#'   Stan's `"pathfinder"` method, which uses a parallel iterative optimization
#'   approach to approximate the posterior distribution. Returns a
#'   [`StanPathfinder`] object.
#'
#' @template param-data
#' @template param-init
#' @param num_paths (integer) The number of paths to use.
#' @param single_path_draws (integer) Number of draws per path.
#' @param draws (integer) Total number of draws from the approximation.
#' @param max_lbfgs_iters (integer) Maximum LBFGS iterations per path.
#' @param num_elbo_draws (integer) Number of draws for ELBO estimation.
#' @param save_single_paths (logical) Should single path results be saved?
#' @param psis_resample (logical) Should Pareto smoothed importance sampling
#'   resampling be used?
#' @param calculate_lp (logical) Should the log density be calculated?
#' @param num_threads (integer) The total number of threads to use across all
#'   chains. Defaults to `RcppParallel::defaultNumThreads()` (all available threads).
#' @template param-opencl_ids
#'
#' @return A [`StanPathfinder`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_pathfinder <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  init_alpha = 0.001,
  tol_obj = 1e-12,
  tol_rel_obj = 1e4,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  history_size = 5L,
  single_path_draws = 1000L,
  draws = 1000L,
  num_paths = 4,
  max_lbfgs_iters = 1000L,
  num_elbo_draws = 25L,
  save_single_paths = FALSE,
  psis_resample = TRUE,
  calculate_lp = TRUE,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  save_single_paths <- .stanr_flag(save_single_paths, "save_single_paths")
  psis_resample <- .stanr_flag(psis_resample, "psis_resample")
  calculate_lp <- .stanr_flag(calculate_lp, "calculate_lp")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  refresh <- .stanr_int(refresh, "refresh")
  num_paths <- .stanr_int(num_paths, "num_paths", min = 1L)
  single_path_draws <- .stanr_int(single_path_draws, "single_path_draws")
  draws <- .stanr_int(draws, "draws")
  max_lbfgs_iters <- .stanr_int(max_lbfgs_iters, "max_lbfgs_iters")
  num_elbo_draws <- .stanr_int(num_elbo_draws, "num_elbo_draws")
  history_size <- .stanr_int(history_size, "history_size")

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "pathfinder",
      seed = seed,
      id = 1L,
      init_radius = resolved_init$radius,
      max_lbfgs_iters = max_lbfgs_iters,
      history_size = history_size,
      num_elbo_draws = num_elbo_draws,
      num_draws = single_path_draws,
      num_paths = num_paths,
      num_psis_draws = draws,
      init_alpha = as.double(init_alpha),
      tol_obj = as.double(tol_obj),
      tol_rel_obj = as.double(tol_rel_obj),
      tol_grad = as.double(tol_grad),
      tol_rel_grad = as.double(tol_rel_grad),
      tol_param = as.double(tol_param),
      save_single_paths = save_single_paths,
      psis_resample = psis_resample,
      calculate_lp = calculate_lp,
      refresh = refresh,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    if (result$return_code != 0) {
      return(list(draws = NULL))
    }
    draws <- posterior::as_draws_matrix(result$draws)
    special_vars <- c("lp_approx__", "lp__", "path__")
    present <- intersect(special_vars, colnames(result$draws))
    diagnostics <- if (length(present)) {
      posterior::subset_draws(draws, variable = present)
    }
    list(draws = draws, diagnostics = diagnostics)
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanPathfinder$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "pathfinder",
      num_paths = num_paths,
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
StanModel$set("public", "pathfinder", stan_model_pathfinder)

#' Run generated quantities
#'
#' @name model-method-generate-quantities
#' @aliases generate_quantities
#' @family StanModel methods
#'
#' @description The `$generate_quantities()` method of a [`StanModel`] object
#'   runs Stan's `"generate quantities"` method, which evaluates the generated
#'   quantities block of the Stan program for a set of parameter values.
#'   Returns a [`StanGQ`] object.
#'
#' @param fitted_params A [`StanFit`] object, or anything accepted by
#'   [posterior::as_draws_matrix()], containing draws with every model
#'   parameter present by name (e.g. `beta[1]`, not a positional column).
#' @template param-data
#' @param num_threads (integer) The total number of threads to use across all
#'   chains. Defaults to `RcppParallel::defaultNumThreads()` (all available threads).
#' @template param-opencl_ids
#'
#' @return A [`StanGQ`] object containing the generated quantities.
#'
#' @seealso [`$sample()`][model-method-sample]
#'
NULL

stan_model_generate_quantities <- function(
  fitted_params,
  data = list(),
  seed = NULL,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)

  input <- if (inherits(fitted_params, "StanFit")) {
    fitted_params$draws(format = "draws_matrix")
  } else {
    posterior::as_draws_matrix(fitted_params)
  }

  native_args_fn <- function(seed, resolved_init, model) {
    pars <- .stanr_bracket_names(self$constrained_param_names(model))
    draws_matrix <- posterior::as_draws_matrix(
      posterior::subset_draws(input, variable = pars)
    )

    list(
      method = "generate_quantities",
      seed = seed,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      draws = draws_matrix
    )
  }

  payload_fn <- function(result) {
    list(draws = posterior::as_draws_array(result$samples))
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = NULL,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanGQ$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = NULL,
    elapsed = res$elapsed,
    metadata = list(
      method = "generate_quantities",
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
StanModel$set("public", "generate_quantities", stan_model_generate_quantities)

#' Run gradient diagnostics
#'
#' @name model-method-diagnose
#' @aliases diagnose
#' @family StanModel methods
#'
#' @description The `$diagnose()` method of a [`StanModel`] object runs Stan's
#'   `"diagnose"` method to check the correctness of gradients computed by
#'   Stan. Returns a [`StanDiagnose`] object.
#'
#' @param epsilon (number) The finite difference step size.
#' @param error (number) The maximum allowed relative error.
#'
#' @return A [`StanDiagnose`] object containing gradient check results.
#'
#' @seealso [`$sample()`][model-method-sample]
#'
NULL

stan_model_diagnose <- function(
  data = list(),
  seed = NULL,
  init = 2,
  epsilon = 1e-6,
  error = 1e-6,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")

  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0) {
    stop("`epsilon` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(error) || length(error) != 1L || error <= 0) {
    stop("`error` must be a positive number.", call. = FALSE)
  }

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "diagnose",
      epsilon = as.double(epsilon),
      error = as.double(error),
      seed = as.integer(seed),
      id = 1L,
      init_radius = resolved_init$radius,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = 1L,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    list(
      num_failed = as.integer(result$num_failed),
      gradients = data.frame(
        param_idx = seq_along(result$value) - 1L,
        value = result$value,
        model = result$model,
        finite_diff = result$finite_diff,
        error = result$error,
        check.names = FALSE
      ),
      lp = result$lp
    )
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  if (res$payload$num_failed == 0L) {
    message("[stanr] All gradient tests passed.")
  } else {
    message(sprintf(
      "[stanr] %d parameter(s) failed the gradient test.",
      res$payload$num_failed
    ))
  }

  StanDiagnose$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "diagnose",
      epsilon = epsilon,
      error = error
    )
  )
}
StanModel$set("public", "diagnose", stan_model_diagnose)

# StanModel internal native entry points ---------------------------------------
# These remain public because sourceCpp functions live in a model-specific
# environment. They are not the user-facing API.

stan_model_new_model <- function(data, seed) {
  private$ensure_compiled()
  private$compiled_env_$new_model(data, seed)
}
StanModel$set("public", "new_model", stan_model_new_model)

stan_model_run_model <- function(model, args) {
  private$ensure_compiled()
  private$compiled_env_$run_model(model, args)
}
StanModel$set("public", "run_model", stan_model_run_model)

stan_model_constrained_param_names <- function(model) {
  private$ensure_compiled()
  private$compiled_env_$constrained_param_names(model)
}
StanModel$set(
  "public",
  "constrained_param_names",
  stan_model_constrained_param_names
)

stan_model_native_function <- function(name, required = TRUE) {
  private$ensure_compiled()
  fun <- private$compiled_env_[[name]]
  if (is.null(fun) && required) {
    stop(
      "The compiled model does not provide native function `",
      name,
      "`; recompile the model with the current stanr version.",
      call. = FALSE
    )
  }
  fun
}
StanModel$set("public", "native_function", stan_model_native_function)
