#' Post-process standalone-functions C++ into an `Rcpp::sourceCpp()` unit
#'
#' Rewrites the `// [[stan::function]]` wrappers emitted by
#' `stanc(code, standalone_functions = TRUE)` into `// [[Rcpp::export]]`
#' wrappers callable from R. The `pstream__` and (for `_rng` functions)
#' `base_rng__` parameters are stripped from each signature -- they become
#' file-static variables instead -- and a function registry is appended so
#' the exposed set can be discovered on a cache hit without rerunning stanc.
#'
#' @param cpp_code The C++ text returned by
#'   `stanc(code, standalone_functions = TRUE)`.
#' @param reserved_names Character vector of function names that must not be
#'   exposed; a collision is an error, not a skip.
#'
#' @return A list with elements:
#'   * `full_code` -- the entire processed translation unit, usable as a
#'     standalone compile unit.
#'   * `wrapper_section` -- the RNG/registry prelude and rewritten wrappers
#'     only, without the leading `model_header.hpp` include and
#'     `model_namespace` block (for appending after a model's own generated
#'     C++, which already provides those definitions).
#'   * `functions` -- a data frame with columns `name` and `is_rng`, one row
#'     per exposed function, in exposure order.
#' @noRd
.stanr_process_standalone_cpp <- function(cpp_code, reserved_names) {
  lines <- strsplit(cpp_code, "\n", fixed = TRUE)[[1]]
  marker_idx <- which(trimws(lines) == "// [[stan::function]]")
  if (length(marker_idx) == 0L) {
    stop(
      "Stan program has no functions that can be exposed as R functions.",
      call. = FALSE
    )
  }

  preamble <- if (marker_idx[[1]] > 1L) {
    lines[seq_len(marker_idx[[1]] - 1L)]
  } else {
    character()
  }

  wrappers <- list()

  for (i in seq_along(marker_idx)) {
    start <- marker_idx[[i]] + 1L
    end <- if (i < length(marker_idx)) {
      marker_idx[[i + 1L]] - 1L
    } else {
      length(lines)
    }
    block <- lines[start:end]

    right_trimmed <- sub("\\s+$", "", block)
    last_char <- substr(
      right_trimmed,
      nchar(right_trimmed),
      nchar(right_trimmed)
    )
    sig_end <- which(last_char == "{")
    if (!length(sig_end)) {
      stop(
        "Malformed stanc standalone-functions output: no wrapper signature ",
        "found after a `// [[stan::function]]` marker.",
        call. = FALSE
      )
    }
    sig_end <- sig_end[[1]]

    signature <- gsub(
      "\\s+",
      " ",
      paste(trimws(block[seq_len(sig_end)]), collapse = " ")
    )

    depth <- 1L
    body_end <- NA_integer_
    for (j in seq(sig_end + 1L, length(block))) {
      depth <- depth +
        nchar(gsub("[^{]", "", block[[j]])) -
        nchar(gsub("[^}]", "", block[[j]]))
      if (depth == 0L) {
        body_end <- j
        break
      }
    }
    if (is.na(body_end)) {
      stop(
        "Malformed stanc standalone-functions output: unterminated wrapper ",
        "body after a `// [[stan::function]]` marker.",
        call. = FALSE
      )
    }
    body <- block[seq(sig_end + 1L, body_end)]

    paren_pos <- regexpr("(", signature, fixed = TRUE)[[1]]
    before_paren <- trimws(substr(signature, 1L, paren_pos - 1L))
    name <- regmatches(before_paren, regexpr("[A-Za-z0-9_]+$", before_paren))

    if (name %in% reserved_names) {
      stop(
        "Stan function `",
        name,
        "` collides with a reserved/internal stanr export name; rename ",
        "the Stan function to expose it.",
        call. = FALSE
      )
    }

    # base_rng__ is stripped before pstream__: it always sits immediately
    # before pstream__, so removing it first (with its own optional leading
    # comma) leaves pstream__'s comma handling correct whether or not other
    # arguments precede it.
    is_rng <- grepl(",?\\s*stan::rng_t&\\s*base_rng__", signature)
    signature <- sub(",?\\s*stan::rng_t&\\s*base_rng__", "", signature)
    signature <- sub(
      ",?\\s*std::ostream\\s*\\*\\s*pstream__\\s*=\\s*nullptr",
      "",
      signature
    )

    wrappers[[length(wrappers) + 1L]] <- list(
      name = name,
      is_rng = is_rng,
      signature = signature,
      body = body
    )
  }

  names_vec <- vapply(wrappers, `[[`, character(1), "name")
  is_dup <- duplicated(names_vec)
  dropped_duplicate <- unique(names_vec[is_dup])
  wrappers <- wrappers[!is_dup]

  if (length(dropped_duplicate)) {
    warning(
      "Stan function(s) with duplicate name(s) after overload resolution; ",
      "only the first overload of each is exposed: ",
      paste(dropped_duplicate, collapse = ", "),
      call. = FALSE
    )
  }

  surv_names <- vapply(wrappers, `[[`, character(1), "name")
  surv_is_rng <- vapply(wrappers, `[[`, logical(1), "is_rng")

  wrapper_blocks <- vapply(
    wrappers,
    function(w) {
      paste(c("// [[Rcpp::export]]", w$signature, w$body), collapse = "\n")
    },
    character(1)
  )

  # 1234/0 are placeholder seed/chain: the real per-session seed is set from
  # R via stanr_rng_set_seed() once the compiled functions are exposed.
  # RcppEigen.h (not plain Rcpp.h) is required here: wrappers with
  # vector/matrix/row_vector args or returns are exported as
  # `Eigen::Matrix<...>`, and only RcppEigen.h provides the `Rcpp::as()`/
  # `Rcpp::wrap()` specializations that marshal those to/from SEXP.
  # stanr/rcpp_tuple_interop.hpp adds the Rcpp::wrap()/Exporter overloads
  # for std::tuple (and std::vector<tuple> nestings) that Stan's tuple
  # wrappers need; it must come after RcppEigen.h/Rcpp.h (both TU modes
  # satisfy this).
  # The `Rcpp::depends` attributes (matching inst/stan_model.cpp's own) are
  # what let `Rcpp::sourceCpp()` resolve RcppEigen/BH/RcppParallel include
  # paths itself; harmless if this ends up appended after inst/stan_model.cpp
  # (combined-TU mode), which already declares the same three.
  prelude <- paste(
    c(
      "#include <RcppEigen.h>",
      "#include <stanr/rcpp_tuple_interop.hpp>",
      "// [[Rcpp::depends(BH)]]",
      "// [[Rcpp::depends(RcppEigen)]]",
      "// [[Rcpp::depends(RcppParallel)]]",
      "static stan::rng_t base_rng__ = stan::services::util::create_rng(1234, 0);",
      "static std::ostream* pstream__ = &Rcpp::Rcout;",
      "// [[Rcpp::export]]",
      "void stanr_rng_set_seed(int seed) {",
      "  base_rng__ = stan::services::util::create_rng(static_cast<unsigned int>(seed), 0);",
      "}"
    ),
    collapse = "\n"
  )

  registry <- paste(
    c(
      "// [[Rcpp::export]]",
      "Rcpp::List stanr_exposed_functions() {",
      "  return Rcpp::List::create(",
      paste0(
        '    Rcpp::Named("name") = Rcpp::CharacterVector::create(',
        paste(sprintf('"%s"', surv_names), collapse = ", "),
        "),"
      ),
      paste0(
        '    Rcpp::Named("is_rng") = Rcpp::LogicalVector::create(',
        paste(ifelse(surv_is_rng, "true", "false"), collapse = ", "),
        ")"
      ),
      "  );",
      "}"
    ),
    collapse = "\n"
  )

  wrapper_section <- paste(
    c(prelude, wrapper_blocks, registry),
    collapse = "\n"
  )
  full_code <- paste(
    paste(preamble, collapse = "\n"),
    wrapper_section,
    sep = "\n"
  )

  list(
    full_code = full_code,
    wrapper_section = wrapper_section,
    functions = data.frame(name = surv_names, is_rng = surv_is_rng)
  )
}

#' Compile a Stan program's `functions` block into its own translation unit
#'
#' Mirrors `.compile_stan_model_environment()` (R/stan_model.R), but for the
#' separate-TU expose path: no `model_name` messaging, no OpenCL, no
#' `libstanr_runner.a` (this TU has no services to link against).
#'
#' @param code Stan program source, already `#include`-resolved by the
#'   caller (e.g. a model's `resolved_code()`).
#' @param external_cpp `NULL`, or a character vector of paths, forwarded to
#'   `stanc()`.
#' @param cpp_options C++ compilation options; see `stan_model()`.
#' @param verbose Print compiler/cache progress messages.
#' @param precompiled_headers Reuse the model-PCH `.gch` when flags match.
#'
#' @return An environment populated by `Rcpp::sourceCpp()`, with
#'   `stanr_exposed_functions`, `stanr_rng_set_seed`, and one R function
#'   per exposed Stan function.
#' @noRd
.compile_standalone_functions_environment <- function(
  code,
  external_cpp = NULL,
  cpp_options = list(),
  verbose = FALSE,
  precompiled_headers = TRUE
) {
  .stanr_require_compile_packages()

  # Only ever used for this separate-TU path; the combined-TU call site in
  # `.compile_stan_model_environment()` builds its own larger
  # reserved_names set.
  reserved_names <- c("stanr_exposed_functions", "stanr_rng_set_seed")

  stanc_out <- stanc(
    code,
    standalone_functions = TRUE,
    external_cpp = external_cpp
  )
  processed <- .stanr_process_standalone_cpp(stanc_out, reserved_names)

  # OPENCL_LIBS is meaningless here -- this TU never uses OpenCL.
  cpp_option_assignments <- .stanr_parse_cpp_options(cpp_options)
  extra_assignments <- Filter(
    function(a) !identical(a$name, "OPENCL_LIBS"),
    cpp_option_assignments
  )

  # Unlike the model hash, this hashes the post-processed *output* rather
  # than pre-stanc inputs: there's no expensive compiler/filesystem work
  # being skipped by avoiding the stanc()/post-processing call, only the
  # actual C++ compile is cache-gated.
  functions_hash <- digest::digest(
    c(
      processed$full_code,
      as.character(utils::packageVersion("stanr")),
      .stanr_stan_version(),
      R.version$platform,
      .stanr_compiler_identity(),
      .stanr_cpp_options_hash_component(extra_assignments)
    ),
    algo = "xxhash64"
  )

  # Functions .cpp files live alongside model .cpp files in the same cache
  # dir, distinguished only by filename prefix.
  cache_dir <- .stanr_models_cache_dir()
  cpp_file <- file.path(cache_dir, paste0("functions_", functions_hash, ".cpp"))
  if (!file.exists(cpp_file)) {
    if (verbose) {
      message("[stanr] Compiling Stan functions...")
    }
    writeLines(processed$full_code, cpp_file)
  }

  base_cppflags <- .stanr_base_cppflags()
  pch_enabled <- FALSE
  if (
    precompiled_headers &&
      length(external_cpp) == 0 &&
      !.stanr_cpp_options_block_pch(extra_assignments)
  ) {
    pch_flags <- .stanr_pch_flags(base_cppflags, verbose)
    pch_enabled <- nzchar(pch_flags)
    cppflags <- paste(pch_flags, base_cppflags)
  } else {
    cppflags <- base_cppflags
  }

  env <- new.env()

  compile_functions <- function(compilation_cppflags) {
    .stanr_sourcecpp(
      cpp_file = cpp_file,
      env = env,
      cppflags = compilation_cppflags,
      libs = .stanr_tbb_libs(),
      extra_assignments = extra_assignments,
      rebuild = FALSE,
      cache_dir = cache_dir,
      verbose = verbose
    )
  }

  .stanr_compile_with_pch_retry(
    compile_functions,
    cppflags,
    base_cppflags,
    pch_enabled,
    verbose
  )

  env
}

#' Wrap a compiled `_rng` export with an explicit `seed` argument
#'
#' The compiled export `fn` already has `pstream__`/`base_rng__` stripped
#' from its C++ signature by `.stanr_process_standalone_cpp()`, so its R
#' formals are exactly the user-facing Stan args. The wrapper's formals are
#' copied from `fn` (rather than written as `function(..., seed = NULL)`) so
#' `args()`/autocomplete on the exposed function shows real parameter names.
#'
#' @param fn A compiled `_rng` export.
#' @param compiled_env The environment `fn` was sourced into (provides
#'   `stanr_rng_set_seed()`).
#' @return A function with `fn`'s formals plus a trailing `seed = NULL`.
#' @noRd
.stanr_rng_wrapper <- function(fn, compiled_env) {
  base_formals <- formals(fn)
  arg_names <- names(base_formals) %||% character()
  wrapper <- function(seed = NULL) {
    if (!is.null(seed)) {
      compiled_env$stanr_rng_set_seed(seed)
    }
    do.call(fn, mget(arg_names, envir = environment()))
  }
  formals(wrapper) <- c(base_formals, alist(seed = NULL))
  wrapper
}

#' Populate a target environment from a compiled functions environment
#'
#' Shared by both expose paths (separate-TU and combined-TU): reads the
#' function registry off `compiled_env`, wraps
#' `_rng` exports with `.stanr_rng_wrapper()`, and assigns everything into
#' `target_env` (and, if `global`, also into `global_env`).
#'
#' @param compiled_env An environment as returned by
#'   `.compile_standalone_functions_environment()`.
#' @param target_env The environment to (re)populate, e.g. a model's
#'   `$functions` env.
#' @param global Also assign each function into `global_env`.
#' @param global_env Environment to assign into when `global` is `TRUE`.
#'
#' @return `target_env`, invisibly.
#' @noRd
.stanr_build_functions_env <- function(
  compiled_env,
  target_env,
  global,
  global_env = globalenv()
) {
  registry <- compiled_env$stanr_exposed_functions()

  # Cleared first so re-exposing after a model recompile (functions block
  # changed) doesn't leave stale bindings from the previous compile.
  rm(list = ls(target_env), envir = target_env)

  if (any(registry$is_rng)) {
    compiled_env$stanr_rng_set_seed(sample.int(.Machine$integer.max, 1))
  }

  for (i in seq_along(registry$name)) {
    name <- registry$name[[i]]
    fn <- compiled_env[[name]]
    value <- if (registry$is_rng[[i]]) {
      .stanr_rng_wrapper(fn, compiled_env)
    } else {
      fn
    }
    assign(name, value, envir = target_env)
    if (global) {
      assign(name, value, envir = global_env)
    }
  }

  invisible(target_env)
}
