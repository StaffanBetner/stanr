# Lazily created QuickJSR context hosting `stanc.js` (3.1 MB, so only
# created on first use), memoized for the session.
stanc_ctx <- function() {
  if (is.null(.stanr_memo$stanc_context)) {
    use_quickjsr <- isTRUE(getOption("stanr_use_quickjsr", FALSE)) ||
      !identical(Sys.getenv("STANR_USE_QUICKJSR", ""), "")
    if (requireNamespace("V8", quietly = TRUE) && !use_quickjsr) {
      ctx <- V8::v8()
    } else {
      ctx <- QuickJSR::JSContext$new(stack_size = 4 * 1024 * 1024)
    }
    ctx$source(system.file("stanc.js", package = "stanr", mustWork = TRUE))
    .stanr_memo$stanc_context <- ctx
  }
  .stanr_memo$stanc_context
}

# Resolves Stan `#include` directives from `include_directories` (searched
# in order). `include_stack` catches circular includes.
resolve_stan_includes <- function(
  model_code,
  include_directories,
  include_stack = character()
) {
  lines <- strsplit(model_code, "\n", fixed = TRUE)[[1]]

  candidates <- grepl("#include", lines, fixed = TRUE)
  if (!any(candidates)) {
    return(model_code)
  }

  resolve_line <- function(line) {
    match <- regexec(
      "^[[:space:]]*#include[[:space:]]+(.+?)[[:space:]]*$",
      line,
      perl = TRUE
    )
    parts <- regmatches(line, match)[[1]]
    if (length(parts) == 0) {
      return(line)
    }

    include_name <- trimws(sub("[[:space:]]*//.*$", "", parts[2]))
    include_name <- sub('^"([^"]+)"$', "\\1", include_name)
    include_name <- sub("^<([^>]+)>$", "\\1", include_name)
    if (include_name == "" || grepl("[[:space:]]", include_name)) {
      return(line)
    }

    include_candidates <- file.path(include_directories, include_name)
    match_index <- which(
      file.exists(include_candidates) & !dir.exists(include_candidates)
    )[1]
    if (is.na(match_index)) {
      searched <- if (length(include_directories) == 0) {
        "no include directories"
      } else {
        paste(shQuote(include_directories), collapse = ", ")
      }
      stop(
        "Could not find Stan include file '",
        include_name,
        "'. Searched ",
        searched,
        ".",
        call. = FALSE
      )
    }

    include_path <- normalizePath(
      include_candidates[match_index],
      mustWork = TRUE
    )
    if (include_path %in% include_stack) {
      stop(
        "Circular Stan include detected: ",
        paste(c(include_stack, include_path), collapse = " -> "),
        call. = FALSE
      )
    }

    resolve_stan_includes(
      paste(readLines(include_path, warn = FALSE), collapse = "\n"),
      include_directories = include_directories,
      include_stack = c(include_stack, include_path)
    )
  }

  lines[candidates] <- vapply(
    lines[candidates],
    resolve_line,
    character(1),
    USE.NAMES = FALSE
  )

  paste(lines, collapse = "\n")
}

# Reads `external_cpp` file contents (one per path, in order).
.stanr_external_cpp_contents <- function(paths) {
  if (is.null(paths)) {
    return(character())
  }
  if (!is.character(paths) || anyNA(paths)) {
    stop(
      "external_cpp must be NULL or a character vector of file paths.",
      call. = FALSE
    )
  }
  bad <- !file.exists(paths) | dir.exists(paths)
  if (any(bad)) {
    stop(
      "External C++ files do not exist or are directories: ",
      paste(shQuote(paths[bad]), collapse = ", "),
      call. = FALSE
    )
  }
  vapply(
    paths,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
}

#' Compile Stan code to C++
#'
#' Compiles a Stan program with the bundled `stanc3` compiler. Before
#' compilation, `#include` directives are replaced with the contents of the
#' matching file from `include_directories`. Includes may be nested; directories
#' are searched in the supplied order.
#'
#' @param model_code Stan model code as a single string.
#' @param include_directories Character vector of directories to search for Stan
#'   include files. Defaults to no include directories.
#' @param standalone_functions Generate standalone function definitions.
#' @param use_opencl Enable OpenCL code generation.
#' @param optim_level stanc optimization level.
#' @param allow_undefined Allow undefined functions.
#' @param warn_pedantic Request pedantic compiler warnings.
#' @param warn_uninitialized Request uninitialized-variable warnings.
#' @param external_cpp `NULL` (the default), or a character vector of paths to
#'   C++ source or header files. Their contents are prepended to the generated
#'   C++, in order. Supplying this argument enables `allow_undefined` so Stan
#'   functions implemented by the supplied C++ can be declared in the model.
#'
#' @return A string containing the generated C++ model implementation.
#' @export
stanc <- function(
  model_code,
  include_directories = character(),
  standalone_functions = FALSE,
  use_opencl = FALSE,
  optim_level = 0,
  allow_undefined = FALSE,
  warn_pedantic = FALSE,
  warn_uninitialized = FALSE,
  external_cpp = NULL
) {
  if (
    !is.character(model_code) || length(model_code) != 1 || is.na(model_code)
  ) {
    stop("model_code must be a single, non-missing string.", call. = FALSE)
  }
  if (model_code == "") {
    stop("No model code provided!", call. = FALSE)
  }

  if (is.null(include_directories)) {
    include_directories <- character()
  }
  if (!is.character(include_directories) || anyNA(include_directories)) {
    stop("include_directories must be a character vector.", call. = FALSE)
  }
  if (length(include_directories) > 0) {
    include_directories <- normalizePath(include_directories, mustWork = FALSE)
    missing_directories <- !dir.exists(include_directories)
    if (any(missing_directories)) {
      stop(
        "Include directories do not exist: ",
        paste(
          shQuote(include_directories[missing_directories]),
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }
  model_code <- resolve_stan_includes(model_code, include_directories)

  contents <- .stanr_external_cpp_contents(external_cpp)
  external_cpp_code <- if (length(contents) == 0) {
    NULL
  } else {
    paste(contents, collapse = "\n")
  }

  stanc_flags <- c(
    if (allow_undefined || length(external_cpp) > 0) "allow-undefined",
    paste0("O", optim_level),
    if (standalone_functions) "standalone-functions",
    if (use_opencl) "use-opencl",
    if (warn_pedantic) "warn-pedantic",
    if (warn_uninitialized) "warn-uninitialized"
  )

  res <- stanc_ctx()$call(
    "stanc",
    "model",
    model_code,
    as.array(stanc_flags)
  )

  if (!is.null(res$errors)) {
    errors <- paste(res$errors, collapse = "\n")
    stop(errors, call. = FALSE)
  }

  if (length(res$warnings) > 0) {
    warnings <- paste(res$warnings, collapse = "\n")
    warning(warnings, call. = FALSE)
  }
  if (is.null(external_cpp_code)) {
    res$result
  } else {
    paste(external_cpp_code, res$result, sep = "\n")
  }
}

# Collapses a tuple's `{type, dimensions}` slot array (QuickJSR::from_json
# parses it as a plain nested list) into the type/dimensions data.frame
# `src/r_data_context.cpp` expects.
.stanr_normalize_type <- function(type) {
  if (!is.list(type)) {
    return(type)
  }
  dims <- vapply(type, function(slot) as.integer(slot$dimensions), integer(1))
  types <- lapply(type, function(slot) .stanr_normalize_type(slot$type))
  if (any(vapply(types, is.list, logical(1)))) {
    out <- data.frame(dimensions = dims)
    out$type <- types
    return(out[c("type", "dimensions")])
  }
  data.frame(type = unlist(types), dimensions = dims, stringsAsFactors = FALSE)
}

.stanr_normalize_variables <- function(vars) {
  lapply(vars, function(v) {
    v$type <- .stanr_normalize_type(v$type)
    v
  })
}

# Variable metadata from stanc's info output.
model_variables <- function(
  model_code,
  include_directories = character(),
  allow_undefined = FALSE
) {
  model_code <- resolve_stan_includes(model_code, include_directories)

  flags <- c(
    "info",
    if (allow_undefined) "allow-undefined"
  )

  res <- stanc_ctx()$call(
    "stanc",
    "model",
    model_code,
    as.array(flags)
  )

  if (!is.null(res$errors)) {
    stop(paste(res$errors, collapse = "\n"), call. = FALSE)
  }

  variables <- QuickJSR::from_json(res$result)
  variables$data <- variables$inputs
  variables$inputs <- NULL
  variables$transformed_parameters <- variables[["transformed parameters"]]
  variables[["transformed parameters"]] <- NULL
  variables$generated_quantities <- variables[["generated quantities"]]
  variables[["generated quantities"]] <- NULL
  groups <- c(
    "data",
    "parameters",
    "transformed_parameters",
    "generated_quantities"
  )
  variables[groups] <- lapply(variables[groups], .stanr_normalize_variables)
  variables[groups]
}

# Reformats Stan code via stanc's auto-formatter.
stanc_format <- function(
  model_code,
  canonicalize = FALSE,
  max_line_length = NULL
) {
  flags <- "auto-format"
  if (isTRUE(canonicalize)) {
    flags <- c(flags, "print-canonical")
  } else if (is.character(canonicalize)) {
    flags <- c(
      flags,
      paste0("canonicalize=", paste(canonicalize, collapse = ","))
    )
  }
  if (!is.null(max_line_length)) {
    flags <- c(flags, paste0("max-line-length=", max_line_length))
  }

  res <- stanc_ctx()$call(
    "stanc",
    "model",
    model_code,
    as.array(flags)
  )

  if (!is.null(res$errors)) {
    stop(paste(res$errors, collapse = "\n"), call. = FALSE)
  }

  res$result
}
