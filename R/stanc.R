#' Resolve Stan `#include` directives
#'
#' @param model_code A Stan program as a single string.
#' @param include_directories Character vector of directories to search, in order.
#' @param include_stack Paths included while resolving the current program.
#'
#' @return The Stan program with matching include directives replaced by their contents.
#' @keywords internal
resolve_stan_includes <- function(
  model_code,
  include_directories,
  include_stack = character()
) {
  lines <- strsplit(model_code, "\n", fixed = TRUE)[[1]]

  resolved_lines <- vapply(
    lines,
    function(line) {
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

      candidates <- file.path(include_directories, include_name)
      match_index <- which(file.exists(candidates) & !dir.exists(candidates))[1]
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

      include_path <- normalizePath(candidates[match_index], mustWork = TRUE)
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
    },
    character(1),
    USE.NAMES = FALSE
  )

  paste(resolved_lines, collapse = "\n")
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

  if (is.null(external_cpp)) {
    external_cpp <- character()
  }
  if (!is.character(external_cpp) || anyNA(external_cpp)) {
    stop(
      "external_cpp must be NULL or a character vector of file paths.",
      call. = FALSE
    )
  }
  missing_external_cpp <- !file.exists(external_cpp) | dir.exists(external_cpp)
  if (any(missing_external_cpp)) {
    stop(
      "External C++ files do not exist or are directories: ",
      paste(shQuote(external_cpp[missing_external_cpp]), collapse = ", "),
      call. = FALSE
    )
  }
  external_cpp_code <- if (length(external_cpp) == 0) {
    NULL
  } else {
    paste(
      vapply(
        external_cpp,
        function(path) {
          paste(readLines(path, warn = FALSE), collapse = "\n")
        },
        character(1)
      ),
      collapse = "\n"
    )
  }

  stanc_flags <- c(
    ifelse(allow_undefined || length(external_cpp) > 0, "allow-undefined", ""),
    paste0("O", optim_level),
    ifelse(standalone_functions, "standalone-functions", ""),
    ifelse(use_opencl, "use-opencl", ""),
    ifelse(warn_pedantic, "warn-pedantic", ""),
    ifelse(warn_uninitialized, "warn-uninitialized", "")
  )

  res <- stanc_context$call(
    "stanc",
    "model",
    model_code,
    as.array(stanc_flags[!(stanc_flags == "")])
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

#' Extract variable metadata from Stan code using stanc info output
#'
#' @param model_code Stan model code as a single string.
#' @param include_directories Character vector of directories to search for
#'   Stan include files.
#' @param allow_undefined Allow undefined functions.
#'
#' @return A named list with elements `data`, `parameters`,
#'   `transformed_parameters`, and `generated_quantities`. Each element is a
#'   named list of variables, where each variable has `type` and `dimensions`.
#' @keywords internal
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

  res <- stanc_context$call(
    "stanc",
    "model",
    model_code,
    as.array(flags)
  )

  if (!is.null(res$errors)) {
    stop(paste(res$errors, collapse = "\n"), call. = FALSE)
  }

  variables <- jsonlite::fromJSON(res$result)
  variables$data <- variables$inputs
  variables$inputs <- NULL
  variables$transformed_parameters <- variables[["transformed parameters"]]
  variables[["transformed parameters"]] <- NULL
  variables$generated_quantities <- variables[["generated quantities"]]
  variables[["generated quantities"]] <- NULL
  variables$functions <- NULL
  variables$distributions <- NULL
  variables$included_files <- NULL
  # Reorder to match expected output
  variables[c(
    "data",
    "parameters",
    "transformed_parameters",
    "generated_quantities"
  )]
}
