#' Compile and load a Stan model
#'
#' @param file Path to a `.stan` file (or `NULL` if `code` is provided)
#' @param code Stan model code as a string (alternative to `file`)
#' @param model_name Override model name (default: basename of `file` without `.stan`)
#' @param data Named list of data variables
#' @param init Initial values for parameters (numeric vector, function, or `"random"`)
#' @param verbose Print compilation progress
#'
#' @return An S3 object of class `"newstan_fit"` wrapping a compiled Stan model.
#'
#' @export
stan_model <- function(file = NULL, code = NULL, model_name = NULL,
                       verbose = FALSE) {
  # Validate inputs
  if (is.null(file) && is.null(code)) {
    stop("Either 'file' or 'code' must be provided.")
  }
  if (!is.null(file) && !is.null(code)) {
    stop("Provide either 'file' or 'code', not both.")
  }

  # Read Stan code
  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("File not found: ", file)
    }
    code <- paste(readLines(file, warn = FALSE), collapse = "\n")
  }

  # Determine model name
  if (is.null(model_name)) {
    if (!is.null(file)) {
      model_name <- sub("\\.stan$", "", basename(file))
    } else {
      model_name <- "model"
    }
  }


  if (verbose) message("[newstan] Compiling '", model_name, "'...")

  # Step 1: Stan -> C++ via stanc.js (QuickJSR)
  cpp_code <- stanc_process(code)

  fun_base <- "
    #include <Rcpp.h>

    // [[Rcpp::depends(BH)]]
    // [[Rcpp::depends(RcppEigen)]]
    // [[Rcpp::depends(RcppParallel)]]

    // [[Rcpp::export]]
    Rcpp::XPtr<stan::model::model_base> new_model(Rcpp::XPtr<stan::io::var_context> data_context, unsigned int seed) {
      Rcpp::XPtr<stan::model::model_base> m(new stan_model(*data_context.get(), seed, &Rcpp::Rcout));
      return m;
    }
  "

  cppflags <- paste(
    paste0("-I", system.file("include", package = "newstan", mustWork = TRUE)),
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS -O3 -w"
  )

  env <- new.env()

  withr::with_makevars(
    c(
      USE_CXX17 = 1,
      PKG_CPPFLAGS = cppflags
    ),
    Rcpp::sourceCpp(code = paste0(cpp_code, fun_base, sep = "\n"), env = env, rebuild = TRUE, verbose = TRUE)
  )

  env
}
