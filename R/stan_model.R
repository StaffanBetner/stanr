#' Compile and load a Stan model
#'
#' @param file Path to a `.stan` file (or `NULL` if `code` is provided)
#' @param code Stan model code as a string (alternative to `file`)
#' @param model_name Override model name (default: basename of `file` without `.stan`)
#' @param force_recompile Whether to always recompile, even if a cached model is found
#' @param verbose Print compilation progress
#'
#' @return An S3 object of class `"newstan_fit"` wrapping a compiled Stan model.
#'
#' @export
stan_model <- function(
  file = NULL,
  code = NULL,
  model_name = NULL,
  verbose = FALSE,
  force_recompile = FALSE
) {
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

  if (verbose) {
    message("[newstan] Compiling '", model_name, "'...")
  }

  # Step 1: Stan -> C++ via stanc.js (QuickJSR)
  cpp_code <- stanc_process(code)

  fun_base <- "
    #include <Rcpp.h>
    #include <stan/math/rev/core.hpp>

    namespace newstan {
    struct model_bridge {
      using log_prob_fn = double (*)(const void*, const double*, double*, bool,
                                     bool, std::ostream*);
      const void* context;
      log_prob_fn log_prob;
    };

    inline double model_log_prob(const void* context, const double* theta,
                                 double* gradient, bool propto, bool jacobian,
                                 std::ostream* msgs) {
      const auto* model = static_cast<const stan_model*>(context);
      stan::math::nested_rev_autodiff nested;
      Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> ad_theta(
          model->num_params_r());
      for (size_t i = 0; i < model->num_params_r(); ++i) {
        ad_theta(i) = theta[i];
      }

      stan::math::var ad_log_prob;
      if (propto) {
        ad_log_prob = jacobian
            ? model->template log_prob<true, true>(ad_theta, msgs)
            : model->template log_prob<true, false>(ad_theta, msgs);
      } else {
        ad_log_prob = jacobian
            ? model->template log_prob<false, true>(ad_theta, msgs)
            : model->template log_prob<false, false>(ad_theta, msgs);
      }

      const double log_prob = ad_log_prob.val();
      if (gradient != nullptr) {
        ad_log_prob.grad();
        for (size_t i = 0; i < model->num_params_r(); ++i) {
          gradient[i] = ad_theta(i).adj();
        }
      }
      return log_prob;
    }
    }  // namespace newstan

    // [[Rcpp::depends(BH)]]
    // [[Rcpp::depends(RcppEigen)]]
    // [[Rcpp::depends(RcppParallel)]]

    // [[Rcpp::export]]
    Rcpp::XPtr<stan::model::model_base> new_model(Rcpp::XPtr<stan::io::var_context> data_context, unsigned int seed) {
      Rcpp::XPtr<stan::model::model_base> m(new stan_model(*data_context.get(), seed, &Rcpp::Rcout));
      return m;
    }

    // [[Rcpp::export]]
    Rcpp::XPtr<newstan::model_bridge> new_model_bridge(
        Rcpp::XPtr<stan::model::model_base> model) {
      auto* bridge = new newstan::model_bridge{
          static_cast<const stan_model*>(model.get()), newstan::model_log_prob};
      return Rcpp::XPtr<newstan::model_bridge>(bridge);
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
    Rcpp::sourceCpp(
      code = paste0(cpp_code, fun_base, sep = "\n"),
      env = env,
      rebuild = force_recompile,
      verbose = FALSE
    )
  )

  env
}
