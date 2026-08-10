# StanMCMC class

#' StanMCMC objects
#'
#' @name StanMCMC
#' @description A `StanMCMC` object is returned by [`$sample()`][model-method-sample]
#'   and contains MCMC posterior draws, sampler diagnostics, and fit metadata.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanMCMC` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$sampler_diagnostics()`][fit-method-mcmc] | Extract sampler diagnostics. |
#'  [`$num_chains()`][fit-method-mcmc] | Return the number of chains. |
#'  [`$diagnostic_summary()`][fit-method-mcmc] | Summarize sampler diagnostics. |
#'  [`$inv_metric()`][fit-method-mcmc] | Return the inverse mass matrix. |
#'
NULL

StanMCMC <- R6Class(
  "StanMCMC",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list()
    ) {
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_array"
      )
      private$warmup_draws_ <- .stanr_normalize_draw_names(
        payload$warmup_draws
      )
      private$warmup_diagnostics_ <- .stanr_normalize_draw_names(
        payload$warmup_diagnostics
      )
      if (!is.null(private$diagnostics_) && !isTRUE(metadata$fixed_param)) {
        invisible(self$diagnostic_summary(
          metadata$diagnostics %||% c("divergences", "treedepth", "ebfmi"),
          quiet = FALSE
        ))
      }
    },

    sampler_diagnostics = function(inc_warmup = FALSE, format = "draws_array") {
      inc_warmup <- .stanr_flag(inc_warmup, "inc_warmup")
      diagnostics <- private$diagnostics_
      if (is.null(diagnostics)) {
        stop("This fit does not contain sampler diagnostics.", call. = FALSE)
      }
      if (inc_warmup) {
        if (is.null(private$warmup_diagnostics_)) {
          stop(
            "warmup draws were not saved; only `$sample()` runs with ",
            "`save_warmup = TRUE` store them.",
            call. = FALSE
          )
        }
        diagnostics <- posterior::bind_draws(
          private$warmup_diagnostics_,
          diagnostics,
          along = "iteration"
        )
      }
      .stanr_as_draws_format(diagnostics, format)
    },

    num_chains = function() {
      private$metadata_$chains %||%
        posterior::nchains(private$draws_)
    },

    diagnostic_summary = function(
      diagnostics = c("divergences", "treedepth", "ebfmi"),
      quiet = FALSE
    ) {
      quiet <- .stanr_flag(quiet, "quiet")
      if (!length(diagnostics) || identical(diagnostics, "")) {
        return(list())
      }
      diagnostics <- match.arg(
        diagnostics,
        choices = c("divergences", "treedepth", "ebfmi"),
        several.ok = TRUE
      )

      draws <- self$sampler_diagnostics(format = "draws_array")
      n_chains <- self$num_chains()
      vars <- posterior::variables(draws)
      n_draws <- posterior::niterations(draws) * n_chains
      collected <- posterior::nchains(draws) == n_chains

      out <- list()

      if ("divergences" %in% diagnostics) {
        num_divergent <- if (collected && "divergent__" %in% vars) {
          vapply(
            seq_len(n_chains),
            function(i) sum(draws[, i, "divergent__"] > 0),
            integer(1)
          )
        } else {
          rep(NA_integer_, n_chains)
        }
        if (!quiet) {
          .stanr_check_divergences(num_divergent, n_draws)
        }
        out$num_divergent <- num_divergent
      }

      if ("treedepth" %in% diagnostics) {
        max_depth <- private$metadata_$arguments$max_depth %||% 10L
        num_max_treedepth <- if (collected && "treedepth__" %in% vars) {
          vapply(
            seq_len(n_chains),
            function(i) sum(draws[, i, "treedepth__"] >= max_depth),
            integer(1)
          )
        } else {
          rep(NA_integer_, n_chains)
        }
        if (!quiet) {
          .stanr_check_max_treedepth(num_max_treedepth, n_draws, max_depth)
        }
        out$num_max_treedepth <- num_max_treedepth
      }

      if ("ebfmi" %in% diagnostics) {
        out$ebfmi <- if (collected && "energy__" %in% vars) {
          .stanr_check_ebfmi(draws, n_chains, quiet)
        } else {
          rep(NA_real_, n_chains)
        }
      }

      out
    },

    inv_metric = function(matrix = TRUE) {
      matrix <- .stanr_flag(matrix, "matrix")
      metric <- private$inv_metric_
      if (is.null(metric)) {
        stop(
          "no adapted metric is available; the metric is only captured when ",
          "sampling runs with adaptation",
          call. = FALSE
        )
      }
      dense <- is.matrix(metric[[1]])
      if (dense) {
        if (!matrix) {
          stop(
            "`matrix = FALSE` is only available for diagonal metrics.",
            call. = FALSE
          )
        }
        return(metric)
      }
      if (matrix) {
        lapply(metric, function(v) diag(v, nrow = length(v)))
      } else {
        metric
      }
    },

    loo = function(
      variables = "log_lik",
      r_eff = FALSE,
      moment_match = FALSE,
      ...
    ) {
      if (!requireNamespace("loo", quietly = TRUE)) {
        stop("The `loo` package is required!", call. = FALSE)
      }
      moment_match <- .stanr_flag(moment_match, "moment_match")
      if (length(variables) != 1) {
        stop(
          "Only a single variable name is allowed for the 'variables' argument.",
          call. = FALSE
        )
      }
      LLarray <- self$draws(variables, format = "draws_array")
      if (is.logical(r_eff)) {
        if (isTRUE(r_eff)) {
          r_eff_cores <- getOption("mc.cores", 1)
          r_eff <- loo::relative_eff(
            exp(sweep(LLarray, 3, apply(LLarray, 3, max), FUN = "-")),
            cores = r_eff_cores
          )
        } else {
          r_eff <- NULL
        }
      }

      if (moment_match) {
        loo_result <- suppressWarnings(loo::loo.array(LLarray, r_eff = r_eff, ...))

        log_lik_i <- function(x, i, parameter_name = "log_lik", ...) {
          ll_array <- x$draws(variables = parameter_name, format = "draws_array")[,,
            i
          ]
          attr(ll_array, "dim") <- attributes(ll_array)$dim[1:2]
          ll_array
        }

        log_lik_i_upars <- function(x, upars, i, parameter_name = "log_lik", ...) {
          private$ensure_native()
          constrained <- private$model_$native_function("model_constrain_matrix")(
            private$model_ptr_,
            private$rng_ptr_,
            upars,
            TRUE,
            TRUE
          )
          colnames(constrained) <- .stanr_bracket_names(colnames(constrained))
          target <- paste0(parameter_name, "[", i, "]")
          if (!target %in% colnames(constrained)) {
            target <- parameter_name
          }
          constrained[, target]
        }

        loo::loo_moment_match.default(
          x = self,
          loo = loo_result,
          post_draws = function(x, ...) {
            x$draws(format = "draws_matrix")
          },
          log_lik_i = log_lik_i,
          unconstrain_pars = function(x, pars, ...) {
            x$unconstrain_draws(format = "draws_matrix")
          },
          log_prob_upars = function(x, upars, ...) {
            apply(upars, 1, x$log_prob)
          },
          log_lik_i_upars = log_lik_i_upars,
          ...
        )
      } else {
        loo::loo.array(LLarray, r_eff = r_eff, ...)
      }
    }
  ),
  cloneable = FALSE
)

# StanMCMC method documentation
#' MCMC-specific methods
#'
#' @name fit-method-mcmc
#' @family StanFit methods
#'
#' @description Methods specific to [`StanMCMC`] objects for accessing sampler
#'   diagnostics, chain information, and model evaluation.
#'
#'   ```
#'   sampler_diagnostics(inc_warmup = FALSE, format = "draws_array")
#'   num_chains()
#'   diagnostic_summary(diagnostics = c("divergences", "treedepth", "ebfmi"),
#'                       quiet = FALSE)
#'   inv_metric(matrix = TRUE)
#'   loo(variables = "log_lik", r_eff = FALSE, moment_match = FALSE, ...)
#'   ```
#'
#' @param matrix (logical) For `$inv_metric()`: return each chain's inverse
#'   metric as a matrix? For a diagonal metric, `TRUE` (the default) wraps the
#'   adapted diagonal in `diag()`; `FALSE` returns it as a vector. For a dense
#'   metric, the matrix is always returned and `matrix = FALSE` is an error.
#' @param diagnostics (character vector) For `$diagnostic_summary()`: which
#'   diagnostics to check. One or more of `"divergences"`, `"treedepth"`, and
#'   `"ebfmi"`. The default checks all three; `NULL` or `""` checks none.
#' @param quiet (logical) For `$diagnostic_summary()`: should messages about
#'   problems found in the requested diagnostics be suppressed? The default,
#'   `FALSE`, prints them in addition to returning the values. Diagnostics
#'   that could not be computed at all (rather than computed and found
#'   problematic) always warn, regardless of `quiet`.
#'
#' @return
#' * `$sampler_diagnostics()` returns sampler diagnostics (e.g., `divergent__`,
#'   `treedepth__`, `accept__`) as a posterior draws object.
#' * `$num_chains()` returns the number of MCMC chains.
#' * `$diagnostic_summary()` returns a list with one element per requested
#'   diagnostic: `num_divergent` (divergent transitions per chain),
#'   `num_max_treedepth` (iterations per chain that hit the max treedepth),
#'   and/or `ebfmi` (E-BFMI per chain). An element is `NA` for every chain if
#'   the corresponding diagnostic was not collected (e.g. `divergent__`/
#'   `treedepth__`/`energy__` are unavailable for the `static` and `walnuts`
#'   engines, or are all-`NA` for `fixed_param` runs).
#' * `$inv_metric()` returns a list (one element per chain) of the inverse
#'   mass matrix adapted during sampling. Errors if the fit was not sampled
#'   with adaptation.
#' * `$loo()` returns a LOO-CV object from the \pkg{loo} package.
#'
#' @seealso [`$draws()`][fit-method-draws], [`$loo()`][fit-method-loo]
#'
NULL

#' Leave-one-out cross-validation (LOO-CV)
#'
#' @name fit-method-loo
#' @aliases loo
#' @family StanFit methods
#'
#' @description The `$loo()` method computes approximate LOO-CV using the
#'   \pkg{loo} package. In order to use this method you must compute and save
#'   the pointwise log-likelihood in your Stan program. See [loo::loo.array()]
#'   and the \pkg{loo} package [vignettes](https://mc-stan.org/loo/articles/)
#'   for details.
#'
#' @param variables (string) The name of the variable in the Stan program
#'   containing the pointwise log-likelihood. The default is to look for
#'   `"log_lik"`. This argument is passed to the [`$draws()`][fit-method-draws]
#'   method.
#' @param r_eff (multiple options) How to handle the `r_eff` argument for
#'   `loo()`. `r_eff` measures the amount of autocorrelation in MCMC draws, and
#'   is used to compute more accurate ESS and MCSE estimates for pointwise and
#'   total ELPDs.
#'   * `TRUE` will call [loo::relative_eff()] to compute the `r_eff`
#'   argument to pass to [loo::loo.array()].
#'   * `FALSE` (the default) or `NULL` will avoid computing `r_eff`,
#'   which can be very slow. The reported ESS and MCSE estimates may be
#'   over-optimistic if the posterior draws are far from independent.
#'   * If `r_eff` is anything else, that object will be passed as the `r_eff`
#'   argument to [loo::loo.array()].
#' @param moment_match (logical) Whether to use a
#'   [moment-matching][loo::loo_moment_match()] correction for problematic
#'   observations. The default is `FALSE`. Using `moment_match = TRUE` uses
#'   the fit's model methods (see [fit-method-model-methods]) to automatically
#'   supply the functions for the `log_lik_i`, `unconstrain_pars`,
#'   `log_prob_upars`, and `log_lik_i_upars` arguments to
#'   [loo::loo_moment_match()].
#' @param ... Other arguments (e.g., `cores`, `save_psis`, etc.) passed to
#'   [loo::loo.array()] or [loo::loo_moment_match.default()]
#'   (if `moment_match = TRUE` is set).
#'
#' @return The object returned by [loo::loo.array()] or
#'   [loo::loo_moment_match.default()].
#'
#' @references
#' * Vehtari, A., Gelman, A., and Gabry, J. (2017). Practical Bayesian model
#'   evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing*, 27(5), 1413-1432.
#'   doi:10.1007/s11222-016-9696-4.
#' * Vehtari, A., Simpson, D., Gelman, A., Yao, Y., and Gabry, J. (2024).
#'   Pareto smoothed importance sampling.
#'   *Journal of Machine Learning Research*, 25(72), 1-58.
#' * Paananen, T., Piironen, J., Buerkner, P.-C., and Vehtari, A. (2021).
#'   Implicitly adaptive importance sampling.
#'   *Statistics and Computing*, 31, 16. doi:10.1007/s11222-020-09982-2
#'   (for `moment_match = TRUE`).
#'
#' @seealso The \pkg{loo} package website with
#'   [documentation](https://mc-stan.org/loo/reference/index.html) and
#'   [vignettes](https://mc-stan.org/loo/articles/).
#'
NULL
