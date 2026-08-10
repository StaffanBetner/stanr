# Shared by StanLaplace/StanVB/StanPathfinder, all of which report an
# approximate (rather than exact) log density.
.stanr_lp_approx_impl <- function() {
  as.numeric(self$draws(variables = "lp_approx__", format = "draws_matrix"))
}

# StanLaplace class

#' StanLaplace objects
#'
#' @name StanLaplace
#' @description A `StanLaplace` object is returned by
#'   [`$laplace()`][model-method-laplace] and contains draws from a Gaussian
#'   approximation to the posterior centered at the mode.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanLaplace` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$mode()`][fit-method-laplace-mode] | Return the mode used for the approximation. |
#'  [`$lp_approx()`][fit-method-lp] | Extract the log density approximation draws. |
#'
NULL

StanLaplace <- R6Class(
  "StanLaplace",
  inherit = StanFit,
  public = list(
    initialize = function(
      payload = list(),
      model = NULL,
      data = list(),
      seed = 1L,
      init = NULL,
      elapsed = NA_real_,
      metadata = list(),
      mode = NULL
    ) {
      private$mode_ <- mode
      payload$draws <- .stanr_rename_draw_columns(payload$draws)
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    },

    mode = function() private$mode_,

    lp_approx = .stanr_lp_approx_impl
  ),
  private = list(mode_ = NULL),
  cloneable = FALSE
)

# StanLaplace method documentation
#' Extract Laplace mode
#'
#' @name fit-method-laplace-mode
#' @aliases mode
#' @family StanFit methods
#'
#' @description Return the mode (point estimate) used as the center of the
#'   Laplace approximation. This is either a [`StanMLE`] object from a prior
#'   call to [`$optimize()`][model-method-optimize], or a numeric vector
#'   provided directly.
#'
#' @return The mode object or numeric vector.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL

# StanVB class

#' StanVB objects
#'
#' @name StanVB
#' @description A `StanVB` object is returned by
#'   [`$variational()`][model-method-variational] and contains approximate
#'   posterior draws from Automatic Differentiation Variational Inference (ADVI).
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanVB` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$lp_approx()`][fit-method-lp] | Extract the log density approximation draws. |
#'
NULL

StanVB <- R6Class(
  "StanVB",
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
      draws <- payload$draws
      draws <- .stanr_rename_draw_columns(draws)
      requested <- payload$args$output_samples
      if (
        !is.null(draws) && !is.null(requested) && nrow(draws) == requested + 1L
      ) {
        draws <- draws[-1L, , drop = FALSE]
      }
      if (
        !is.null(draws) &&
          "lp__" %in% colnames(draws) &&
          all(is.na(draws[, "lp__"]) | draws[, "lp__"] == 0)
      ) {
        draws <- draws[, setdiff(colnames(draws), "lp__"), drop = FALSE]
      }
      payload$draws <- draws
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    },

    lp_approx = .stanr_lp_approx_impl
  ),
  cloneable = FALSE
)

# StanPathfinder class

#' StanPathfinder objects
#'
#' @name StanPathfinder
#' @description A `StanPathfinder` object is returned by
#'   [`$pathfinder()`][model-method-pathfinder] and contains approximate
#'   posterior draws from the Pathfinder algorithm.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanPathfinder` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$lp_approx()`][fit-method-lp] | Extract the log density approximation draws. |
#'
NULL

StanPathfinder <- R6Class(
  "StanPathfinder",
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
      payload$draws <- .stanr_rename_draw_columns(payload$draws)
      super$initialize(
        payload,
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    },

    lp_approx = .stanr_lp_approx_impl
  ),
  cloneable = FALSE
)
