# StanDiagnose class -----------------------------------------------------------

#' StanDiagnose objects
#'
#' @name StanDiagnose
#' @description A `StanDiagnose` object is returned by
#'   [`$diagnose()`][model-method-diagnose] and contains the results of Stan's
#'   gradient checking diagnostics.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanDiagnose` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$gradients()`][fit-method-diagnose] | Return the gradient check results. |
#'  [`$lp()`][fit-method-diagnose] | Return the log probability evaluated at the
#'   initial parameter values. |
#'  [`$num_failed()`][fit-method-diagnose] | Return the number of failed gradient checks. |
#'
NULL

StanDiagnose <- R6Class(
  "StanDiagnose",
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
      private$gradients_ <- payload$gradients %||% data.frame()
      private$lp_ <- payload$lp %||% NA_real_
      private$num_failed_ <- as.integer(payload$num_failed %||% NA_integer_)
      metadata$num_failed <- private$num_failed_
      super$initialize(
        list(
          return_code = payload$return_code %||% 0L,
          output = payload$output %||% character(),
          model_ptr = payload$model_ptr
        ),
        model,
        data,
        seed,
        init,
        elapsed,
        metadata,
        default_format = "draws_matrix"
      )
    },

    gradients = function() private$gradients_,

    lp = function() as.numeric(private$lp_),

    num_failed = function() private$num_failed_
  ),
  private = list(
    gradients_ = NULL,
    lp_ = NULL,
    num_failed_ = NULL
  ),
  cloneable = FALSE
)

# StanDiagnose method documentation --------------------------------------------

#' Gradient diagnostic results
#'
#' @name fit-method-diagnose
#' @family StanFit methods
#'
#' @description Access gradient checking results from a [`StanDiagnose`] object.
#'
#'   ```
#'   gradients()
#'   lp()
#'   num_failed()
#'   ```
#'
#' @return
#' * `$gradients()` returns a data frame with gradient check results for each
#'   parameter, including columns `param_idx`, `value`, `model`, `finite_diff`,
#'   and `error`.
#' * `$lp()` returns the log probability evaluated at the initial parameter
#'   values.
#' * `$num_failed()` returns the number of parameters that failed the gradient
#'   check.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL
