# StanGQ class -----------------------------------------------------------------

#' StanGQ objects
#'
#' @name StanGQ
#' @description A `StanGQ` object is returned by
#'   [`$generate_quantities()`][model-method-generate-quantities] and contains
#'   the generated quantities computed from posterior draws.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanGQ` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$num_chains()`][fit-method-gq] | Return the number of chains in the draws. |
#'
NULL

StanGQ <- R6Class(
  "StanGQ",
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
    },

    num_chains = function() {
      if (is.null(private$draws_)) {
        return(0L)
      }
      posterior::nchains(private$draws_)
    }
  ),
  cloneable = FALSE
)

# StanGQ method documentation --------------------------------------------------

#' Generated quantities chain count
#'
#' @name fit-method-gq
#' @family StanFit methods
#'
#' @description Return the number of chains in a [`StanGQ`] object's draws.
#'
#' @return An integer giving the number of chains, or `0` if no draws are
#'   available.
#'
#' @seealso [`$draws()`][fit-method-draws]
#'
NULL
