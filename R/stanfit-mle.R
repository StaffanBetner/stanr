# StanMLE class

#' StanMLE objects
#'
#' @name StanMLE
#' @description A `StanMLE` object is returned by
#'   [`$optimize()`][model-method-optimize] and contains a point estimate
#'   (MAP or MLE) and optimization metadata.
#'
#' @section Methods: In addition to the methods inherited from [`StanFit`],
#'   `StanMLE` objects have:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$mle()`][fit-method-mle] | Extract the point estimate as a numeric vector. |
#'
NULL

StanMLE <- R6Class(
  "StanMLE",
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
      par <- payload$par %||% numeric()
      if (!is.null(names(par))) {
        par <- par[!(names(par) %in% c("lp__", "converged__"))]
        names(par) <- .stanr_bracket_names(names(par))
      }
      payload$par <- par
      if (!is.null(payload$iterations)) {
        iterations <- payload$iterations
        iterations <- iterations[,
          colnames(iterations) != "converged__",
          drop = FALSE
        ]
        colnames(iterations) <- .stanr_bracket_names(colnames(iterations))
        payload$draws <- iterations
      } else if (length(par)) {
        payload$draws <- matrix(
          c(payload$value %||% NA_real_, unname(par)),
          nrow = 1L,
          dimnames = list(NULL, c("lp__", names(par)))
        )
      }
      if (!is.null(payload$draws)) {
        payload$draws <- posterior::as_draws_matrix(payload$draws)
      }
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

    mle = function(variables = NULL) {
      value <- private$par_ %||% numeric()
      if (!is.null(variables)) {
        missing <- setdiff(variables, names(value))
        if (length(missing)) {
          stop(
            "Unknown variable(s): ",
            paste(missing, collapse = ", "),
            ".",
            call. = FALSE
          )
        }
        value <- value[variables]
      }
      value
    },

    summary = function(variables = NULL, ...) {
      value <- self$mle(variables)
      data.frame(
        variable = names(value),
        estimate = unname(value),
        row.names = NULL
      )
    },

    print = function(variables = NULL, ..., digits = 2, max_rows = 20) {
      out <- self$summary(variables = variables, ...)
      print(utils::head(out, max_rows), digits = digits)
      invisible(self)
    }
  ),
  cloneable = FALSE
)

# StanMLE method documentation
#' Extract point estimate
#'
#' @name fit-method-mle
#' @aliases mle
#' @family StanFit methods
#'
#' @description Extract the maximum likelihood or maximum a posteriori estimate
#'   from a [`StanMLE`] object as a named numeric vector.
#'
#' @template param-variables
#'
#' @return A named numeric vector of parameter estimates. Always reflects the
#'   final optimization iteration, regardless of whether
#'   [`$optimize()`][model-method-optimize] was run with `save_iterations =
#'   TRUE`.
#'
#' @seealso [`$draws()`][fit-method-draws], which under
#'   `save_iterations = TRUE` returns the full optimization path (one row per
#'   saved iteration) instead of a single row for the final estimate.
#'
NULL
