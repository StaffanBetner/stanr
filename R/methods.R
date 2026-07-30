#' @noRd
#' @method summary StanSample
#' @export
summary.StanSample <- function(object, ...) {
  posterior::summarise_draws(object$draws, ...)
}

#' @noRd
#' @method summary StanPathfinder
#' @export
summary.StanPathfinder <- function(object, ...) {
  posterior::summarise_draws(object$draws, ...)
}

#' @noRd
#' @method summary StanVariational
#' @export
summary.StanVariational <- function(object, ...) {
  posterior::summarise_draws(object$draws, ...)
}

#' @noRd
#' @method summary StanLaplace
#' @export
summary.StanLaplace <- function(object, ...) {
  posterior::summarise_draws(object$draws, ...)
}

#' @noRd
#' @method summary StanGeneratedQuantities
#' @export
summary.StanGeneratedQuantities <- function(object, ...) {
  posterior::summarise_draws(object$draws, ...)
}

#' @noRd
#' @method summary StanOptimize
#' @export
summary.StanOptimize <- function(object, ...) {
  data.frame(
    variable = c(names(object$par), "lp__"),
    estimate = c(unname(object$par), object$value),
    row.names = NULL
  )
}

#' @noRd
#' @method summary StanDiagnose
#' @export
summary.StanDiagnose <- function(object, ...) {
  data.frame(
    diagnostic = "num_failed",
    value = unclass(object),
    row.names = NULL
  )
}
