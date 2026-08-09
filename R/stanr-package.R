#' The stanr package.
#'
#' @description A modern, simpler R interface for Stan. Bundles all required
#'   Stan and Eigen headers internally, and RcppParallel (TBB) for external
#'   dependencies.
#'
#' @docType package
#' @name stanr-package
#' @aliases stanr
#' @useDynLib stanr, .registration = TRUE
#'
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom R6 R6Class
#' @importFrom posterior as_draws_df subset_draws
#' @importFrom withr with_makevars
#' @importFrom jsonlite fromJSON
#'
"_PACKAGE"
