#' The stanr package.
#'
#' @description A modern, simpler R interface for Stan. Bundles all required
#'   Stan headers internally, depending only on BH (Boost), RcppEigen
#'   (Eigen), and RcppParallel (TBB) for external dependencies.
#'
#' @docType package
#' @name stanr-package
#' @aliases stanr
#' @useDynLib stanr, .registration = TRUE
#'
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom R6 R6Class
#' @importFrom Rcpp sourceCpp
#' @importFrom posterior as_draws_df subset_draws
#' @importFrom withr with_makevars
#' @importFrom jsonlite fromJSON
#'
"_PACKAGE"
