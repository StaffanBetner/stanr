# Suppress R CMD check NOTES for R6 internal variables used in standalone
# method definitions attached via $set().
private <- self <- NULL

.onLoad <- function(libname, pkgname) {
  assign("stanc_context", QuickJSR::JSContext$new(), envir = topenv())
  stanc_context$source(system.file(
    "stanc.js",
    package = "newstan",
    mustWork = TRUE
  ))
}
