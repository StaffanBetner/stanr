.onLoad <- function(libname, pkgname) {
  assign("stanc_context", QuickJSR::JSContext$new(), envir = topenv())
  stanc_context$source(system.file(
    "stanc.js",
    package = "newstan",
    mustWork = TRUE
  ))
}
