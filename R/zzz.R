# Suppresses R CMD check NOTES for R6 `self`/`private`, used in standalone
# method definitions assigned by reference into R6Class().
private <- self <- NULL

# Session-lifetime memo cache, keyed only on session-stable inputs (package
# versions, toolchain identity, etc.) -- never on anything that can change
# mid-session.
.stanr_memo <- new.env(parent = emptyenv())
