# Suppresses R CMD check NOTES for R6 `self`/`private`.
private <- self <- NULL

# Session memo: compiled model envs, stanc context, stan version.
.stanr_memo <- new.env(parent = emptyenv())
