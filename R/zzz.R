# Suppress R CMD check NOTES for R6 internal variables used in standalone
# method definitions attached via $set().
private <- self <- NULL

# Package-local memo environment. Holds session-lifetime caches for values
# that depend only on session-stable inputs (installed package versions,
# toolchain configuration, etc.) -- never on anything that can change within
# a session, such as file contents covered by the PCH fingerprint.
.newstan_memo <- new.env(parent = emptyenv())
