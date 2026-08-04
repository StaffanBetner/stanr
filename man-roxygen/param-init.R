#' @param init Initial values for parameters. Either `NULL` (equivalent to
#'   the default radius of 2), a non-negative number, used as the range of
#'   CmdStan-style random initialization (`Uniform(-init, init)` on the
#'   unconstrained scale; the default is 2), or a named list / named numeric
#'   vector of constrained parameter values.
#'   Supplied values are shared by all chains; parameters not covered by the
#'   supplied values are randomly initialized with the default range. A named
#'   list may include tuple-shaped list entries and R complex values for
#'   `tuple(...)`/`complex` parameters, using the same canonical shapes
#'   (and the same dotted-name escape hatch) documented for `data`.
