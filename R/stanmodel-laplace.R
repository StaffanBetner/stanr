# StanModel $laplace() method
#' Run Laplace approximation
#'
#' @name model-method-laplace
#' @aliases laplace
#' @family StanModel methods
#'
#' @description The `$laplace()` method of a [`StanModel`] object runs Stan's
#'   `"laplace"` method to draw from a Gaussian approximation to the posterior
#'   centered at the mode. Returns a [`StanLaplace`] object.
#'
#' @template param-data
#' @template param-seed
#' @template param-init
#' @param mode A numeric vector of parameter values at the mode, or a
#'   [`StanMLE`] object from [`$optimize()`][model-method-optimize]. If `NULL`,
#'   optimization is run first.
#' @param opt_args (list) Additional arguments to pass to
#'   [`$optimize()`][model-method-optimize] when finding the mode.
#' @param jacobian (logical) Should the log density be adjusted by the
#'   abs-determinant of the Jacobian of the inverse transformation?
#' @param draws (integer) The number of draws from the Laplace approximation.
#' @param calculate_lp (logical) Should the log density of the Laplace
#'   approximation be calculated?
#' @template param-num_threads
#' @template param-show_messages
#' @template param-show_exceptions
#' @template param-opencl_ids
#'
#' @return A [`StanLaplace`] object containing approximate posterior draws.
#'
#' @seealso [`$optimize()`][model-method-optimize],
#'   [`$variational()`][model-method-variational]
#'
NULL

stan_model_laplace_impl <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  mode = NULL,
  opt_args = NULL,
  jacobian = TRUE,
  draws = 1000L,
  show_messages = TRUE,
  show_exceptions = TRUE,
  calculate_lp = TRUE
) {
  if (!is.null(mode) && !is.null(opt_args)) {
    stop("`mode` and `opt_args` cannot both be supplied.", call. = FALSE)
  }
  reserved <- intersect(
    names(opt_args),
    c("data", "seed", "init", "jacobian", "show_messages", "show_exceptions")
  )
  if (length(reserved)) {
    stop(
      "`opt_args` cannot override: ",
      paste(reserved, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  jacobian <- .stanr_flag(jacobian, "jacobian")
  calculate_lp <- .stanr_flag(calculate_lp, "calculate_lp")
  common <- .stanr_common_service_flags(
    show_messages,
    show_exceptions,
    opencl_ids,
    private,
    num_threads,
    refresh
  )
  show_messages <- common$show_messages
  show_exceptions <- common$show_exceptions
  num_threads <- common$num_threads
  refresh <- common$refresh
  # Seed resolved once so the internal optimize() run and laplace share it.
  resolved_seed <- .stanr_seed(seed)

  mode_fit <- NULL
  if (is.null(mode)) {
    mode_fit <- do.call(
      self$optimize,
      c(
        list(
          data = data,
          seed = resolved_seed,
          init = init,
          jacobian = jacobian,
          show_messages = show_messages,
          show_exceptions = show_exceptions
        ),
        opt_args %||% list()
      )
    )
    mode_val <- mode_fit$mle()
  } else if (inherits(mode, "StanMLE")) {
    mode_fit <- mode
    mode_val <- mode_fit$mle()
  } else {
    # Numeric mode vector (stanr extension)
    mode_val <- mode
  }

  draws <- .stanr_int(draws, "draws")

  if (!is.numeric(mode_val) || is.null(names(mode_val))) {
    stop(
      "mode must be a named numeric vector or an optimization result.",
      call. = FALSE
    )
  }

  native_args_fn <- function(seed, resolved_init, model) {
    pars <- self$constrained_param_names(model)
    mode_val <- mode_val[.stanr_bracket_names(pars)]
    if (anyNA(mode_val)) {
      stop(
        "mode must contain every constrained model parameter.",
        call. = FALSE
      )
    }
    list(
      method = "laplace",
      mode = as.double(mode_val),
      jacobian = jacobian,
      num_draws = draws,
      calculate_lp = calculate_lp,
      seed = seed,
      refresh = refresh,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads
    )
  }

  payload_fn <- function(result) {
    list(draws = posterior::as_draws_matrix(result$draws))
  }

  # `init` isn't part of laplace's native args (centered at `mode`), so unused.
  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = resolved_seed,
    init = NULL,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanLaplace$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = resolved_seed,
    init = init,
    elapsed = res$elapsed,
    mode = mode_fit %||% mode,
    metadata = list(
      method = "laplace",
      jacobian = jacobian,
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
