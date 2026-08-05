# StanModel $variational() method ----------------------------------------------

#' Run variational inference (ADVI)
#'
#' @name model-method-variational
#' @aliases variational
#' @family StanModel methods
#'
#' @description The `$variational()` method of a [`StanModel`] object runs
#'   Stan's `"variational"` method, which uses Automatic Differentiation
#'   Variational Inference (ADVI) to approximate the posterior distribution.
#'   Returns a [`StanVB`] object.
#'
#' @template param-data
#' @template param-init
#' @param algorithm (string) The variational inference algorithm: `"meanfield"`
#'   or `"fullrank"`.
#' @param iter (integer) The number of ADVI iterations.
#' @param grad_samples (integer) Number of samples to use for gradient estimation.
#' @param elbo_samples (integer) Number of samples for ELBO evaluation.
#' @param eta (number) Learning rate for ADVI.
#' @param adapt_engaged (logical) Should the learning rate be adapted?
#' @param adapt_iter (integer) Number of iterations for learning rate adaptation.
#' @param tol_rel_obj (number) Relative tolerance for ELBO convergence.
#' @param eval_elbo (integer) How often to evaluate the ELBO.
#' @param draws (integer) The number of draws from the variational approximation.
#' @param num_threads (integer) The total number of threads to use across all
#'   chains. Defaults to `RcppParallel::defaultNumThreads()` (all available threads).
#' @template param-opencl_ids
#'
#' @return A [`StanVB`] object containing approximate posterior draws.
#'
#' @seealso [`$sample()`][model-method-sample],
#'   [`$pathfinder()`][model-method-pathfinder]
#'
NULL

stan_model_variational_impl <- function(
  data = list(),
  seed = NULL,
  refresh = 100L,
  init = 2,
  save_latent_dynamics = FALSE,
  num_threads = RcppParallel::defaultNumThreads(),
  opencl_ids = NULL,
  algorithm = "meanfield",
  iter = 10000L,
  grad_samples = 1L,
  elbo_samples = 100L,
  eta = 1,
  adapt_engaged = TRUE,
  adapt_iter = 50L,
  tol_rel_obj = 0.01,
  eval_elbo = 100L,
  draws = 1000L,
  show_messages = TRUE,
  show_exceptions = TRUE
) {
  save_latent_dynamics <- .stanr_flag(
    save_latent_dynamics,
    "save_latent_dynamics"
  )
  adapt_engaged <- .stanr_flag(adapt_engaged, "adapt_engaged")
  show_messages <- .stanr_flag(show_messages, "show_messages")
  show_exceptions <- .stanr_flag(show_exceptions, "show_exceptions")
  if (!is.null(opencl_ids)) {
    private$select_opencl(opencl_ids)
  }
  if (save_latent_dynamics) {
    stop("`save_latent_dynamics` is not yet supported.", call. = FALSE)
  }
  if (!algorithm %in% c("meanfield", "fullrank")) {
    stop(
      "`algorithm` must be one of \"meanfield\", \"fullrank\".",
      call. = FALSE
    )
  }

  num_threads <- .stanr_int(num_threads %||% 1L, "num_threads", min = 1L)
  refresh <- .stanr_int(refresh, "refresh")
  iter <- .stanr_int(iter, "iter")
  grad_samples <- .stanr_int(grad_samples, "grad_samples")
  elbo_samples <- .stanr_int(elbo_samples, "elbo_samples")
  adapt_iter <- .stanr_int(adapt_iter, "adapt_iter")
  eval_elbo <- .stanr_int(eval_elbo, "eval_elbo")
  draws <- .stanr_int(draws, "draws")

  native_args_fn <- function(seed, resolved_init, model) {
    list(
      method = "variational",
      algorithm = algorithm,
      seed = seed,
      id = 1L,
      init_radius = resolved_init$radius,
      iter = iter,
      grad_samples = grad_samples,
      elbo_samples = elbo_samples,
      tol_rel_obj = as.double(tol_rel_obj),
      eta = as.double(eta),
      adapt_engaged = adapt_engaged,
      adapt_iter = adapt_iter,
      eval_elbo = eval_elbo,
      output_samples = draws,
      verbose = show_messages,
      show_exceptions = show_exceptions,
      num_threads = num_threads,
      init = resolved_init$values
    )
  }

  payload_fn <- function(result) {
    list(
      draws = if (result$return_code == 0L) {
        posterior::as_draws_matrix(result$draws)
      }
    )
  }

  res <- .stanr_run_service(
    self = self,
    data = data,
    seed = seed,
    init = init,
    native_args_fn = native_args_fn,
    payload_fn = payload_fn
  )

  StanVB$new(
    payload = res$payload,
    model = self,
    data = data,
    seed = res$seed,
    init = init,
    elapsed = res$elapsed,
    metadata = list(
      method = "variational",
      num_threads = num_threads,
      show_exceptions = show_exceptions
    )
  )
}
