#' Generate quantities from posterior draws
#'
#' Takes pre-computed parameter draws and generates quantities of interest.
#'
#' @param stanmod A model environment returned by [stan_model()], compiled from
#'   a Stan model with a generated quantities block.
#' @param data Named list of data variables to pass to the model.
#' @param draws An object containing posterior draws of constrained parameters
#'   (e.g., the `draws` element from a [sampling()] result).
#' @param seed Random seed (NA = random).
#' @param chain_id Chain ID for RNG advancement (default: 1).
#' @param verbose Print progress (default: FALSE).
#' @param ... Unused.
#'
#' @return A list containing:
#'   - `draws`: a `posterior::as_draws_df` object with generated quantity draws.
#'   - `return_code`: integer status code.
#'   - `args`: named list of generated quantities arguments.
#'
#' @export
generated_quantities <- function(
  stanmod,
  data,
  draws,
  seed = NA,
  chain_id = 1,
  verbose = FALSE,
  ...
) {
  if (is.na(seed)) {
    seed <- as.integer(runif(1, 1, 2^31 - 1))
  }

  # Convert draws to matrix (rows=samples, columns=parameters)
  draws_matrix <- if (inherits(draws, "draws")) {
    posterior::as_draws_matrix(draws)
  } else {
    as.matrix(draws)
  }

  dat_ptr <- .Call(`r_data_context`, data)
  mod_ptr <- stanmod$new_model(dat_ptr, seed)

  # Get constrained parameter names and exclude lp__
  constrained_names <- .Call(`r_param_names`, mod_ptr)
  constrained_names <- constrained_names[constrained_names != "lp__"]

  # Extract constrained parameter values and unconstrain them
  par_cols <- intersect(colnames(draws_matrix), constrained_names)
  draws_constrained <- draws_matrix[, par_cols, drop = FALSE]
  n_draws <- nrow(draws_constrained)
  n_unconstrained <- length(.Call(
    `r_unconstrain`,
    mod_ptr,
    draws_constrained[1, , drop = FALSE]
  ))
  draws_unconstrained <- matrix(
    .Call(`r_unconstrain`, mod_ptr, as.numeric(t(draws_constrained))),
    nrow = n_draws,
    ncol = n_unconstrained
  )

  args <- list(
    method = "standalone_gqs",
    seed = as.integer(seed),
    chain_id = as.integer(chain_id),
    verbose = as.logical(verbose),
    draws = draws_unconstrained
  )

  result <- .Call(`newstan_run`, mod_ptr, args)
  gqs_draws <- posterior::as_draws_df(result$samples)

  list(
    draws = gqs_draws,
    return_code = result$return_code,
    args = args
  )
}
