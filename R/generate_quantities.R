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

  dat_ptr <- .Call(`r_data_context`, data)
  mod_ptr <- stanmod$new_model(dat_ptr, seed)
  pars <- .Call(`constrained_par_names`, mod_ptr)

  # Convert draws to matrix (rows=samples, columns=parameters)
  draws_matrix <- if (inherits(draws, "draws")) {
    posterior::as_draws_matrix(posterior::subset_draws(draws, variable = pars))
  } else {
    as.matrix(draws)
  }

  args <- list(
    method = "standalone_gqs",
    seed = as.integer(seed),
    chain_id = as.integer(chain_id),
    verbose = as.logical(verbose),
    draws = draws_matrix
  )

  result <- .Call(`newstan_run`, mod_ptr, args)
  gqs_draws <- posterior::as_draws_df(as.data.frame(result$samples))

  list(
    draws = gqs_draws,
    return_code = result$return_code,
    args = args
  )
}
