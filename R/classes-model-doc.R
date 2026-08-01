#' Input and output variables of a Stan program
#'
#' @name model-method-variables
#' @aliases variables

#' @description The `$variables()` method of a `StanModel` object (created by
#'   [`stan_model()`]) returns
#'   a list, each element representing a Stan model block: `data`, `parameters`,
#'   `transformed_parameters` and `generated_quantities`.
#'
#'   Each element contains a list of variables, with each variable represented
#'   as a list with information on its scalar type (`real` or `int`) and
#'   number of dimensions.
#'
#'   The number of dimensions reported is the number of indexing dimensions in
#'   the declared Stan variable, equivalently the number of indices needed to
#'   access one scalar element. This means a scalar has 0 dimensions, a vector
#'   or one-dimensional array has 1, and a matrix or two-dimensional array has
#'   2. Array dimensions are added to any vector or matrix dimensions, so
#'   `array[J] matrix[N, K]` has 3 dimensions. See **Examples**.
#'
#'   `transformed data` is not included, as variables in that block are not
#'   part of the model's input or output.
#'
#' @return The method returns a list with information on input and output
#'   variables for each of the Stan model blocks.
#'
#' @examples
#' mod <- stan_model(
#'   code = "
#'   data {
#'     int N;
#'     array[2, 3] int y;
#'   }
#'   parameters {
#'     real alpha;
#'     vector[N] beta;
#'     array[2] matrix[3, 4] theta;
#'   }
#'   ",
#'   compile = FALSE
#' )
#'
#' vars <- mod$variables()
#' str(vars)
NULL
