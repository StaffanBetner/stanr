
<!-- README.md is generated from README.Rmd. Please edit that file -->

# newstan

<!-- badges: start -->

<!-- badges: end -->

`newstan` is an R interface for compiling Stan programs and running Stan
services directly from R. Results use `posterior` draw objects where
possible, and every service result includes the arguments passed through
to the backend.

## Service Return Types

| Function | Purpose | Return type |
|----|----|----|
| `sampling()` | MCMC sampling, including HMC/NUTS and fixed-parameter runs | list with `draws`, `diagnostics`, `return_code`, `args` |
| `pathfinder()` | Pathfinder approximate posterior draws | list with `draws`, `diagnostics`, `return_code`, `args` |
| `variational()` | ADVI approximate posterior draws | list with `draws`, `return_code`, `args` |
| `optimizing()` | Posterior mode or maximum likelihood estimate | list with `par`, `value`, `return_code`, `args` |
| `laplace()` | Laplace approximation draws around a mode | list with `draws`, `return_code`, `args` |
| `generated_quantities()` | Generated quantities from existing parameter draws | list with `draws`, `return_code`, `args` |
| `gradient_check()` | Finite-difference gradient diagnostic | integer count of failed checks |

## Installation

You can install the development version of newstan from GitHub:

``` r
# install.packages("pak")
pak::pak("andrjohns/newstan")
```

## Compile A Model

Models can be compiled from a string or from a `.stan` file. The
returned object is a model environment used by the service functions
below.

``` r
library(newstan)

bernoulli_model <- "
data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  theta ~ beta(1, 1);
  y ~ bernoulli(theta);
}
generated quantities {
  array[N] real log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_lpmf(y[n] | theta);
  }
}
"

mod <- stan_model(code = bernoulli_model, model_name = "bernoulli")
data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

fixed_param_model <- "
data {
  int<lower=0> N;
}
generated quantities {
  array[N] int<lower=0, upper=1> y_rep;
  for (n in 1:N) {
    y_rep[n] = bernoulli_rng(0.5);
  }
}
"

fixed_mod <- stan_model(code = fixed_param_model, model_name = "fixed_param")
fixed_data <- list(N = 5)
```

`stan_model()` returns an S3 object of class `"newstan_fit"` wrapping
the compiled model.

## Sampling

Use `sampling()` for MCMC. The result is a list with posterior draws,
sampler diagnostics, a Stan return code, and the normalized argument
list.

``` r
fit <- sampling(
  mod,
  data,
  num_warmup = 20,
  num_samples = 20,
  num_chains = 2,
  seed = 123,
  verbose = FALSE
)

list(
  draws_class = class(fit$draws)[1],
  diagnostics_class = class(fit$diagnostics)[1],
  variables = posterior::variables(fit$draws),
  return_code = fit$return_code
)
#> $draws_class
#> [1] "draws_df"
#> 
#> $diagnostics_class
#> [1] "draws_df"
#> 
#> $variables
#>  [1] "lp__"       "theta"      "log_lik.1"  "log_lik.2"  "log_lik.3" 
#>  [6] "log_lik.4"  "log_lik.5"  "log_lik.6"  "log_lik.7"  "log_lik.8" 
#> [11] "log_lik.9"  "log_lik.10"
#> 
#> $return_code
#> [1] 0

head(summary(fit), 3)
#> # A tibble: 3 × 10
#>   variable    mean median    sd   mad     q5    q95  rhat ess_bulk ess_tail
#>   <chr>      <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 lp__      -8.73  -8.47  0.612 0.210 -9.99  -8.32   1.24     10.6     22.4
#> 2 theta      0.500  0.485 0.126 0.101  0.316  0.700  1.20     21.1     21.4
#> 3 log_lik.1 -0.727 -0.723 0.282 0.218 -1.15  -0.357  1.16     21.1     21.4
```

The return value has:

- `draws`: a `posterior::draws_df` containing model parameter draws.
- `diagnostics`: a `posterior::draws_df` containing sampler diagnostics
  such as `accept_stat__`, `stepsize__`, `treedepth__`, `n_leapfrog__`,
  `divergent__`, and `energy__`.
- `return_code`: integer status code.
- `args`: named list of sampling arguments.

### Fixed Param

For models without parameters, or when you only want generated
quantities, use the fixed-parameter sampler.

``` r
fixed_fit <- sampling(
  fixed_mod,
  fixed_data,
  algorithm = "fixed_param",
  num_warmup = 0,
  num_samples = 10,
  seed = 123,
  verbose = FALSE
)

list(
  variables = posterior::variables(fixed_fit$draws),
  ndraws = posterior::ndraws(fixed_fit$draws),
  return_code = fixed_fit$return_code
)
#> $variables
#> [1] "lp__"    "y_rep.1" "y_rep.2" "y_rep.3" "y_rep.4" "y_rep.5"
#> 
#> $ndraws
#> [1] 10
#> 
#> $return_code
#> [1] 0

head(summary(fixed_fit), 3)
#> # A tibble: 3 × 10
#>   variable  mean median    sd   mad    q5   q95  rhat ess_bulk ess_tail
#>   <chr>    <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 lp__       0      0   0     0         0     0 NA          NA       NA
#> 2 y_rep.1    0.5    0.5 0.527 0.741     0     1 NA           5       NA
#> 3 y_rep.2    0.6    1   0.516 0         0     1  1.06        5       NA
```

### Supplying Inverse Metrics

`sampling()` accepts precomputed inverse metric values through
`inv_metric`. This corresponds to Stan’s `metric_file` input.

For a diagonal metric, pass a numeric vector with one value per
unconstrained parameter. The Bernoulli model above has one unconstrained
parameter.

``` r
diag_metric_fit <- sampling(
  mod,
  data,
  metric = "diag_e",
  inv_metric = c(1.0),
  num_warmup = 20,
  num_samples = 20,
  seed = 123,
  verbose = FALSE
)

list(
  variables = posterior::variables(diag_metric_fit$draws),
  return_code = diag_metric_fit$return_code
)
#> $variables
#>  [1] "lp__"       "theta"      "log_lik.1"  "log_lik.2"  "log_lik.3" 
#>  [6] "log_lik.4"  "log_lik.5"  "log_lik.6"  "log_lik.7"  "log_lik.8" 
#> [11] "log_lik.9"  "log_lik.10"
#> 
#> $return_code
#> [1] 0

head(summary(diag_metric_fit), 3)
#> # A tibble: 3 × 10
#>   variable    mean median     sd    mad     q5    q95  rhat ess_bulk ess_tail
#>   <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 lp__      -8.52  -8.42  0.235  0.110  -8.99  -8.32  0.988    15.1      20.4
#> 2 theta      0.464  0.453 0.0856 0.0523  0.337  0.605 0.985     9.66     20.4
#> 3 log_lik.1 -0.783 -0.792 0.186  0.115  -1.09  -0.502 0.985     9.66     20.4
```

For a dense metric, pass a square matrix. To provide one metric per
chain, pass a list whose length equals `num_chains`; a single vector or
matrix is reused for all chains.

``` r
dense_metric_fit <- sampling(
  mod,
  data,
  metric = "dense_e",
  inv_metric = list(matrix(1.0, nrow = 1, ncol = 1)),
  num_chains = 1,
  num_warmup = 20,
  num_samples = 20,
  seed = 123,
  verbose = FALSE
)

list(
  variables = posterior::variables(dense_metric_fit$draws),
  return_code = dense_metric_fit$return_code
)
#> $variables
#>  [1] "lp__"       "theta"      "log_lik.1"  "log_lik.2"  "log_lik.3" 
#>  [6] "log_lik.4"  "log_lik.5"  "log_lik.6"  "log_lik.7"  "log_lik.8" 
#> [11] "log_lik.9"  "log_lik.10"
#> 
#> $return_code
#> [1] 0

head(summary(dense_metric_fit), 3)
#> # A tibble: 3 × 10
#>   variable    mean median     sd    mad     q5    q95  rhat ess_bulk ess_tail
#>   <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 lp__      -8.52  -8.42  0.235  0.110  -8.99  -8.32  0.988    15.1      20.4
#> 2 theta      0.464  0.453 0.0856 0.0523  0.337  0.605 0.985     9.66     20.4
#> 3 log_lik.1 -0.783 -0.792 0.186  0.115  -1.09  -0.502 0.985     9.66     20.4
```

## Pathfinder

Use `pathfinder()` for an approximate posterior based on L-BFGS paths.

``` r
pf <- pathfinder(
  mod,
  data,
  max_lbfgs_iters = 100,
  num_paths = 1,
  num_draws = 50,
  seed = 123,
  verbose = FALSE
)

list(
  draws_class = class(pf$draws)[1],
  diagnostics_class = class(pf$diagnostics)[1],
  variables = posterior::variables(pf$draws),
  return_code = pf$return_code
)
#> $draws_class
#> [1] "draws_df"
#> 
#> $diagnostics_class
#> [1] "draws_df"
#> 
#> $variables
#>  [1] "theta"      "log_lik.1"  "log_lik.2"  "log_lik.3"  "log_lik.4" 
#>  [6] "log_lik.5"  "log_lik.6"  "log_lik.7"  "log_lik.8"  "log_lik.9" 
#> [11] "log_lik.10"
#> 
#> $return_code
#> [1] 0

head(summary(pf), 3)
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> # A tibble: 3 × 10
#>   variable    mean median    sd   mad     q5    q95  rhat ess_bulk ess_tail
#>   <chr>      <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 theta      0.491  0.495 0.140 0.135  0.283  0.740 0.994     84.9     34.0
#> 2 log_lik.1 -0.753 -0.703 0.297 0.286 -1.26  -0.301 0.994     84.9     34.0
#> 3 log_lik.2 -0.721 -0.683 0.325 0.252 -1.35  -0.332 0.994     84.9     34.0
```

The return value has `draws`, `diagnostics`, `return_code`, and `args`.
If no pathfinder diagnostic columns are returned, `diagnostics` is
`NULL`.

## Variational Inference

Use `variational()` for ADVI with either a mean-field or full-rank
Gaussian family.

``` r
vb <- variational(
  mod,
  data,
  algorithm = "meanfield",
  iter = 1000,
  output_samples = 50,
  seed = 123,
  verbose = FALSE
)

list(
  draws_class = class(vb$draws)[1],
  variables = posterior::variables(vb$draws),
  return_code = vb$return_code
)
#> $draws_class
#> [1] "draws_df"
#> 
#> $variables
#>  [1] "lp__"       "log_p__"    "log_g__"    "theta"      "log_lik.1" 
#>  [6] "log_lik.2"  "log_lik.3"  "log_lik.4"  "log_lik.5"  "log_lik.6" 
#> [11] "log_lik.7"  "log_lik.8"  "log_lik.9"  "log_lik.10"
#> 
#> $return_code
#> [1] 0

head(summary(vb), 3)
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5      q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>    <dbl> <dbl>    <dbl>    <dbl>
#> 1 lp__      0      0      0    0       0     0       NA        NA       NA  
#> 2 log_p__  -8.71  -8.52   1.49 0.273 -10.7  -8.32     1.01     42.1     46.0
#> 3 log_g__  -0.644 -0.169  1.04 0.240  -3.11 -0.00251  1.01     30.8     17.9
```

The return value has `draws`, `return_code`, and `args`.

## Optimization

Use `optimizing()` to find a posterior mode or maximum likelihood
estimate.

``` r
opt <- optimizing(
  mod,
  data,
  algorithm = "lbfgs",
  seed = 123,
  verbose = FALSE
)

list(
  par = opt$par,
  value = opt$value,
  return_code = opt$return_code
)
#> $par
#>        lp__ converged__       theta   log_lik.1   log_lik.2   log_lik.3 
#>  -6.9314718  31.0000000   0.5000001  -0.6931471  -0.6931473  -0.6931471 
#>   log_lik.4   log_lik.5   log_lik.6   log_lik.7   log_lik.8   log_lik.9 
#>  -0.6931471  -0.6931473  -0.6931471  -0.6931473  -0.6931473  -0.6931471 
#>  log_lik.10 
#>  -0.6931473 
#> 
#> $value
#> [1] -6.931472
#> 
#> $return_code
#> [1] 0

summary(opt)
#>       variable   estimate
#> 1         lp__ -6.9314718
#> 2  converged__ 31.0000000
#> 3        theta  0.5000001
#> 4    log_lik.1 -0.6931471
#> 5    log_lik.2 -0.6931473
#> 6    log_lik.3 -0.6931471
#> 7    log_lik.4 -0.6931471
#> 8    log_lik.5 -0.6931473
#> 9    log_lik.6 -0.6931471
#> 10   log_lik.7 -0.6931473
#> 11   log_lik.8 -0.6931473
#> 12   log_lik.9 -0.6931471
#> 13  log_lik.10 -0.6931473
#> 14        lp__ -6.9314718
```

The return value has `par`, `value`, `return_code`, and `args`.

## Laplace Approximation

Use `laplace()` to draw from a Laplace approximation around a mode. You
can pass the full `optimizing()` result or a named numeric vector of
constrained parameters.

``` r
lap <- laplace(
  mod,
  data,
  mode = opt,
  draws = 20,
  seed = 123,
  verbose = FALSE
)

list(
  draws_class = class(lap$draws)[1],
  variables = posterior::variables(lap$draws),
  return_code = lap$return_code
)
#> $draws_class
#> [1] "draws_df"
#> 
#> $variables
#>  [1] "log_p__"    "log_q__"    "theta"      "log_lik.1"  "log_lik.2" 
#>  [6] "log_lik.3"  "log_lik.4"  "log_lik.5"  "log_lik.6"  "log_lik.7" 
#> [11] "log_lik.8"  "log_lik.9"  "log_lik.10"
#> 
#> $return_code
#> [1] 0

head(summary(lap), 3)
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad      q5     q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#> 1 log_p__  -9.16  -8.78  0.928 0.645 -10.7   -8.33   1.13      19.0     25.6
#> 2 log_q__  -0.586 -0.433 0.764 0.597  -1.34  -0.0106 0.985     20.5     20.4
#> 3 theta     0.596  0.609 0.149 0.181   0.386  0.785  0.954     26.0     25.6
```

The return value has `draws`, `return_code`, and `args`.

## Generated Quantities

Use `generated_quantities()` with existing parameter draws and a model
containing a `generated quantities` block.

``` r
gq <- generated_quantities(
  mod,
  data,
  fitted_params = fit$draws,
  seed = 123,
  verbose = FALSE
)

list(
  draws_class = class(gq$draws)[1],
  variables = posterior::variables(gq$draws),
  return_code = gq$return_code
)
#> $draws_class
#> [1] "draws_df"
#> 
#> $variables
#>  [1] "log_lik.1"  "log_lik.2"  "log_lik.3"  "log_lik.4"  "log_lik.5" 
#>  [6] "log_lik.6"  "log_lik.7"  "log_lik.8"  "log_lik.9"  "log_lik.10"
#> 
#> $return_code
#> [1] 0

head(summary(gq), 3)
#> # A tibble: 3 × 10
#>   variable    mean median    sd   mad    q5    q95  rhat ess_bulk ess_tail
#>   <chr>      <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 log_lik.1 -0.727 -0.723 0.282 0.218 -1.15 -0.357  1.09     16.0     21.1
#> 2 log_lik.2 -0.728 -0.664 0.268 0.197 -1.20 -0.380  1.09     16.0     21.1
#> 3 log_lik.3 -0.727 -0.723 0.282 0.218 -1.15 -0.357  1.09     16.0     21.1
```

The return value has `draws`, `return_code`, and `args`.

## Gradient Check

Use `gradient_check()` to compare autodiff gradients with
finite-difference approximations.

``` r
n_failed <- gradient_check(
  mod,
  data,
  seed = 123,
  verbose = FALSE
)
#> [newstan] All gradient tests passed.

n_failed
#> [1] 0
#> attr(,"class")
#> [1] "StanDiagnose" "integer"

summary(n_failed)
#>   diagnostic value
#> 1 num_failed     0
```

`gradient_check()` returns an integer count of failed parameter gradient
checks.

## Stan Compilation Helpers

`stanc()` compiles Stan code to C++ and returns the generated C++ source
as a single string.

``` r
cpp_code <- stanc(bernoulli_model)
substr(cpp_code, 1, 80)
#> [1] "// Code generated by stanc 4d256d1\n#include <stan/model/model_header.hpp>\nnamesp"
```

`stan_model()` is usually the higher-level entry point: it calls
`stanc()`, compiles the generated C++, and returns the model object used
by the services.
