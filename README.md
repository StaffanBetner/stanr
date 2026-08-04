
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stanr

<!-- badges: start -->

<!-- badges: end -->

`stanr` is an R interface for compiling Stan programs and running Stan
services directly from R. It uses an R6-based API similar to `cmdstanr`:
`stan_model()` returns a `StanModel` object, and inference methods
(`$sample()`, `$optimize()`, `$variational()`, etc.) return R6 fit
objects with methods for extracting draws, summaries, and diagnostics.
Results use `posterior` draw objects where possible.

## Inference Methods

| Method | Purpose | Return type |
|----|----|----|
| `$sample()` | MCMC sampling (HMC/NUTS, static HMC, fixed param) | `StanMCMC` |
| `$optimize()` | Posterior mode or maximum likelihood estimate | `StanMLE` |
| `$laplace()` | Laplace approximation draws around a mode | `StanLaplace` |
| `$variational()` | ADVI approximate posterior draws | `StanVB` |
| `$pathfinder()` | Pathfinder approximate posterior draws | `StanPathfinder` |
| `$generate_quantities()` | Generated quantities from existing draws | `StanGQ` |
| `$diagnose()` | Gradient checking diagnostic | `StanDiagnose` |

## Fit Object Methods

All fit objects inherit from `StanFit` and share common methods:

| Method | Description |
|----|----|
| `$draws()` | Extract draws as `posterior` objects |
| `$summary()` | Summarize draws via `posterior::summarise_draws()` |
| `$print()` | Print a summary table |
| `$return_codes()` | Stan return codes (0 = success) |
| `$metadata()` | Fit metadata (seed, data, arguments) |
| `$time()` | Timing information |
| `$log_prob()` | Evaluate log probability |
| `$constrain_variables()` | Constrain unconstrained parameters |
| `$unconstrain_variables()` | Unconstrain parameters |
| `$save_object()` | Save fit to file |

## Installation

You can install the development version of stanr from GitHub:

``` r
# install.packages("pak")
pak::pak("andrjohns/stanr")
```

## Compile a Model

Models are compiled from a string or from a `.stan` file. `stan_model()`
returns a `StanModel` R6 object.

``` r
library(stanr)

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
```

## Sampling

Call `$sample()` on a `StanModel` to run MCMC. The result is a
`StanMCMC` object with methods for extracting draws and diagnostics.

``` r
fit <- mod$sample(
  data = data,
  iter_warmup = 20,
  iter_sampling = 20,
  chains = 2,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(fit$draws())
#>  [1] "lp__"        "theta"       "log_lik[1]"  "log_lik[2]"  "log_lik[3]" 
#>  [6] "log_lik[4]"  "log_lik[5]"  "log_lik[6]"  "log_lik[7]"  "log_lik[8]" 
#> [11] "log_lik[9]"  "log_lik[10]"
```

Extract a summary via `$summary()`:

``` r
fit$summary()
#> # A tibble: 12 × 10
#>    variable      mean median    sd   mad     q5    q95  rhat ess_bulk ess_tail
#>    <chr>        <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__        -8.73  -8.47  0.612 0.210 -9.99  -8.32   1.24     10.6     22.4
#>  2 theta        0.500  0.485 0.126 0.101  0.316  0.700  1.20     21.1     21.4
#>  3 log_lik[1]  -0.727 -0.723 0.282 0.218 -1.15  -0.357  1.16     21.1     21.4
#>  4 log_lik[2]  -0.728 -0.664 0.268 0.197 -1.20  -0.380  1.20     21.1     21.4
#>  5 log_lik[3]  -0.727 -0.723 0.282 0.218 -1.15  -0.357  1.16     21.1     21.4
#>  6 log_lik[4]  -0.727 -0.723 0.282 0.218 -1.15  -0.357  1.16     21.1     21.4
#>  7 log_lik[5]  -0.728 -0.664 0.268 0.197 -1.20  -0.380  1.20     21.1     21.4
#>  8 log_lik[6]  -0.727 -0.723 0.282 0.218 -1.15  -0.357  1.16     21.1     21.4
#>  9 log_lik[7]  -0.728 -0.664 0.268 0.197 -1.20  -0.380  1.20     21.1     21.4
#> 10 log_lik[8]  -0.728 -0.664 0.268 0.197 -1.20  -0.380  1.20     21.1     21.4
#> 11 log_lik[9]  -0.727 -0.723 0.282 0.218 -1.15  -0.357  1.16     21.1     21.4
#> 12 log_lik[10] -0.728 -0.664 0.268 0.197 -1.20  -0.380  1.20     21.1     21.4
```

Access sampler diagnostics and chain information:

``` r
list(
  chains = fit$num_chains(),
  return_codes = fit$return_codes(),
  diagnostics = fit$diagnostic_summary()
)
#> $chains
#> [1] 2
#> 
#> $return_codes
#> [1] 0 0
#> 
#> $diagnostics
#>   chain num_divergent num_max_treedepth
#> 1     1             0                 0
#> 2     2             0                 0
```

### Fixed Param

For models without parameters, use `fixed_param = TRUE`:

``` r
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

fixed_fit <- fixed_mod$sample(
  data = list(N = 5),
  iter_warmup = 0,
  iter_sampling = 10,
  fixed_param = TRUE,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(fixed_fit$draws())
#> [1] "lp__"     "y_rep[1]" "y_rep[2]" "y_rep[3]" "y_rep[4]" "y_rep[5]"
```

### Supplying Inverse Metrics

`$sample()` accepts precomputed inverse metric values through
`inv_metric`.

For a diagonal metric, pass a numeric vector with one value per
unconstrained parameter:

``` r
diag_fit <- mod$sample(
  data = data,
  metric = "diag_e",
  inv_metric = c(1.0),
  iter_warmup = 20,
  iter_sampling = 20,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(diag_fit$draws())
#>  [1] "lp__"        "theta"       "log_lik[1]"  "log_lik[2]"  "log_lik[3]" 
#>  [6] "log_lik[4]"  "log_lik[5]"  "log_lik[6]"  "log_lik[7]"  "log_lik[8]" 
#> [11] "log_lik[9]"  "log_lik[10]"
```

For a dense metric, pass a square matrix. To provide one metric per
chain, pass a list whose length equals `chains`:

``` r
dense_fit <- mod$sample(
  data = data,
  metric = "dense_e",
  inv_metric = list(matrix(1.0, nrow = 1, ncol = 1)),
  chains = 1,
  iter_warmup = 20,
  iter_sampling = 20,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(dense_fit$draws())
#>  [1] "lp__"        "theta"       "log_lik[1]"  "log_lik[2]"  "log_lik[3]" 
#>  [6] "log_lik[4]"  "log_lik[5]"  "log_lik[6]"  "log_lik[7]"  "log_lik[8]" 
#> [11] "log_lik[9]"  "log_lik[10]"
```

## Pathfinder

Call `$pathfinder()` for an approximate posterior based on L-BFGS paths.

``` r
pf <- mod$pathfinder(
  data = data,
  max_lbfgs_iters = 100,
  num_paths = 1,
  draws = 50,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(pf$draws())
#>  [1] "lp_approx__" "lp__"        "path__"      "theta"       "log_lik[1]" 
#>  [6] "log_lik[2]"  "log_lik[3]"  "log_lik[4]"  "log_lik[5]"  "log_lik[6]" 
#> [11] "log_lik[7]"  "log_lik[8]"  "log_lik[9]"  "log_lik[10]"
pf$summary()
#> # A tibble: 14 × 10
#>    variable      mean median    sd   mad      q5    q95   rhat ess_bulk ess_tail
#>    <chr>        <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl>
#>  1 lp_approx__ -0.931 -0.653 0.735 0.295  -2.34  -0.439  1.00     1085.     979.
#>  2 lp__        -8.86  -8.57  0.770 0.340 -10.4   -8.32   1.00     1095.     941.
#>  3 path__       1      1     0     0       1      1     NA          NA       NA 
#>  4 theta        0.510  0.515 0.141 0.152   0.275  0.738  0.999     953.     906.
#>  5 log_lik[1]  -0.716 -0.663 0.306 0.287  -1.29  -0.303  0.999     953.     906.
#>  6 log_lik[2]  -0.761 -0.724 0.320 0.306  -1.34  -0.322  0.999     953.     906.
#>  7 log_lik[3]  -0.716 -0.663 0.306 0.287  -1.29  -0.303  0.999     953.     906.
#>  8 log_lik[4]  -0.716 -0.663 0.306 0.287  -1.29  -0.303  0.999     953.     906.
#>  9 log_lik[5]  -0.761 -0.724 0.320 0.306  -1.34  -0.322  0.999     953.     906.
#> 10 log_lik[6]  -0.716 -0.663 0.306 0.287  -1.29  -0.303  0.999     953.     906.
#> 11 log_lik[7]  -0.761 -0.724 0.320 0.306  -1.34  -0.322  0.999     953.     906.
#> 12 log_lik[8]  -0.761 -0.724 0.320 0.306  -1.34  -0.322  0.999     953.     906.
#> 13 log_lik[9]  -0.716 -0.663 0.306 0.287  -1.29  -0.303  0.999     953.     906.
#> 14 log_lik[10] -0.761 -0.724 0.320 0.306  -1.34  -0.322  0.999     953.     906.
```

## Variational Inference

Call `$variational()` for ADVI with either a mean-field or full-rank
Gaussian family.

``` r
vb <- mod$variational(
  data = data,
  algorithm = "meanfield",
  iter = 1000,
  draws = 50,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(vb$draws())
#>  [1] "lp_approx__" "theta"       "log_lik[1]"  "log_lik[2]"  "log_lik[3]" 
#>  [6] "log_lik[4]"  "log_lik[5]"  "log_lik[6]"  "log_lik[7]"  "log_lik[8]" 
#> [11] "log_lik[9]"  "log_lik[10]"
vb$summary()
#> # A tibble: 12 × 10
#>    variable      mean median    sd   mad     q5      q95  rhat ess_bulk ess_tail
#>    <chr>        <dbl>  <dbl> <dbl> <dbl>  <dbl>    <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp_approx__ -0.657 -0.181 1.04  0.253 -3.14  -0.00320 0.999     38.4     20.6
#>  2 theta        0.501  0.503 0.144 0.134  0.212  0.712   1.01      84.7     46.0
#>  3 log_lik[1]  -0.743 -0.687 0.352 0.250 -1.55  -0.339   1.01      84.7     46.0
#>  4 log_lik[2]  -0.737 -0.700 0.294 0.259 -1.25  -0.239   1.01      84.7     46.0
#>  5 log_lik[3]  -0.743 -0.687 0.352 0.250 -1.55  -0.339   1.01      84.7     46.0
#>  6 log_lik[4]  -0.743 -0.687 0.352 0.250 -1.55  -0.339   1.01      84.7     46.0
#>  7 log_lik[5]  -0.737 -0.700 0.294 0.259 -1.25  -0.239   1.01      84.7     46.0
#>  8 log_lik[6]  -0.743 -0.687 0.352 0.250 -1.55  -0.339   1.01      84.7     46.0
#>  9 log_lik[7]  -0.737 -0.700 0.294 0.259 -1.25  -0.239   1.01      84.7     46.0
#> 10 log_lik[8]  -0.737 -0.700 0.294 0.259 -1.25  -0.239   1.01      84.7     46.0
#> 11 log_lik[9]  -0.743 -0.687 0.352 0.250 -1.55  -0.339   1.01      84.7     46.0
#> 12 log_lik[10] -0.737 -0.700 0.294 0.259 -1.25  -0.239   1.01      84.7     46.0
```

## Optimization

Call `$optimize()` to find a posterior mode or maximum likelihood
estimate.

``` r
opt <- mod$optimize(
  data = data,
  algorithm = "lbfgs",
  seed = 123,
  show_messages = FALSE
)

opt$mle()
#>       theta  log_lik[1]  log_lik[2]  log_lik[3]  log_lik[4]  log_lik[5] 
#>   0.5000001  -0.6931471  -0.6931473  -0.6931471  -0.6931471  -0.6931473 
#>  log_lik[6]  log_lik[7]  log_lik[8]  log_lik[9] log_lik[10] 
#>  -0.6931471  -0.6931473  -0.6931473  -0.6931471  -0.6931473
opt$summary()
#>       variable   estimate
#> 1        theta  0.5000001
#> 2   log_lik[1] -0.6931471
#> 3   log_lik[2] -0.6931473
#> 4   log_lik[3] -0.6931471
#> 5   log_lik[4] -0.6931471
#> 6   log_lik[5] -0.6931473
#> 7   log_lik[6] -0.6931471
#> 8   log_lik[7] -0.6931473
#> 9   log_lik[8] -0.6931473
#> 10  log_lik[9] -0.6931471
#> 11 log_lik[10] -0.6931473
```

## Laplace Approximation

Call `$laplace()` to draw from a Gaussian approximation around a mode.
Pass a `StanMLE` object or let `$laplace()` run optimization first.

``` r
lap <- mod$laplace(
  data = data,
  mode = opt,
  draws = 20,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(lap$draws())
#>  [1] "lp__"        "lp_approx__" "theta"       "log_lik[1]"  "log_lik[2]" 
#>  [6] "log_lik[3]"  "log_lik[4]"  "log_lik[5]"  "log_lik[6]"  "log_lik[7]" 
#> [11] "log_lik[8]"  "log_lik[9]"  "log_lik[10]"
lap$summary()
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
#> # A tibble: 13 × 10
#>    variable      mean median    sd   mad      q5     q95  rhat ess_bulk ess_tail
#>    <chr>        <dbl>  <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__        -9.16  -8.78  0.928 0.645 -10.7   -8.33   1.13      19.0     25.6
#>  2 lp_approx__ -0.586 -0.433 0.764 0.597  -1.34  -0.0106 0.985     20.5     20.4
#>  3 theta        0.596  0.609 0.149 0.181   0.386  0.785  0.954     26.0     25.6
#>  4 log_lik[1]  -0.552 -0.497 0.283 0.323  -0.956 -0.242  0.953     26.0     25.6
#>  5 log_lik[2]  -0.974 -0.939 0.384 0.404  -1.54  -0.488  0.972     26.0     25.6
#>  6 log_lik[3]  -0.552 -0.497 0.283 0.323  -0.956 -0.242  0.953     26.0     25.6
#>  7 log_lik[4]  -0.552 -0.497 0.283 0.323  -0.956 -0.242  0.953     26.0     25.6
#>  8 log_lik[5]  -0.974 -0.939 0.384 0.404  -1.54  -0.488  0.972     26.0     25.6
#>  9 log_lik[6]  -0.552 -0.497 0.283 0.323  -0.956 -0.242  0.953     26.0     25.6
#> 10 log_lik[7]  -0.974 -0.939 0.384 0.404  -1.54  -0.488  0.972     26.0     25.6
#> 11 log_lik[8]  -0.974 -0.939 0.384 0.404  -1.54  -0.488  0.972     26.0     25.6
#> 12 log_lik[9]  -0.552 -0.497 0.283 0.323  -0.956 -0.242  0.953     26.0     25.6
#> 13 log_lik[10] -0.974 -0.939 0.384 0.404  -1.54  -0.488  0.972     26.0     25.6
```

## Generated Quantities

Call `$generate_quantities()` with a `StanFit` object or draws matrix to
evaluate the generated quantities block.

``` r
gq <- mod$generate_quantities(
  fitted_params = fit,
  data = data,
  seed = 123,
  show_messages = FALSE
)

posterior::variables(gq$draws())
#>  [1] "log_lik[1]"  "log_lik[2]"  "log_lik[3]"  "log_lik[4]"  "log_lik[5]" 
#>  [6] "log_lik[6]"  "log_lik[7]"  "log_lik[8]"  "log_lik[9]"  "log_lik[10]"
gq$summary()
#> # A tibble: 10 × 10
#>    variable      mean median    sd   mad    q5    q95  rhat ess_bulk ess_tail
#>    <chr>        <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#>  1 log_lik[1]  -0.727 -0.723 0.282 0.218 -1.15 -0.357  1.09     16.0     21.1
#>  2 log_lik[2]  -0.728 -0.664 0.268 0.197 -1.20 -0.380  1.09     16.0     21.1
#>  3 log_lik[3]  -0.727 -0.723 0.282 0.218 -1.15 -0.357  1.09     16.0     21.1
#>  4 log_lik[4]  -0.727 -0.723 0.282 0.218 -1.15 -0.357  1.09     16.0     21.1
#>  5 log_lik[5]  -0.728 -0.664 0.268 0.197 -1.20 -0.380  1.09     16.0     21.1
#>  6 log_lik[6]  -0.727 -0.723 0.282 0.218 -1.15 -0.357  1.09     16.0     21.1
#>  7 log_lik[7]  -0.728 -0.664 0.268 0.197 -1.20 -0.380  1.09     16.0     21.1
#>  8 log_lik[8]  -0.728 -0.664 0.268 0.197 -1.20 -0.380  1.09     16.0     21.1
#>  9 log_lik[9]  -0.727 -0.723 0.282 0.218 -1.15 -0.357  1.09     16.0     21.1
#> 10 log_lik[10] -0.728 -0.664 0.268 0.197 -1.20 -0.380  1.09     16.0     21.1
```

## Gradient Check

Call `$diagnose()` to compare autodiff gradients with finite-difference
approximations.

``` r
diag <- mod$diagnose(
  data = data,
  seed = 123
)
#> TEST GRADIENT MODE
#> 
#>  Log probability=-12.776
#> 
#>  param idx           value           model     finite diff           error
#>          0         1.83245        -4.34465        -4.34465      2.2707e-09
#> [stanr] All gradient tests passed.

list(
  num_failed = diag$num_failed(),
  lp = diag$lp(),
  gradients = diag$gradients()
)
#> $num_failed
#> [1] 0
#> 
#> $lp
#> [1] -12.776
#> 
#> $gradients
#>   param_idx   value    model finite_diff      error
#> 1         0 1.83245 -4.34465    -4.34465 2.2707e-09
```

## Tuples and Complex Numbers

Stan `tuple(...)` values map to/from **unnamed** R lists (one element
per slot; arrays of tuples become lists of such lists), and `complex` /
`complex_vector` / `complex_row_vector` / `complex_matrix` map to/from
R’s native complex type – for `data =`, `init =`, draws, and exposed
functions alike.

``` r
tuple_complex_code <- "
functions {
  tuple(real, vector) split_stat(vector x) {
    return (mean(x), head(x, 2));
  }
  complex_vector rotate(complex_vector z, complex phase) {
    return z * phase;
  }
}
"

tc_mod <- stan_model(
  code = tuple_complex_code,
  model_name = "tuple_complex",
  compile = FALSE
)
tc_mod$expose_stan_functions()

# tuple(real, vector) comes back as an unnamed list: the scalar slot, then
# the vector slot.
tc_mod$functions$split_stat(c(1, 2, 3, 4))
#> [[1]]
#> [1] 2.5
#> 
#> [[2]]
#> [1] 1 2

# complex_vector arguments/returns round-trip as native R complex vectors.
tc_mod$functions$rotate(c(1 + 2i, 3 - 1i), 1i)
#> [1] -2+1i  1+3i
```

## Stan Compilation Helpers

`stanc()` compiles Stan code to C++ and returns the generated C++ source
as a single string.

``` r
cpp_code <- stanc(bernoulli_model)
substr(cpp_code, 1, 80)
#> [1] "// Code generated by stanc 4d256d1\n#include <stan/model/model_header.hpp>\nnamesp"
```

`stan_model()` is the higher-level entry point: it calls `stanc()`,
compiles the generated C++, and returns a `StanModel` object used by all
inference methods.
