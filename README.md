<!-- README.md currently documents the implemented R6 API directly. -->

# newstan

`newstan` is an in-process R interface to Stan. It compiles a Stan program to
native code, exposes Stan services through a `StanModel` R6 object, and returns
service-specific `StanFit` R6 objects backed by `posterior` draw formats.

The package is under active development. Its current API follows the shape and
argument names of `cmdstanr`, but not every `cmdstanr` feature is implemented
yet.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("andrjohns/newstan")
```

## Compile a model

Pass a Stan file to `stan_model()`. Compilation happens immediately by default
and the result is a `StanModel` R6 object.

``` r
library(newstan)

mod <- stan_model(
  stan_file = "bernoulli.stan",
  quiet = TRUE
)

mod$model_name()
mod$stan_file()
mod$stan_version()
mod$is_compiled()
```

Use `compile = FALSE` to defer compilation until `$compile()` or a service
method is called:

``` r
mod <- stan_model(
  stan_file = "bernoulli.stan",
  compile = FALSE
)

mod$compile()
```

`stan_model()` also accepts `code` instead of `stan_file`, but exactly one of
the two must be supplied. `include_paths` and `external_cpp` are available for
models with Stan includes or external C++ definitions.

## Run Stan services

Each service is a method on `StanModel`. The methods use the canonical argument
names of the new R6 API and return a specialized fit object.

| Model method | Result class | Purpose |
|---|---|---|
| `$sample()` | `StanMCMC` | HMC/NUTS or fixed-parameter sampling |
| `$optimize()` | `StanMLE` | Optimization |
| `$laplace()` | `StanLaplace` | Laplace approximation |
| `$variational()` | `StanVB` | Mean-field or full-rank ADVI |
| `$pathfinder()` | `StanPathfinder` | Pathfinder approximation |
| `$generate_quantities()` | `StanGQ` | Generated quantities for existing draws |
| `$diagnose()` | `StanDiagnose` | Finite-difference gradient checks |

Assume that `bernoulli.stan` contains data variables `N` and `y`, a scalar
parameter `theta`, and a generated-quantities block:

``` r
data <- list(
  N = 10L,
  y = c(1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 0L)
)

fit <- mod$sample(
  data = data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  adapt_delta = 0.9,
  max_treedepth = 12,
  refresh = 100
)
```

For a model without parameters, set `fixed_param = TRUE` and normally set
`iter_warmup = 0`:

``` r
fixed_mod <- stan_model(stan_file = "fixed-param.stan")
fixed_data <- list(N = 100L)

fixed_fit <- fixed_mod$sample(
  data = fixed_data,
  seed = 123,
  chains = 1,
  iter_warmup = 0,
  iter_sampling = 100,
  fixed_param = TRUE,
  refresh = 0
)
```

An inverse metric can be supplied in memory with `inv_metric`. Use a numeric
vector with `metric = "diag_e"` or a square matrix with
`metric = "dense_e"`; a list can provide one metric per chain.

``` r
metric_fit <- mod$sample(
  data = data,
  chains = 1,
  iter_warmup = 500,
  iter_sampling = 500,
  metric = "diag_e",
  inv_metric = 1,
  step_size = 0.5,
  seed = 123
)
```

Other services use the same model object:

``` r
opt <- mod$optimize(
  data = data,
  algorithm = "lbfgs",
  iter = 2000,
  seed = 123
)

lap <- mod$laplace(
  data = data,
  mode = opt,
  jacobian = FALSE,
  draws = 1000,
  seed = 123
)

vb <- mod$variational(
  data = data,
  algorithm = "meanfield",
  iter = 10000,
  draws = 1000,
  seed = 123
)

pf <- mod$pathfinder(
  data = data,
  num_paths = 4,
  single_path_draws = 1000,
  draws = 1000,
  seed = 123
)

gq <- mod$generate_quantities(
  fitted_params = fit,
  data = data,
  seed = 123
)

diagnosis <- mod$diagnose(
  data = data,
  seed = 123,
  epsilon = 1e-6,
  error = 1e-6
)
```

`$laplace()` accepts a `StanMLE` result or a named constrained parameter vector
as `mode`. `$generate_quantities()` accepts a `StanFit` or a compatible draws
object as `fitted_params`.

## Work with fit objects

All service results inherit from `StanFit`. Use methods rather than accessing
implementation fields:

``` r
fit$draws()
fit$draws(variables = "theta", format = "draws_df")
fit$summary(variables = "theta")
fit$return_codes()
fit$metadata()
fit$time()
fit$output()
fit$init()
fit$code()
```

Supported draw formats are `"draws_array"`, `"draws_matrix"`, `"draws_df"`,
`"draws_list"`, and `"rvars"`. `StanMCMC` uses `draws_array` by default;
other draw-producing fits use their service-specific default. Request a format
explicitly when downstream code requires one.

`StanMCMC` adds sampling-specific accessors:

``` r
fit$num_chains()
fit$sampler_diagnostics()
fit$diagnostic_summary()
fit$inv_metric()
```

When warmup was retained with `save_warmup = TRUE`, pass
`inc_warmup = TRUE` to `$draws()` or `$sampler_diagnostics()`.

Other specialized accessors are:

``` r
opt$mle()
lap$mode()
gq$num_chains()
diagnosis$num_failed()
diagnosis$gradients()
```

Persist a fit with `$save_object()`:

``` r
fit$save_object("bernoulli-fit.rds")
fit2 <- readRDS("bernoulli-fit.rds")
```

Native model bindings are restored automatically when a model-evaluation
method is first needed after deserialization.

## Evaluate the model log density

Fit objects retain the data-bound native model and expose log-density and
parameter-transformation methods. Inputs to the density methods are on the
unconstrained scale.

``` r
upars <- fit$unconstrain_variables(list(theta = 0.5))

fit$log_prob(upars, jacobian = TRUE)
fit$grad_log_prob(upars, jacobian = TRUE)
fit$hessian(upars, jacobian = TRUE)

fit$constrain_variables(
  upars,
  transformed_parameters = TRUE,
  generated_quantities = TRUE
)

fit$variable_skeleton(
  transformed_parameters = TRUE,
  generated_quantities = TRUE
)
```

`$constrain_variables()` can run transformed parameters and generated
quantities, so its random-number stream is controlled separately. Reset that
stream with `$init_model_methods(seed)` when reproducibility is required.

Convert all constrained parameter draws at once with `$unconstrain_draws()`:

``` r
unconstrained <- fit$unconstrain_draws(format = "draws_matrix")
```

## Current limitations

The R6 classes and in-process services are implemented, but several parts of
the `cmdstanr`-shaped interface are intentionally unavailable at present:

- File-backed service output is not implemented. Non-default `output_dir`,
  `output_basename`, `sig_figs`, and `save_cmdstan_config` requests produce an
  error. `metric_file` is also unsupported; use `inv_metric` in memory.
- OpenCL execution is not implemented, so `opencl_ids` is rejected.
- Saving latent dynamics is not implemented.
- The native runner currently exposes aggregate service status and output
  metadata. Consequently `$return_codes()`, `$output()`, and `$time()` do not
  yet provide complete independent per-chain records; for multi-chain fits an
  aggregate return code may be repeated for each chain.
- Jacobian-adjusted optimization is not implemented. Calling `$optimize()`
  with `jacobian = TRUE` errors. If `$laplace()` needs to optimize its own
  mode, use `jacobian = FALSE`; alternatively supply an already computed
  `mode`.
- `StanModel$variables()` currently returns a placeholder structure whose
  sections contain `NULL`, rather than parsed Stan variable metadata.
- Non-empty `cpp_options` and `stanc_options` are currently rejected, and
  `user_header` is not supported. Use `external_cpp` for external C++ code.
- The in-process backend does not yet reproduce every file, console-output,
  diagnostic, timing, and metric artifact available from CmdStan.

Unsupported options fail explicitly rather than being silently ignored.
