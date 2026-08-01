# Aligning `newstan` with the `cmdstanr` API

## Purpose and comparison baseline

This document is an implementation guide for changing `newstan` from its
current procedural/S3 API to an R6 API that is intentionally familiar to
`cmdstanr` users. It compares the checked-out repositories as of 2026-07-31:

- `newstan` 0.1.0 (`newstan/DESCRIPTION`), which bundles Stan 2.39.0 and runs
  Stan services in-process through Rcpp.
- `cmdstanr` 0.9.0.9001 (`cmdstanr/DESCRIPTION`), which compiles CmdStan
  executables, launches processes, and reads CmdStan files.

The target should be *caller-level and result-level parity where the two
architectures have the same concept*. It should not pretend that an in-process
library is a CmdStan executable. In particular, CmdStan installation/path
management, executable inspection, MPI process launching, and CmdStan command
line utilities are not sensible requirements for the first parity milestone.

## Executive summary

The required change is substantially larger than putting the existing return
lists inside R6 objects.

1. `stan_model()` should return a model R6 object with `$sample()`,
   `$optimize()`, `$laplace()`, `$variational()`, `$pathfinder()`,
   `$generate_quantities()`, and `$diagnose()` methods. The compiled Rcpp
   environment should become an internal implementation detail.
2. Each service should return a method-specific R6 fit object inheriting from
   a shared fit superclass. Users should access values through methods such as
   `$draws()`, `$summary()`, `$return_codes()`, and `$metadata()`, not mutable
   list fields.
3. Public method arguments should use CmdStanR names and shapes. The largest
   changes are `chains`, `iter_warmup`, `iter_sampling`, `chain_ids`,
   `threads_per_chain`, `fixed_param`, `adapt_delta`, `step_size`,
   `max_treedepth`, Pathfinder's `single_path_draws`/`draws`, and ADVI's
   `draws`.
4. The native result contract must be expanded. It currently discards or
   conflates information required by fit methods: warmup versus sampling,
   per-chain statuses, initial values, adapted metrics, diagnostic output,
   timing, service messages, and several structured-writer results.
5. Output normalization must match CmdStanR conventions. This includes Stan
   array names such as `beta[1]`, posterior draw formats, omission of ADVI's
   mean row, renaming `log_p__`/`log_g__`/`log_q__`, and retaining Pathfinder
   log-density columns in the main draw object.
6. Features that can be implemented entirely in R should follow the R6 work
   quickly: variable selection, draw format conversion, summaries, printing,
   `lp()`, `lp_approx()`, `mle()`, `mode()`, `num_chains()`, serialization, and
   posterior diagnostics. Features requiring native output should be designed
   before finalizing the R6 constructor contract.
7. Model evaluation and transformation methods are part of core parity, not a
   later optional enhancement. Every data-bound fit must provide
   `$log_prob()`, `$grad_log_prob()`, `$hessian()`, `$constrain_variables()`,
   `$unconstrain_variables()`, `$unconstrain_draws()`, and
   `$variable_skeleton()` through native bindings compiled with the model.

The class names are `StanModel`, `StanFit`, `StanMCMC`, `StanMLE`,
`StanLaplace`, `StanVB`, `StanPathfinder`, `StanGQ`, and `StanDiagnose`. Their
methods and arguments mirror CmdStanR, while avoiding `CmdStan*` names that
would imply the presence of a CmdStan runset or executable.

## Current `newstan` architecture

### Model compilation

`R/stan_model.R` currently:

- accepts exactly one of `file` and `code`;
- calls the QuickJS-backed `stanc()` wrapper;
- optionally prepends `external_cpp` source;
- appends `inst/stan_model.cpp` to the generated translation unit;
- compiles it through `Rcpp::sourceCpp()` into a new environment; and
- returns that environment directly.

The returned environment publicly exposes generated bindings such as
`new_model()`, `run_model()`, and `constrained_param_names()`. It does not
retain a stable public description of the source file, code, model name,
include paths, compilation options, generated C++, or compiled-library path.

The current compiler has useful newstan-specific features worth retaining:

- compilation directly from a string;
- nested Stan include expansion;
- external C++ definitions;
- content-hash/sourceCpp caching; and
- optional cached precompiled headers.

### Service functions and results

The public functions are `sampling()`, `optimizing()`, `laplace()`,
`variational()`, `pathfinder()`, `generated_quantities()`, and
`gradient_check()`. They take the compiled environment as their first argument
and return S3 lists (or, for diagnosis, an integer with an S3 class).

| Current function | Current class | Main public fields |
|---|---|---|
| `sampling()` | `StanSample` | `draws`, `diagnostics`, `return_code`, `args` |
| `optimizing()` | `StanOptimize` | `par`, `value`, `return_code`, `args` |
| `laplace()` | `StanLaplace` | `draws`, `return_code`, `args` |
| `variational()` | `StanVariational` | `draws`, `return_code`, `args` |
| `pathfinder()` | `StanPathfinder` | `draws`, `diagnostics`, `return_code`, `args` |
| `generated_quantities()` | `StanGeneratedQuantities` | `draws`, `return_code`, `args` |
| `gradient_check()` | `StanDiagnose` | integer number of failed checks |

Only S3 `summary()` methods are provided. Results have no stable methods for
subsetting or format conversion, no model/run metadata contract, and no
method-specific extraction helpers.

### Native output limitations

The native writers in `src/include/` collect samples efficiently, but their
current result shape constrains the R API:

- `r_sample_writer::operator()(const std::string&)` discards Stan comments,
  including information that can be used for timing and run metadata.
- Sampling stacks all chains and adds only `.chain`. It returns one aggregate
  return code.
- Sampling creates init and metric writers but does not return their contents.
- The sampler diagnostic writer is a discard writer, so latent dynamics cannot
  be returned or saved.
- Saved warmup and post-warmup rows are in the same matrix with no phase marker.
- The shared logger prints buffered messages but does not retain them on the
  result object.
- ADVI returns the first mean row as if it were an approximate posterior draw.
- Diagnose discards the gradient table and only returns the failed count.
- Pathfinder structured diagnostics and per-path products are written into
  string streams but not returned to R.
- Laplace's Hessian structured output is not returned.
- No service returns elapsed time or model profiling data.

These are native-result design issues, not problems an R6 wrapper can repair
after the fact.

## Target object model

### `StanModel`

`stan_model()` remains the public factory but returns a `StanModel`. The R6
generator itself can remain internal, as `CmdStanModel` generally does in
CmdStanR's public workflow.

Recommended private state:

- original file path, code split into lines, model name, and include paths;
- external C++/user-header paths and normalized stanc/C++ options;
- PCH and cache settings;
- generated C++ and content hash;
- compiled sourceCpp environment and, if discoverable, shared-library path;
- bundled Stan and stanc versions; and
- enough compilation information to rebuild native bindings after
  serialization or a new R session.

Recommended model methods for the first two milestones:

| Method | Priority | Required behavior |
|---|---:|---|
| `$sample()` | P0 | CmdStanR-style arguments; returns `StanMCMC` |
| `$optimize()` | P0 | CmdStanR-style arguments; returns `StanMLE` |
| `$laplace()` | P0 | accepts an MLE or runs optimization; returns `StanLaplace` |
| `$variational()` | P0 | returns `StanVB` |
| `$pathfinder()` | P0 | returns `StanPathfinder` |
| `$generate_quantities()` | P0 | returns `StanGQ` and preserves chains |
| `$diagnose()` | P0 | returns `StanDiagnose` |
| `$code()`, `$print()`, `$model_name()` | P0 | expose retained source metadata |
| `$stan_file()`, `$has_stan_file()`, `$include_paths()` | P0 | CmdStanR-like accessors |
| `$compile()` | P1 | recompile/update the same R6 object invisibly |
| `$variables()` | P0 | structured data/parameter/transformed/GQ declarations and transform metadata |
| `$check_syntax()` | P1 | stanc validation without native compilation |
| `$format()` | P1 | stanc auto-format/canonicalize support |
| `$hpp_file()`/`$save_hpp_file()` | P2 | expose/save generated C++ where meaningful |
| `$expose_functions()` | P2 | expose functions-block functions to R |
| `$stan_defaults()` | P1 | defaults for the bundled Stan version |

Use `$stan_version()` rather than `$cmdstan_version()`, which would imply a
concept that does not exist. Similarly, `$dll_file()` or `$compiled_file()` is
more honest than `$exe_file()`.

### `StanFit` and subclasses

All successful and failed service calls should return an R6 result. A failed
run must still support `$return_codes()`, `$metadata()`, `$output()`, and
`$time()`; methods that require missing draws should fail with a clear
method-specific message.

Recommended common private state:

- post-warmup/main draws;
- warmup draws when applicable;
- normalized method arguments and metadata;
- a vector of per-run/per-chain return codes;
- user-supplied initial values in normalized list-of-lists form;
- retained console messages/exceptions;
- total and per-chain timing;
- retained Stan code and a rebuildable model descriptor;
- original input data or a serialization-safe copy;
- a live, data-bound generated-model external pointer when available;
- a dedicated RNG external pointer for transformed parameters/generated
  quantities evaluated by model methods;
- profiles and optional persisted artifact paths; and
- the service-specific native payload.

Because newstan results are already in memory, `$materialize()` can be a
documented no-op returning `self` invisibly. `$save_object()` is still useful,
but serialization tests must cover loaded DLL/native-symbol lifetimes. A saved
fit should retain all draw and metadata methods even when model-evaluation
methods need to recompile bindings.

## Model factory and compilation API changes

The public factory should accept the existing newstan conveniences while
adopting CmdStanR vocabulary. One workable signature is:

```r
stan_model <- function(
  stan_file = NULL,
  code = NULL,
  compile = TRUE,
  model_name = NULL,
  include_paths = NULL,
  user_header = NULL,
  cpp_options = list(),
  stanc_options = list(),
  force_recompile = getOption("newstan_force_recompile", FALSE),
  precompiled_headers = FALSE,
  quiet = TRUE,
  external_cpp = NULL
)
```

API rules:

- `external_cpp` remains a newstan extension. `user_header` should mean a
  header included during compilation, while `external_cpp` retains its current
  “prepend these contents” behavior; do not silently treat the two as
  identical.
- `compile = FALSE` should construct an inspectable model object without a
  native binding. Fitting methods should then compile lazily or give a direct
  instruction to call `$compile()`.
- Source-string compilation remains supported. `stan_file()` should return
  `character(0)` and `has_stan_file()` should return `FALSE` for code-only
  models, while `code()` always works.
- Store normalized options on the model. At present most stanc flags exposed
  by `stanc()` cannot be specified through `stan_model()`.

The bundled `stanc.js` advertises `info`, `auto-format`, `canonicalize`,
pedantic/uninitialized warnings, OpenCL generation, and standalone-functions
flags. Reuse that compiler rather than reproducing syntax parsing in R.

## Service method argument alignment

### Common conventions

Use `NULL` in public signatures when CmdStanR uses `NULL`, then resolve it to a
bundled Stan 2.39 default in one normalization layer. This gives familiar
signatures without sacrificing deterministic internal values. The resolved
values belong in `$metadata()`; user-specified versus defaulted values can be
recorded separately if useful.

| Current newstan convention | Target convention | Notes |
|---|---|---|
| `data` required list | `data = NULL`, list or supported file path | Add no-data models and file input. Validate against model declarations. |
| `seed = NA` | `seed = NULL` | Replace the old convention and store the generated seed. |
| `id` | `chain_ids` for MCMC; internal ID otherwise | MCMC needs a unique vector; non-MCMC normally uses ID 1 internally. |
| `init = 2` | `init = NULL` | Support radius, list, list-of-lists, function, and supported file paths. Return user inits via `$init()`. |
| `verbose` | `show_messages`, `show_exceptions` | The logger must distinguish and retain normal messages versus errors. |
| `num_threads` | `threads`, or `threads_per_chain` for MCMC/GQ | Use only the new names; do not reproduce deprecated CmdStanR aliases. |
| explicit service defaults | public `NULL`, normalized bundled defaults | Implement a single source of defaults and validation. |
| `...` silently unused | no catch-all, or reject unknown names | Silent acceptance masks misspelled CmdStanR arguments. |

CmdStanR's common file arguments—`output_dir`, `output_basename`, `sig_figs`,
`save_cmdstan_config`, and sometimes `save_latent_dynamics`—need an explicit
policy. They should be present only if they have a defined newstan behavior;
otherwise reject non-default use with “not supported by the in-process
backend.” Silently ignoring them is not acceptable. A later milestone can
serialize canonical CSV/JSON artifacts from the in-memory run.

`opencl_ids` should remain unsupported until compilation and linking actually
enable Stan OpenCL. The current `stanc(use_opencl = TRUE)` flag alone is not a
complete OpenCL implementation.

### Sampling

Target name and result:

```r
fit <- model$sample(...)  # StanMCMC
```

| Current `sampling()` argument | CmdStanR/target argument | Required change |
|---|---|---|
| `num_warmup` | `iter_warmup` | Replace the old name. |
| `num_samples` | `iter_sampling` | Replace the old name. |
| `num_chains = 1` | `chains = 4` | Align default and validate scalar integer. |
| no equivalent | `parallel_chains = getOption("mc.cores", 1)` | Add scheduling control distinct from within-chain threads. |
| `id = 1` | `chain_ids = seq_len(chains)` | Support arbitrary unique IDs and use them for RNG advancement. |
| `num_threads` | `threads_per_chain` | Define total TBB concurrency for parallel chains; do not conflate chain count with within-chain threads. |
| `algorithm = "hmc"/"fixed_param"` | `fixed_param = FALSE` | Canonical public switch. |
| `max_depth` | `max_treedepth` | Rename. |
| `delta` | `adapt_delta` | Rename. |
| `stepsize` | `step_size` | Rename. |
| `metric` | `metric` | Same values: `unit_e`, `diag_e`, `dense_e`. |
| `inv_metric` | `inv_metric` | Support single or per-chain values; expose final values. |
| no file form | `metric_file` | Add JSON/file parsing or explicitly reject until implemented. |
| `init_buffer`, `term_buffer`, `window` | same | Normalize `NULL` to bundled defaults. |
| `thin`, `save_warmup`, `adapt_engaged`, `refresh` | same | Preserve, with CmdStanR defaults. |
| no equivalent | `diagnostics` | Run divergence/treedepth/E-BFMI checks and warnings. |
| no equivalent | `save_metric` | Retain adapted metrics in the object; file persistence is optional later. |
| no equivalent | `save_latent_dynamics` | Requires retaining diagnostic-writer output. |

Newstan currently supports static HMC, step-size jitter, integration time, and
advanced adaptation constants that CmdStanR's `$sample()` does not expose.
Preserve them as clearly documented extensions appended after the CmdStanR
arguments, for example `engine = c("nuts", "static")`, `int_time`,
`step_size_jitter`, `adapt_gamma`, `adapt_kappa`, and `adapt_t0`. The primary
signature and documentation should still present NUTS/fixed-parameter use in
CmdStanR terms.

Sampling result requirements:

- `$draws(variables = NULL, inc_warmup = FALSE, format = ...)` defaults to a
  `draws_array`, selects base or indexed variable names, and excludes warmup by
  default even when warmup was saved.
- `$sampler_diagnostics(inc_warmup = FALSE, format = ...)` has the same
  iterations/chains as the requested draws.
- `$num_chains()` returns `chains`.
- `$return_codes()` has length `chains`, rather than one aggregate code.
- `$inv_metric(matrix = TRUE)` returns one final metric per chain.
- `$diagnostic_summary()`, `$lp()`, and optionally `$loo()` match CmdStanR's
  observable behavior closely.
- Fixed-parameter runs do not fabricate a one-row diagnostic object, which is
  the current behavior. Return a correctly dimensioned empty/NA diagnostic
  representation or the same documented behavior as CmdStanR.
- Partial chain failure leaves successful chains available and identifies
  failed chains in return codes and metadata.

### Optimization

Target name and result:

```r
fit <- model$optimize(...)  # StanMLE
```

The target signature should follow CmdStanR:

```r
optimize(data = NULL, seed = NULL, refresh = NULL, init = NULL,
         output_dir = getOption("newstan_output_dir"), output_basename = NULL,
         sig_figs = NULL, threads = NULL, opencl_ids = NULL,
         algorithm = NULL, jacobian = FALSE, init_alpha = NULL, iter = NULL,
         tol_obj = NULL, tol_rel_obj = NULL, tol_grad = NULL,
         tol_rel_grad = NULL, tol_param = NULL, history_size = NULL,
         show_messages = TRUE, show_exceptions = TRUE,
         save_cmdstan_config = getOption("newstan_save_config", FALSE))
```

Changes from current newstan:

- Add the currently missing `jacobian` switch and call the matching Stan
  service/template path.
- Rename `num_threads` to `threads` and `verbose` to message/exception controls.
- Remove public `id`; keep it internal unless there is a demonstrated use.
- Keep `save_iterations` as an optional newstan extension. If enabled, store
  the trace separately; `$draws()` and `$mle()` must still represent the final
  estimate.
- Return `lp__` once in `$draws()`/`$lp()`, not both inside the parameter vector
  and as a loosely named `value` field.
- `$mle(variables = NULL)` returns a named vector excluding `lp__`.
- `$summary()` returns only `variable` and `estimate` by default.

### Laplace approximation

Target name and result:

```r
fit <- model$laplace(...)  # StanLaplace
```

Key parity requirements:

- `mode = NULL` runs `$optimize(jacobian = jacobian, ...)` automatically.
- `opt_args` is a named list passed to that optimization and cannot be supplied
  together with `mode`.
- Accept `StanMLE` as the canonical mode object. Retaining a named numeric
  vector as a newstan extension is useful because this backend does not need a
  CmdStan mode CSV.
- `$mode()` returns the `StanMLE` used. If a numeric mode was supplied,
  create a lightweight valid MLE object or document a package-specific return
  type rather than changing shape based on input.
- Normalize native `log_p__` to `lp__` and `log_q__` to `lp_approx__`.
- Provide `$lp()` and `$lp_approx()`.
- Keep `calculate_lp` as a newstan extension if the underlying Stan service
  supports it; CmdStanR's primary signature does not expose it.

### Variational inference

Target call and result:

```r
fit <- model$variational(..., draws = NULL)  # StanVB
```

| Current argument | Target argument |
|---|---|
| `output_samples` | `draws` |
| `num_threads` | `threads` |
| `verbose` | `show_messages`, `show_exceptions` |
| explicit defaults | `NULL`, then bundled defaults |

Retain `algorithm`, `iter`, `grad_samples`, `elbo_samples`, `eta`,
`adapt_engaged`, `adapt_iter`, `tol_rel_obj`, and `eval_elbo` under the same
names. Add common output/config arguments with the support policy described
above.

The native writer produces a posterior mean row followed by `draws` random
draws. CmdStanR excludes the first row, drops the placeholder `lp__`, renames
`log_p__` to `lp__`, and renames `log_g__` to `lp_approx__`. Newstan currently
does none of these. Correcting this is required for numerical and row-count
parity, and enables `$lp()`/`$lp_approx()`.

### Pathfinder

Target terminology:

| Current argument | Target argument | Meaning |
|---|---|---|
| `num_draws` | `single_path_draws` | candidates returned by each path |
| `num_psis_draws` | `draws` | final PSIS-resampled draws |
| `num_paths` | `num_paths` | same; CmdStanR default 4 |
| `num_threads` | `threads` | use the current name only |

Other optimizer tolerances already have matching names. Adopt common
`data`/`seed`/`init`/output/message arguments and `NULL` defaults.

Do not move `lp__` and `lp_approx__` out of `$draws()`. CmdStanR includes them
and implements `$lp()` and `$lp_approx()` as extractors. `path__` may also stay
in the draw metadata/variables consistent with the native output. The current
separate `diagnostics` field is not a CmdStanR fit-object concept and should not
drive the new public contract.

`save_single_paths = TRUE` requires returning or persisting the per-path draws
and the structured L-BFGS/ELBO diagnostics currently discarded by
`run_pathfinder.hpp`. Merely forwarding the flag is not feature parity.

### Generated quantities

Rename the model method to singular `generate_quantities()` to match CmdStanR.
Remove the old top-level `generated_quantities()` API.

Requirements:

- Put `fitted_params` first in the method signature, followed by `data`.
- Accept newstan MCMC/MLE/Laplace/VB/Pathfinder fit objects and
  `posterior::draws_array`/`draws_matrix`. A plain numeric matrix can remain a
  documented extension.
- Split input draws by chain, run with deterministic per-chain RNG IDs, and
  preserve the input iteration/chain structure in the `draws_array` result.
- Add `parallel_chains` and `threads_per_chain` with the same distinction used
  by sampling.
- Validate that all required model parameters exist. Ignore extra transformed
  parameters/GQs deliberately and report missing dimensions/names before the
  native call.
- Provide `$num_chains()` and a meaningful replacement for CmdStanR's
  `$fitted_params_files()` (for example `$fitted_params()`); only add the file
  method if files are actually produced.

### Diagnose

Rename `gradient_check()` to the model method `$diagnose()`. The result must be
a `StanDiagnose`, not an integer.

The native diagnostic sample writer already receives Stan output but the R
layer drops it. Return and expose:

- `$gradients()`: a data frame containing finite-difference and autodiff
  gradient information;
- `$lp()`: the evaluated target;
- `$metadata()`, `$return_codes()`, `$init()`, `$output()`, and `$time()`; and
- the failed-check count as metadata or a convenience method such as
  `$num_failed()`.

Do not equate “one or more gradient discrepancies” with a process/configuration
failure without documenting it. CmdStanR's returned object separates the run
record from the gradient values.

### Canonical target signatures

To keep parallel implementation work consistent, use the following CmdStanR
parameter order as the baseline. Replace `cmdstanr_*` options with
`newstan_*` options, omit arguments that are already deprecated in CmdStanR,
and append documented newstan-only extensions after the aligned arguments. An
argument shown here is not permission to ignore it: implement it, or reject
non-default use explicitly until its milestone is complete.

```r
sample(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  save_latent_dynamics = FALSE,
  output_dir = getOption("newstan_output_dir"), output_basename = NULL,
  sig_figs = NULL,
  chains = 4, parallel_chains = getOption("mc.cores", 1),
  chain_ids = seq_len(chains), threads_per_chain = NULL, opencl_ids = NULL,
  iter_warmup = NULL, iter_sampling = NULL, save_warmup = FALSE, thin = NULL,
  max_treedepth = NULL, adapt_engaged = TRUE, adapt_delta = NULL,
  step_size = NULL, metric = NULL, metric_file = NULL, inv_metric = NULL,
  init_buffer = NULL, term_buffer = NULL, window = NULL,
  fixed_param = FALSE,
  show_messages = TRUE, show_exceptions = TRUE,
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  save_metric = getOption("newstan_save_metric", FALSE),
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  # newstan extensions, after the CmdStanR-aligned surface:
  engine = "nuts", int_time = 2 * pi, step_size_jitter = 0,
  adapt_gamma = 0.05, adapt_kappa = 0.75, adapt_t0 = 10
)

optimize(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  output_dir = getOption("newstan_output_dir"), output_basename = NULL,
  sig_figs = NULL, threads = NULL, opencl_ids = NULL,
  algorithm = NULL, jacobian = FALSE, init_alpha = NULL, iter = NULL,
  tol_obj = NULL, tol_rel_obj = NULL, tol_grad = NULL,
  tol_rel_grad = NULL, tol_param = NULL, history_size = NULL,
  show_messages = TRUE, show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  # optional newstan extension:
  save_iterations = FALSE
)

laplace(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  output_dir = getOption("newstan_output_dir"), output_basename = NULL,
  sig_figs = NULL, threads = NULL, opencl_ids = NULL,
  mode = NULL, opt_args = NULL, jacobian = TRUE, draws = NULL,
  show_messages = TRUE, show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE),
  # optional newstan extension:
  calculate_lp = TRUE
)

variational(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  save_latent_dynamics = FALSE,
  output_dir = getOption("newstan_output_dir"), output_basename = NULL,
  sig_figs = NULL, threads = NULL, opencl_ids = NULL,
  algorithm = NULL, iter = NULL, grad_samples = NULL, elbo_samples = NULL,
  eta = NULL, adapt_engaged = NULL, adapt_iter = NULL,
  tol_rel_obj = NULL, eval_elbo = NULL, draws = NULL,
  show_messages = TRUE, show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE)
)

pathfinder(
  data = NULL, seed = NULL, refresh = NULL, init = NULL,
  output_dir = getOption("newstan_output_dir"), output_basename = NULL,
  sig_figs = NULL, threads = NULL, opencl_ids = NULL,
  init_alpha = NULL, tol_obj = NULL, tol_rel_obj = NULL,
  tol_grad = NULL, tol_rel_grad = NULL, tol_param = NULL,
  history_size = NULL, single_path_draws = NULL, draws = NULL,
  num_paths = 4, max_lbfgs_iters = NULL, num_elbo_draws = NULL,
  save_single_paths = NULL, psis_resample = NULL, calculate_lp = NULL,
  show_messages = TRUE, show_exceptions = TRUE,
  save_cmdstan_config = getOption("newstan_save_config", FALSE)
)

generate_quantities(
  fitted_params, data = NULL, seed = NULL,
  output_dir = getOption("newstan_output_dir"), output_basename = NULL,
  sig_figs = NULL, parallel_chains = getOption("mc.cores", 1),
  threads_per_chain = NULL, opencl_ids = NULL,
  show_messages = TRUE, show_exceptions = TRUE
)

diagnose(
  data = NULL, seed = NULL, init = NULL,
  output_dir = getOption("newstan_output_dir"), output_basename = NULL,
  epsilon = NULL, error = NULL
)
```

CmdStanR does not expose `show_messages`, `show_exceptions`, or threading on
`diagnose()`, so neither should the aligned signature. Newstan may retain
all messages internally and make them available through the result object.
For all methods, use exact-name matching internally; do not rely on R's partial
argument matching when extension arguments are present.

## Fit method parity matrix

The following matrix defines the desired caller-facing result API. “Equivalent”
means implement in R over in-memory data; “native” means the native service
must return more information; “backend-specific” means do not copy the CmdStan
name unless a genuine newstan artifact is produced.

| Fit method | Applies to | Priority/source |
|---|---|---|
| `$draws(variables, inc_warmup, format)` | all except Diagnose | P0, equivalent after output normalization |
| `$summary(variables, ...)` | all draw fits | P0, equivalent |
| `$print(variables, ..., digits, max_rows)` | all draw fits | P0, equivalent |
| `$return_codes()` | all | P0; native per-chain work for MCMC/GQ |
| `$metadata()` | all | P0; define native-independent schema |
| `$time()` | all | P1, native instrumentation |
| `$output(id = NULL)` | all | P1, retained logger output |
| `$init()` | all initialized services | P1, retain normalized user input/native init output |
| `$code()` | all | P1, retain model descriptor |
| `$lp()` | MCMC, MLE, Laplace, VB, Pathfinder, Diagnose | P0/P1 depending on output normalization |
| `$lp_approx()` | Laplace, VB, Pathfinder | P0, equivalent after column renaming |
| `$sampler_diagnostics()` | MCMC | P0, split phase and format correctly |
| `$diagnostic_summary()` | MCMC | P1, equivalent R calculations |
| `$inv_metric()` | MCMC | P1, native metric writer |
| `$num_chains()` | MCMC, GQ | P0 |
| `$mle()` | MLE | P0 |
| `$mode()` | Laplace | P0 |
| `$gradients()` | Diagnose | P0, native output |
| `$loo()` | MCMC | P2, wrapper around optional `loo` package |
| `$profiles()` | fits whose model used `profile()` | P2, native model/profile support |
| `$materialize()` | all draw fits | P1 no-op, for familiar workflows |
| `$save_object()` | all draw fits | P1, serialization-safe implementation |
| `$as_draws()` S3 methods | all draw fits | P1, forward to `$draws()` |
| `$init_model_methods()` | draw fits | P0, ensure/rebuild data-bound native state |
| `$log_prob()`/`$grad_log_prob()`/`$hessian()` | draw fits | P0, native model bridge |
| `$constrain_variables()`/`$unconstrain_variables()` | draw fits | P0, native model bridge |
| `$unconstrain_draws()`/`$variable_skeleton()` | draw fits | P0, native bridge plus R shape handling |
| output/data/config/metric file methods | applicable fits | P2, only if newstan writes those artifacts |
| `cmdstan_summary()`/`cmdstan_diagnose()` | CmdStan-only tools | omit; offer R-native summaries instead |

### Draw formats and variable names

This is a cross-cutting compatibility requirement:

- MCMC and GQ should default to `draws_array`; MLE, Laplace, VB, and Pathfinder
  should default to `draws_matrix`, subject to a package option analogous to
  `cmdstanr_draws_format` (use a newstan-prefixed option).
- Accept posterior's standard formats and use `posterior::as_draws_*()` for
  conversion. Do not permanently mutate the stored canonical draws merely
  because one caller requested another format.
- Convert native names `beta.1.1` to `beta[1,1]`. Implement the inverse mapping
  for native calls. Match both base variables (`beta`) and exact indexed
  variables (`beta[2]`) in selectors.
- Preserve `.iteration`, `.chain`, and `.draw` semantics. Arbitrary
  `chain_ids` belong in metadata even if posterior uses sequential chain
  positions internally.
- Keep model variables, sampler diagnostics, and posterior bookkeeping columns
  distinct.
- Summaries for non-MCMC fits should use default summary measures without
  MCMC-specific R-hat/ESS measures, matching CmdStanR. MLE summaries should be
  `variable`/`estimate`.

## Core model evaluation and parameter transformation methods

These methods are required on every data-bound `StanFit` subclass (apart from
`StanDiagnose` if it remains a deliberately smaller class):

```r
init_model_methods(seed = 1, verbose = FALSE)
log_prob(unconstrained_variables, jacobian = TRUE)
grad_log_prob(unconstrained_variables, jacobian = TRUE)
hessian(unconstrained_variables, jacobian = TRUE)
unconstrain_variables(variables)
unconstrain_draws(draws = NULL,
                  format = getOption("newstan_draws_format", "draws_array"),
                  inc_warmup = FALSE)
variable_skeleton(transformed_parameters = TRUE,
                  generated_quantities = TRUE)
constrain_variables(unconstrained_variables,
                    transformed_parameters = TRUE,
                    generated_quantities = TRUE)
```

CmdStanR compiles these bindings separately because a CmdStan executable does
not expose an in-process model object. Newstan should not repeat that design:
the concrete generated `stan_model` type is already in every sourceCpp
translation unit, and `inst/stan_model.cpp` is already appended during
compilation. Compile the model-method Rcpp exports at the same time as
`new_model()` and `run_model()`.

### Ownership and initialization

The methods operate on a model instantiated with the same data as the fit.
This leads to the following ownership requirements:

1. Each service call constructs its own `XPtr<stan_model>` from normalized
   data and the resolved seed. The returned fit retains that pointer rather
   than allowing the local `model_instance` wrapper to be garbage-collected.
2. Each fit retains a serialization-safe copy of the normalized data and a
   reference to the model compilation descriptor/environment. Fits from the
   same `StanModel` but different data must never share a model pointer.
3. Each fit owns a separate `XPtr<stan::rng_t>` used by
   `$constrain_variables()` when transformed parameters or generated
   quantities invoke RNG functions. `init_model_methods(seed)` initializes or
   resets this RNG. Repeated calls with generated quantities advance this RNG
   normally.
4. `$init_model_methods()` is an inexpensive “ensure live bindings and model
   pointer” operation in the normal case. It does not trigger a second model
   compilation. Every other model method calls it automatically.
5. After `saveRDS()`/`readRDS()` in another session, sourceCpp native symbols
   and external pointers are stale. `$init_model_methods()` recompiles or
   reloads the model from its stored compilation descriptor, reconstructs the
   data-bound pointer, and creates a new RNG. Draw and summary methods remain
   usable without this rebuild.

Do not keep a data-bound model pointer on `StanModel` itself. The same model
object can produce simultaneous fits with different data, and mutable shared
pointer state would cause calls such as `$log_prob()` to evaluate the wrong
posterior.

### Native callable surface

Add a common header such as `src/include/model_methods.hpp`; the existing
`src/Makevars` packaging rule will install it under
`inst/include/newstan/model_methods.hpp`. Keep thin `// [[Rcpp::export]]`
wrappers in `inst/stan_model.cpp`, where the concrete generated model type and
Rcpp attributes are available.

The compiled environment needs bindings equivalent to the following. Names
may differ internally, but the inputs and outputs should not:

| Native binding | Input | Output/contract |
|---|---|---|
| `new_base_rng(seed)` | unsigned seed | owning `XPtr<stan::rng_t>` |
| `model_num_upars(model)` | model XPtr | scalar `model.num_params_r()` |
| `model_param_metadata(model)` | model XPtr | ordered names, dimensions, and stage (parameter, transformed parameter, GQ) |
| `model_constrained_names(model, tparams, gqs)` | model XPtr and flags | flattened Stan names |
| `model_unconstrained_names(model)` | model XPtr | one name per unconstrained scalar |
| `model_log_prob(model, upars, jacobian)` | numeric vector | scalar target, proportional form |
| `model_grad_log_prob(model, upars, jacobian)` | numeric vector | gradient with `log_prob` attribute |
| `model_hessian(model, upars, jacobian)` | numeric vector | list of `log_prob`, `grad_log_prob`, and Hessian matrix |
| `model_unconstrain(model, variables)` | named constrained parameter list/context | unconstrained numeric vector |
| `model_unconstrain_matrix(model, values)` | rows of constrained parameter scalars in declared order | numeric matrix, one unconstrained row per input row |
| `model_constrain(model, rng, upars, tparams, gqs)` | model/RNG XPtrs, vector, flags | flat constrained values plus ordered names/metadata |
| `model_compile_info(model)` | model XPtr | stanc version and flags from `model_compile_info()` |

The model compilation cache key currently hashes generated C++ plus
`inst/stan_model.cpp`. Once that file includes `model_methods.hpp`, also hash
the installed header contents (or a single explicit native bridge ABI/version
constant), the runner ABI, and the bundled Stan version. Otherwise a package
update can reuse a sourceCpp artifact whose exported bindings or model-method
semantics are stale even though the include line in `stan_model.cpp` did not
change.

The common C++ implementation can accept `stan::model::model_base&`. Its
non-virtual convenience template dispatches to the appropriate virtual
`log_prob`, `log_prob_jacobian`, `log_prob_propto`, or
`log_prob_propto_jacobian` overload. The parameter transform and name methods
are also virtual on `model_base`. There is therefore no need to duplicate the
method bodies in every generated translation unit.

Expected includes are the bundled-version equivalents of:

```cpp
#include <stan/model/model_base.hpp>
#include <stan/model/log_prob_grad.hpp>
#include <stan/math/rev.hpp>
#include <stan/math/mix.hpp>  // or the direct finite_diff_hessian_auto header
#include <stan/services/util/create_rng.hpp>
#include <newstan/r_data_context.hpp>
```

### Method-specific C++ behavior

#### `log_prob()`

- Match CmdStanR's target: drop additive constants (`propto = true`) and use
  `jacobian` to select whether inverse-transform Jacobian adjustments are
  included. The public default is `jacobian = TRUE`.
- Validate vector length against `num_params_r()` in R and again defensively
  in C++. Reject NA, NaN, and infinite inputs before evaluating the model.
- Pass a C++-owned stream for Stan `print()` output. Return or append captured
  messages to the fit's output buffer; never call R console APIs from a worker
  thread.

#### `grad_log_prob()`

- Call `stan::model::log_prob_grad<true, true/false>()` with an Eigen vector.
- Return a numeric gradient of length `num_params_r()` and store the evaluated
  target in its `"log_prob"` attribute, matching CmdStanR.
- `log_prob_grad` recovers Stan reverse-mode memory on success and exception.
  Preserve that guarantee if the implementation is wrapped or changed.

#### `hessian()`

- Return exactly `list(log_prob, grad_log_prob, hessian)`.
- Match CmdStanR by evaluating the proportional target and respecting the
  `jacobian` flag. `stan::math::internal::finite_diff_hessian_auto()` with a
  lambda that branches at runtime between
  `model_base::log_prob<true, true>()` and
  `model_base::log_prob<true, false>()` is the current CmdStanR approach and is
  already used by bundled Stan's Laplace service.
- Verify Hessian orientation and symmetry in tests for a multivariate model,
  not only the current one-parameter Bernoulli example.
- Ensure autodiff memory is recovered if evaluation throws. Do not cache AD
  variables or a Hessian functor across calls.

#### `unconstrain_variables()`

- Accept a named list of constrained *parameters*. Ignore extra transformed
  parameters and generated quantities, but error before entering C++ if any
  non-zero-size parameter is absent.
- Prefer `model.transform_inits(r_data_context, upars, message_stream)` over
  flattening arbitrary R lists in guessed order. This delegates constraints,
  ordering, and dimension validation to the generated model.
- Upgrade `r_data_context` so unsupported input types produce errors rather
  than being silently omitted. Its representation must cover every parameter
  type promised by the R API, including zero-length containers. Complex and
  tuple parameters require either explicit support or an explicit documented
  error until their R representation is designed.
- Return unconstrained names separately through
  `model_unconstrained_names()`; names are needed by batch transforms and
  posterior draw objects.

#### `unconstrain_draws()`

- With `draws = NULL`, use the fit's draws and honor `inc_warmup`. With a
  supplied posterior draws object, `inc_warmup` is not meaningful and should
  error rather than be silently ignored.
- Select only declared parameters, reorder flattened columns to
  `constrained_param_names(include_tparams = false, include_gqs = false)`, and
  convert repaired R names (`x[1,2]`) back to Stan's dotted native names for
  matching.
- Transform rows in C++ with `model.unconstrain_array()`. Preallocate one
  output matrix after using the first row to determine width; validate that
  every output has the same width.
- Restore the original iteration/chain structure and return the requested
  posterior format. Name output columns with repaired unconstrained names.
- A `files` argument should be added only if newstan also implements a defined
  CSV input contract; it is not needed for the in-memory core API.

#### `constrain_variables()`

- Validate unconstrained width, then call `model.write_array()` with the fit's
  dedicated `stan::rng_t`.
- The flags control which values are returned, not whether dependencies are
  computed internally: generated quantities may depend on transformed
  parameters even when transformed parameters are excluded from the result.
- Obtain flattened output names from `constrained_param_names()` with the same
  flags. Return a named structured list, not an unnamed flat vector.
- Propagate model rejects and capture Stan print output. Generated quantities
  are stochastic when they contain RNG calls; tests must cover reproducibility
  after resetting the method RNG with `init_model_methods(seed)`.

#### `variable_skeleton()` and parameter metadata

- Build parameter metadata once per live model pointer by calling
  `get_param_names()` and `get_dims()` for parameters only, then with
  transformed parameters, then with generated quantities. The ordered tails
  identify the declaration stage without parsing flattened names.
- A scalar skeleton is `array(0, dim = 1)`; containers use their declared
  dimensions. Filter stages according to the two public flags.
- The `model_base` documentation notes that `get_dims()` assumes rectangular
  values and is not sufficient for tuple/ragged representations. Cross-check
  it against stanc `info` metadata and add dedicated tuple tests before
  claiming full Stan 2.39 type coverage.
- The same metadata should drive constrained-list validation, variable
  selection, GQ input processing, and `$variables()`; do not maintain four
  independent notions of parameter order.

### Threading, exceptions, and native lifetime

- Model-method calls are synchronous and run on the R thread. They must not be
  invoked concurrently on the same model pointer or RNG. Document fit objects
  as non-thread-safe mutable references.
- The methods must not reuse the sampling worker's AD stack or retain pointers
  into R vectors. Convert inputs before any native parallel work and construct
  R outputs only on the main thread.
- Translate C++ exceptions to R errors only after AD memory and temporary
  state have been cleaned up. Include the method name and model name in errors
  without discarding Stan's original message.
- Keep the sourceCpp environment (and thus its loaded DLL/native symbols)
  reachable from every live fit. An XPtr without the environment/rebuild
  descriptor is not a sufficient lifetime strategy.
- Never serialize XPtrs as if they were reusable. Mark them stale after reload
  and reconstruct them from stored code, compile options, data, and seed.
- Version the native binding contract and reject/rebuild a pointer compiled
  against a different package/Stan bridge version rather than invoking a
  mismatched symbol.

### Required R implementation

Put public methods on `StanFit` so MCMC, MLE, Laplace, VB, Pathfinder, and
GQ inherit identical behavior. Suggested R files are `R/model-methods.R` for
public methods/validation and `R/fit.R` for pointer ownership and rebuilding.
Keep native functions private.

Every public method should:

1. call `private$ensure_model_methods(seed = NULL)`;
2. validate and normalize its R input using shared parameter metadata;
3. call one narrow native binding;
4. normalize names/shapes/messages; and
5. return the CmdStanR-compatible value without exposing an external pointer.

The fit constructor must receive the exact model pointer used by the service,
not instantiate another model opportunistically. Reinstantiation is reserved
for stale-pointer recovery after serialization.

## Missing functionality and behavioral gaps

### P0: required for a credible R6/API migration

1. **R6 dependency and class hierarchy.** Add `R6` to `Imports`, define the
   model/fit generators, update `NAMESPACE`, and replace direct list field
   access in examples/tests.
2. **Canonical argument normalization.** One internal layer should validate
   common arguments, resolve bundled defaults, generate seeds, normalize
   inits, and produce the native argument list. Avoid seven independent seed
   and thread implementations.
3. **Correct warmup handling.** The fit must store warmup separately and honor
   `inc_warmup`. Current `save_warmup = TRUE` changes the default `draws`
   content, unlike CmdStanR.
4. **Correct approximate-inference columns.** Fix ADVI's mean row and column
   names; normalize Laplace names; keep Pathfinder density columns accessible.
5. **Variable name repair and draw formats.** Current dotted names are visibly
   incompatible with CmdStanR and common posterior tooling.
6. **Chain-aware initialization and GQ.** Support a list/function per chain or
   Pathfinder path, preserve chain structure, and use supplied chain IDs.
7. **Result methods and failure states.** Fit objects must exist on failure and
   provide informative errors only when a missing payload is requested.
8. **Data input validation.** Support `data = NULL`; reject unnamed, duplicate,
   unsupported, non-finite, or missing values consistently; validate required
   variables and shapes. `r_data_context` currently ignores unsupported R
   types instead of always rejecting them. Decide explicitly whether logicals,
   factors, data frames, and lists are converted as CmdStanR does.

### P1: consistency users will expect soon after migration

1. **Adapted inverse metrics and step sizes.** Parse/return the metric writers
   already allocated in sampling. Store them per chain.
3. **Initialization retrieval.** Retain normalized user-specified inits and, if
   desired, parse the init writers for actual initialized values as a separate
   concept.
4. **Timing.** Record wall-clock total time for every service and warmup,
   sampling, and total by chain for MCMC. Retaining service writer comments may
   provide phase timing; otherwise instrument the coordinator.
5. **Message and exception capture.** Change `r_logger` from print-only to a
   drainable result buffer with severity and, where possible, chain/path IDs.
   `show_messages` and `show_exceptions` control display, not retention.
6. **Diagnostic summary.** Implement divergence count, max-treedepth count, and
   E-BFMI checks over the canonical diagnostics. Run selected checks after
   sampling and issue CmdStanR-like warnings.
7. **Model information.** Store code/file/name/include paths, add structured
   `$variables()`, and expose bundled Stan defaults/version.
8. **Serialization.** Confirm a materialized fit survives `saveRDS()` and a new
   session. Recompile lazily and reconstruct the data-bound model pointer and
   model-method RNG when a native evaluation method is requested.

### P2: broader feature parity

1. Functions-block exposure. Direct linkage makes this potentially simpler
   than CmdStanR's auxiliary compilation, but it is distinct from the required
   model evaluation/transform bridge.
2. Persisted output/config/data/metric/latent-dynamics files and their accessor
   methods. Define a newstan file format or intentionally emit CmdStan-compatible
   CSV/JSON before using CmdStanR method names.
3. Model profiling output.
4. `loo()` with optional `loo`, including moment matching only after the model
   log-probability/transform methods exist.
5. OpenCL, including compiler defines, includes, link libraries, device
   selection, validation, and tests. The stanc flag is insufficient.
6. Standalone Stan functions and custom user header behavior matching the
   useful parts of CmdStanR.

### Intentionally out of scope unless separately requested

- CmdStan installation, toolchain, and path management.
- Attaching an existing CmdStan executable with `exe_file`.
- Reconstructing fit objects from arbitrary CmdStan CSV through
  `as_cmdstan_fit()`.
- Calling CmdStan's `stansummary` and `diagnose` binaries.
- MPI process launching. Newstan can retain threaded in-process parallelism;
  an MPI backend would be a separate architectural project.
- Exact CmdStan file lifecycle as a prerequisite for the in-memory API.

## Native result contract required by the R6 layer

Before implementing all fit constructors, define one internal run-result
schema. A suggested shape is:

```r
list(
  method = "sample",
  draws = list(post_warmup = ..., warmup = ...),
  sampler_diagnostics = list(post_warmup = ..., warmup = ...),
  return_codes = integer(),
  chain_ids = integer(),
  init = list(user = ..., actual = ...),
  metric = list(),
  step_size = numeric(),
  timing = list(total = ..., chains = ...),
  messages = data.frame(run_id, severity, text),
  profiles = list(),
  structured = list(),
  metadata = list()
)
```

Non-sampling services use the same top-level keys and leave non-applicable
entries empty. This keeps R6 construction and failure handling uniform.

Native files likely affected:

- `src/include/model_methods.hpp` (new): shared `model_base` implementations
  for log probability, derivatives, metadata, and parameter transforms.
- `inst/stan_model.cpp`: Rcpp exports for the model-method bridge, creation of
  the dedicated method RNG, and non-throwing parameter-name/metadata access.
- `src/include/r_output.hpp`: phase-aware sample writer, retained comments,
  conversion helpers, and possibly structured-writer parsing.
- `src/include/r_logger.hpp`: retain/drain messages and support separate
  display controls.
- `src/include/stack_writer_chains.hpp`: chain IDs, phase-safe assembly, and
  partial-chain handling.
- `src/include/run_sampling.hpp`: scheduling, per-chain results, inits,
  metrics, diagnostics, and timing.
- `src/include/run_advi.hpp`: expose enough metadata to remove the mean row
  reliably and preserve diagnostics if requested.
- `src/include/run_diagnose.hpp`: return gradient and lp output.
- `src/include/run_pathfinder.hpp`: return structured/per-path output when
  requested.
- `src/include/run_laplace.hpp`: return/label density columns and optionally
  Hessian output.
- `src/include/run_standalone_gqs.hpp`: chain-aware invocation is likely best
  coordinated in R/C++ above the single service call.
- `src/runner.cpp` and `inst/stan_model.cpp`: standardized result and expanded
  model-information/evaluation bindings.

`inst/stan_model.cpp` currently exports only `new_model()`, `run_model()`, and
`constrained_param_names()`. Its current constrained-name helper also errors
for parameterless models. Replace that narrow helper with the complete,
flag-aware metadata/name surface described above; parameterless models should
return empty metadata rather than error.

Do not have native worker threads allocate R objects or call R APIs. Preserve
the current safe pattern: copy R inputs before workers start, collect C++
state, then construct R results on the main R thread.

## Implementation status

Status as of 2026-07-31, after the first implementation pass:

| Phase | Status | Verified in this pass | Still required |
|---|---|---|---|
| 1. Aligned contracts | Complete | Canonical constructor/service/model-method signatures, `Stan*` inheritance, unsupported-file argument errors, replacement-API tests, shared normalization/defaults layer (`R/normalize.R`), and native run-result schema definition | None |
| 2. Model R6 and adapters | Initial adapter complete | `StanModel`, `StanFit`, all service-specific subclasses, lazy compilation, retained source metadata, fit accessors, internal service adapters, and removal of procedural/S3 exports | Replace remaining adapter-only limitations as richer native results become available; implement real declaration data for `$variables()` |
| 3. Model methods | Partial | Native bridge; log density, gradient, Hessian, metadata, constrain/unconstrain, batched draw transforms, fit-local RNG, data isolation, validation, and same-session serialization rebuilding | Retain the exact service pointer, fresh-session lifecycle coverage, tuple/ragged representations, and richer captured-message handling |
| 4. Output correctness | Partial | Bracketed variable names, warmup/main draw and diagnostic separation, ADVI mean-row removal/name normalization, and initial Laplace/Pathfinder normalization | Complete every fit format/column contract, chain-preserving GQ, advanced fit extractors, and CmdStanR cross-contract checks |
| 5. Native run metadata | Not started | Aggregate status and elapsed wall time are exposed through R6 | Per-chain status, partial failures, inits, metrics, retained output, detailed timing, and full diagnostics |
| 6. Advanced features | Not started | None claimed | Profiling, exposed Stan functions, artifacts, LOO, and OpenCL |

The initial verified test coverage comprises 13 focused R6/model-method tests
with 171 expectations plus the migrated package suite. It covers analytical
multivariate log probability/gradient/Hessian values, Jacobian switching,
constrain/unconstrain round trips, method RNG reset and advancement, invalid
inputs, independent data-bound fits, batched draw transforms, warmup and chain
shape preservation, and lazy rebuilding after `saveRDS()`/`readRDS()`. The
native bridge was also compiled independently against ordinary,
empty-parameter, and complex-parameter models. These results do not mark the
fresh-R-session or tuple/ragged cases complete. The integrated package passed
`R CMD check --no-manual` with zero errors, warnings, or notes.

## Recommended implementation sequence

### Phase 1: define aligned contracts — complete

1. **Complete:** Add tests that encode target method signatures, class inheritance, default
   classes/formats, variable names, and failure behavior.
2. **Complete:** Define bundled defaults and the normalized internal argument schema.
   Implemented in `R/normalize.R` with `.newstan_defaults` and per-service normalization
   functions (`.newstan_normalize_sample`, `.newstan_normalize_optimize`, etc.).
3. **Complete:** Define the native run-result schema above.
   Implemented as `.newstan_run_result_schema()` in `R/normalize.R`.
4. **Complete for the current surface:** Decide the exact treatment of file-oriented arguments and document every
   accepted-but-unsupported argument as an error, not a no-op.

Acceptance criteria: tests can instantiate mock model/fit objects and verify
the public surface without compiling Stan models.

### Phase 2: model R6 and thin service adapters — initial adapter complete

1. **Complete:** Add `R6` and implement `StanModel` around the existing compiled
   environment.
2. **Complete:** Retain source and compilation metadata.
3. **Complete:** Move current service bodies behind model methods with CmdStanR argument
   names.
4. **Complete:** Initially construct R6 fit objects from the existing payloads, explicitly
   marking unavailable metadata.
5. **Complete:** Remove the procedural service exports and their S3 result methods once the
   corresponding R6 method and fit class exist.

Acceptance criteria: the basic workflow is:

```r
mod <- stan_model(code = model_code)
fit <- mod$sample(data = data, chains = 2, iter_warmup = 100,
                  iter_sampling = 100)
fit$draws()
fit$summary()
fit$return_codes()
```

No user accesses the sourceCpp environment or a raw result list.

### Phase 3: model evaluation and parameter transformations — partial

1. **Complete:** Add `src/include/model_methods.hpp` and the Rcpp exports in
   `inst/stan_model.cpp`.
2. **Partial:** Make fit constructors retain the service's data-bound model pointer,
   normalized data, compilation descriptor, and dedicated method RNG.
   Fits currently reconstruct an equivalent pointer from retained data instead
   of retaining the exact pointer used by the service.
3. **Partial:** Implement parameter metadata, dotted/bracketed name conversion, and
   `variable_skeleton()` first; use them to validate every other model method.
   Model-method metadata is implemented, but `StanModel$variables()` still
   returns a structured placeholder.
4. **Numerical implementation complete; message integration partial:**
   Implement `log_prob()`, `grad_log_prob()`, and `hessian()` with AD cleanup
   and captured model messages. The native layer captures messages, but they
   are not yet consistently retained in the fit-level output contract.
5. **Complete for supported types:** Implement constrained/unconstrained single-value transforms and batched
   `unconstrain_draws()` with posterior shape restoration.
   Tuple/list values are explicitly rejected pending a representation design.
6. **Partial:** Implement stale-native-symbol detection and lazy model/pointer rebuilding
   after serialization.
   Same-session RDS rebuilding is tested; a true fresh-session test remains.

Acceptance criteria: every draw fit evaluates the same data-bound target used
for fitting; two fits from the same model with different data return different
correct targets; all derivative/transform contracts match CmdStanR; saved and
reloaded fits rebuild native state automatically.

### Phase 4: output correctness — partial

1. Apply the shared variable-name repair to every service result and implement
   the complete posterior format conversion surface.
2. Split warmup/main draws and diagnostics.
3. Normalize MLE, ADVI, Laplace, and Pathfinder columns/rows.
4. Preserve GQ chains.
5. Add `lp`, `lp_approx`, `mle`, `mode`, `num_chains`, and Diagnose gradient
   methods.

Acceptance criteria: equivalent fixed-seed newstan and CmdStanR runs have the
same variable names, expected row/chain dimensions, and result method shapes.
Numerical draws need not be bit-identical across backends, but deterministic
services and summary invariants should agree within appropriate tolerances.

### Phase 5: native run metadata — not started

1. Add per-chain statuses and partial failure handling.
2. Return init, metric, step size, timing, and retained output.
3. Add MCMC diagnostics and selected warnings.
4. Include the added run metadata in the already established serialization
   contract.

Acceptance criteria: all P0/P1 rows in the fit parity matrix work without
placeholder values, except explicitly backend-specific file methods.

### Phase 6: advanced model and artifact features — not started

Implement profiling, standalone functions, optional output artifacts, LOO, and
OpenCL as independent workstreams with their own tests. Model evaluation and
parameter transforms are prerequisites for core parity and are not deferred to
this phase.

## Test plan

### API and validation tests

- Snapshot `formals()` for every public factory/model method.
- Assert R6 inheritance and method presence for every result class.
- Test `NULL`, random, scalar, per-chain list, function, and file forms for
  data/init/metric inputs as supported.
- Reject unknown arguments rather than swallowing them through `...`.
- Mirror relevant `cmdstanr/tests/testthat/test-model-*.R` validation cases,
  adapting only backend-specific assertions.

### Result-shape tests

- MCMC with one/multiple chains, zero warmup where allowed, saved warmup,
  thinning, fixed parameter, each metric, custom per-chain metrics, and partial
  chain failure.
- Verify `draws(inc_warmup = FALSE/TRUE)` and
  `sampler_diagnostics(inc_warmup = FALSE/TRUE)` dimensions and chain IDs.
- Verify scalar/vector/matrix/array variables use bracketed names and base-name
  selection.
- MLE with each algorithm, `jacobian` both ways, and optional saved iterations.
- ADVI meanfield/fullrank: exactly `draws` rows, no mean row, and canonical
  `lp__`/`lp_approx__`.
- Pathfinder single/multiple paths, PSIS on/off, calculate-lp on/off, and saved
  path products.
- Laplace with implicit optimization, MLE mode, numeric mode extension,
  matching Jacobian settings, and canonical density names.
- GQ from every supported fit/draw form, preserving multiple chains.
- Diagnose exposes gradients, lp, failed count, and run status.

### Cross-package contract tests

Use small models present in both repositories (Bernoulli, logistic, and
schools-like models). For each backend compare:

- public argument acceptance;
- class-specific method names;
- variable names/order;
- `posterior` format, iteration, and chain dimensions;
- metadata keys and return-code shape;
- summary column names; and
- deterministic transformed/generated quantities where fixed inputs make a
  direct numeric comparison appropriate.

Do not make tests depend on CmdStan file paths, process output text, or exact
RNG equality when direct Stan services and CmdStan use different scheduling.

### Serialization and lifecycle tests

- Save/read every fit class and call draw/summary/metadata methods.
- Save/read a model and verify `$compile()` can rebuild bindings.
- Restart-session tests should verify native evaluation methods work after
  lazy rebuilding. If rebuilding is impossible because the toolchain or source
  is unavailable, the error must identify the missing prerequisite.
- Confirm temporary compilation files disappearing does not invalidate
  materialized fit objects.

### Model-method tests

- Match fixed expected values for `log_prob()`, `grad_log_prob()`, and
  `hessian()` with `jacobian = TRUE/FALSE`.
- Test a multivariate Hessian for dimensions, orientation, symmetry, and
  finite-difference agreement.
- Verify incorrect unconstrained widths, non-finite values, missing constrained
  parameters, wrong dimensions, and extra non-parameter values.
- Round-trip every supported constrained type with
  `constrain_variables(unconstrain_variables(x))`, including scalars, vectors,
  row vectors, matrices, arrays, bounded values, simplexes, ordered types,
  covariance/correlation types, and zero-length containers.
- Explicitly test or explicitly reject complex and tuple parameters.
- Verify transformed-parameter and generated-quantity inclusion flags and the
  returned nested dimensions.
- Verify generated-quantity RNG reproducibility after
  `init_model_methods(seed)` and advancement over repeated calls.
- Compare batched `unconstrain_draws()` with per-row
  `unconstrain_variables()`, preserving chains, iterations, warmup, and the
  requested posterior format.
- Create two fits from one model with different data and show that model
  methods use the correct data independently.
- Save/read a fit in a fresh R session, call every model method, and verify the
  native binding and data-bound pointer are rebuilt without changing results.

## API replacement policy

Newstan is under active development and provides no backwards-compatibility
guarantee. Implement the target API directly:

- `stan_model()` returns `StanModel` and accepts only the new constructor
  argument names.
- Remove the exported procedural service functions `sampling()`,
  `optimizing()`, `laplace()`, `variational()`, `pathfinder()`,
  `generated_quantities()`, and `gradient_check()` when their model methods are
  introduced.
- Remove the existing `StanSample`, `StanOptimize`, `StanLaplace`,
  `StanVariational`, `StanPathfinder`, `StanGeneratedQuantities`, and
  `StanDiagnose` S3 result contracts and summary methods.
- Do not add aliases for old arguments such as `num_chains`, `num_warmup`,
  `num_samples`, `max_depth`, `stepsize`, `delta`, `num_threads`, `verbose`,
  `file`, or `include_directories`.
- Do not add wrapper fields for `fit$draws`, `fit$diagnostics`,
  `fit$return_code`, or `fit$par`; the R6 methods are the only contract.
- Rewrite tests, README examples, documentation, exports, and NEWS for the new
  API in the same change. There is no warning or deprecation period.

This also means implementation should not become more complicated solely to
distinguish whether a value came from an old or new argument name. All
normalization starts from one canonical signature.

## Decisions that should be made before agents implement in parallel

1. Confirm which CmdStanR file/output arguments have a useful in-process
   meaning in the first milestone; omit them or reject non-default use until
   their behavior is implemented.
2. Confirm whether `chains` should immediately default to 4. It should for API
   parity, but this increases test/runtime cost and changes current behavior.
3. Choose the chain execution design needed for true per-chain return codes and
   independent inits while preserving safe TBB within-chain threading.
4. Define whether a numeric Laplace mode and static HMC remain public newstan
   extensions. This guide recommends retaining both.
5. Define the stable metadata schema independently of CmdStan CSV comments.
6. Decide whether normalized user inits or actual initialized unconstrained
   values are returned by `$init()`; CmdStanR returns only user-specified
   values. If both are useful, expose them separately.
7. Decide the supported R representation and metadata source for complex,
   tuple, zero-size, and eventually ragged parameter types used by model
   transforms.
8. Confirm that each fit owns an independent method RNG and that
   `$init_model_methods(seed)` resets it, as recommended here.

Once those decisions are fixed, work can be divided safely among model R6,
fit R6, the native model-method bridge, argument normalization, native sampling
output, approximate-inference normalization, diagnostics, and tests without
each workstream inventing a different public contract.

## Definition of core parity

Core parity is complete when a user familiar with CmdStanR can write the
following against newstan with only the factory/package name differing:

```r
mod <- stan_model(stan_file)
fit <- mod$sample(
  data = data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  adapt_delta = 0.9
)

fit$draws("theta")
fit$sampler_diagnostics()
fit$summary()
fit$diagnostic_summary()
fit$return_codes()
fit$time()

upars <- fit$unconstrain_variables(list(theta = 0.5))
fit$log_prob(upars)
fit$grad_log_prob(upars)
fit$hessian(upars)
fit$constrain_variables(upars)
fit$unconstrain_draws()
```

The same principle must hold for optimization, Laplace, ADVI, Pathfinder,
standalone generated quantities, and diagnosis. Methods that are inherently
about CmdStan processes or files may be absent, but the absence must be
intentional, documented, and not leak into the design of in-memory results.
Core parity also requires the model evaluation and parameter transformation
methods to use the exact data associated with each fit and to survive
serialization through automatic native-state rebuilding.
