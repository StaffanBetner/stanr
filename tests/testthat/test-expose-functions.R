# Fixtures under test-models/expose-fixture-*.cpp are byte-for-byte output
# of stanc(code, standalone_functions = TRUE) against the bundled stanc.js,
# captured once rather than regenerated per test run (still compiler-free:
# stanc only needs the bundled JS engine).
read_fixture <- function(name) {
  paste(readLines(test_path("test-models", name), warn = FALSE), collapse = "\n")
}

test_that("scalar/rng/void functions: stripping, name extraction, registry", {
  cpp_code <- read_fixture("expose-fixture-scalar.cpp")

  result <- newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = character())

  expect_equal(result$functions$name, c("my_add", "my_add_rng", "say_hello"))
  expect_equal(result$functions$is_rng, c(FALSE, TRUE, FALSE))

  # pstream__ stripped, sole-argument and trailing-argument cases both
  # leave clean argument lists (no dangling comma).
  expect_match(
    result$wrapper_section,
    "double my_add(const double& a, const double& b) {",
    fixed = TRUE
  )
  expect_match(
    result$wrapper_section,
    "void say_hello() {",
    fixed = TRUE
  )
  # base_rng__ + pstream__ both stripped from an _rng signature.
  expect_match(
    result$wrapper_section,
    "double my_add_rng(const double& a, const double& b) {",
    fixed = TRUE
  )
  expect_no_match(result$wrapper_section, "pstream__ = nullptr", fixed = TRUE)
  # base_rng__ remains only as the file-static prelude variable and the
  # body's reference to it -- the parameter itself is stripped from the
  # signature (checked above via the exact my_add_rng signature match).
  expect_no_match(
    result$wrapper_section,
    "stan::rng_t& base_rng__",
    fixed = TRUE
  )
  expect_no_match(result$wrapper_section, ", )", fixed = TRUE)
  expect_no_match(result$wrapper_section, "// [[stan::function]]", fixed = TRUE)
  expect_match(result$wrapper_section, "// [[Rcpp::export]]", fixed = TRUE)

  expect_match(
    result$wrapper_section,
    'Rcpp::Named("name") = Rcpp::CharacterVector::create("my_add", "my_add_rng", "say_hello")',
    fixed = TRUE
  )
  expect_match(
    result$wrapper_section,
    'Rcpp::Named("is_rng") = Rcpp::LogicalVector::create(false, true, false)',
    fixed = TRUE
  )
  expect_match(result$wrapper_section, "newstan_rng_set_seed", fixed = TRUE)
  expect_match(result$wrapper_section, "newstan_exposed_functions", fixed = TRUE)
})

test_that("multi-line signatures wrapped mid-parameter are collapsed correctly", {
  cpp_code <- read_fixture("expose-fixture-vecmat.cpp")

  result <- newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = character())

  expect_equal(result$functions$name, c("vec_add", "mat_mult"))
  expect_equal(result$functions$is_rng, c(FALSE, FALSE))

  expect_match(
    result$wrapper_section,
    "Eigen::Matrix<double,-1,1> vec_add(const Eigen::Matrix<double,-1,1>& a, const Eigen::Matrix<double,-1,1>& b) {",
    fixed = TRUE
  )
  expect_match(
    result$wrapper_section,
    "Eigen::Matrix<double,-1,-1> mat_mult(const Eigen::Matrix<double,-1,-1>& a, const Eigen::Matrix<double,-1,-1>& b) {",
    fixed = TRUE
  )
})

test_that("array/nested-vector/int argument and return types", {
  cpp_code <- read_fixture("expose-fixture-array.cpp")

  result <- newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = character())

  expect_equal(result$functions$name, c("arr_fun", "rv_fun", "int_arr_fun"))
  expect_equal(result$functions$is_rng, c(FALSE, FALSE, FALSE))

  expect_match(
    result$wrapper_section,
    "std::vector<double> arr_fun(const std::vector<double>& x, const int& n) {",
    fixed = TRUE
  )
  expect_match(
    result$wrapper_section,
    "Eigen::Matrix<double,1,-1> rv_fun(const Eigen::Matrix<double,1,-1>& x) {",
    fixed = TRUE
  )
  expect_match(
    result$wrapper_section,
    "std::vector<std::vector<int>> int_arr_fun(const std::vector<std::vector<int>>& x) {",
    fixed = TRUE
  )
})

test_that("tuple-returning functions are exposed with intact signatures", {
  cpp_code <- read_fixture("expose-fixture-tuple-skip.cpp")

  result <- newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = character())

  expect_equal(result$functions$name, c("two_things", "keep_me"))
  expect_true("two_things" %in% result$functions$name)
  expect_match(
    result$wrapper_section,
    "std::tuple<double, double> two_things(const double& a) {",
    fixed = TRUE
  )
  expect_match(
    result$wrapper_section,
    "double keep_me(const double& a) {",
    fixed = TRUE
  )
})

test_that("overloaded Stan functions keep only the first, with a warning", {
  cpp_code <- read_fixture("expose-fixture-overload.cpp")

  expect_warning(
    result <- newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = character()),
    "duplicate"
  )

  expect_equal(result$functions$name, "overload")
  expect_equal(nrow(result$functions), 1L)
  # The surviving wrapper is the first-defined overload (single-argument).
  expect_match(
    result$wrapper_section,
    "model_namespace::overload(a, pstream__)",
    fixed = TRUE
  )
  expect_no_match(
    result$wrapper_section,
    "model_namespace::overload(a, b, pstream__)",
    fixed = TRUE
  )
})

test_that("a name colliding with reserved_names is a hard error", {
  cpp_code <- read_fixture("expose-fixture-scalar.cpp")

  expect_error(
    newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = c("my_add", "run_model")),
    "my_add.*reserved"
  )
})

test_that("a program with no [[stan::function]] markers errors", {
  expect_error(
    newstan:::.newstan_process_standalone_cpp(
      "#include <stan/model/model_header.hpp>\nnamespace model_namespace {\n}\n",
      reserved_names = character()
    ),
    "no functions"
  )
})

test_that("a program whose only function returns a tuple is exposed successfully", {
  cpp_code <- read_fixture("expose-fixture-tuple-only.cpp")

  result <- newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = character())

  expect_equal(result$functions$name, "two_things")
  expect_match(
    result$wrapper_section,
    "std::tuple<double, double> two_things(const double& a) {",
    fixed = TRUE
  )
})

test_that("wrapper_section excludes model_namespace; full_code includes it", {
  cpp_code <- read_fixture("expose-fixture-scalar.cpp")

  result <- newstan:::.newstan_process_standalone_cpp(cpp_code, reserved_names = character())

  expect_no_match(result$wrapper_section, "namespace model_namespace", fixed = TRUE)
  expect_no_match(
    result$wrapper_section,
    "#include <stan/model/model_header.hpp>",
    fixed = TRUE
  )
  expect_match(result$full_code, "namespace model_namespace", fixed = TRUE)
  expect_match(
    result$full_code,
    "#include <stan/model/model_header.hpp>",
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(result$full_code, gregexpr("namespace model_namespace", result$full_code))),
    1L
  )
  expect_match(result$full_code, "// [[Rcpp::export]]", fixed = TRUE)
})

# Below: integration tests that actually invoke stanc.js + a real C++
# compile (via .compile_standalone_functions_environment()). Slow but
# expected for this package; the cache dir is redirected by setup.R.

test_that(".compile_standalone_functions_environment compiles a functions block into a callable env", {
  code <- "
functions {
  real my_add(real a, real b) { return a + b; }
  real my_add_rng(real a, real b) { return a + b + normal_rng(0, 1); }
  vector vec_add(vector a, vector b) { return a + b; }
}
"
  env <- newstan:::.compile_standalone_functions_environment(code)

  expect_true(is.function(env$newstan_exposed_functions))
  expect_true(is.function(env$newstan_rng_set_seed))
  expect_true(is.function(env$my_add))
  expect_true(is.function(env$my_add_rng))
  expect_true(is.function(env$vec_add))

  registry <- env$newstan_exposed_functions()
  expect_equal(sort(registry$name), sort(c("my_add", "my_add_rng", "vec_add")))

  expect_equal(env$my_add(2, 3), 5)
  expect_equal(env$vec_add(c(1, 2), c(3, 4)), c(4, 6))
})

test_that("tuple/complex functions compile and round-trip through Rcpp marshalling", {
  code <- "
functions {
  tuple(real, vector) split_stat(vector x) { return (mean(x), head(x, 2)); }
  complex_vector rotate(complex_vector z, complex phase) { return z * phase; }
  complex_matrix cmat_id(complex_matrix m) { return m; }
  complex_row_vector crv_id(complex_row_vector v) { return v; }
  array[] tuple(complex, real) mixed_out(tuple(complex_matrix, int) tm) {
    return {(tm.1[1,1], 1.0), (tm.1[2,2], 2.0)};
  }
  real tup_arr_in(data array[] tuple(real, array[] int) v) {
    real total = 0;
    for (e in v) {
      total += e.1;
      for (j in e.2) {
        total += j;
      }
    }
    return total;
  }
  array[,] tuple(int, real) deep_id(array[,] tuple(int, real) v) { return v; }
  complex czmul_rng(complex a) { return a * normal_rng(0, 1); }
}
"
  env <- newstan:::.compile_standalone_functions_environment(code)

  # tuple(real, vector) return: unnamed list, element 2 shaped as the
  # declared vector.
  expect_equal(env$split_stat(c(1, 2, 3)), list(2, c(1, 2)))

  # complex_vector * complex scalar (elementwise rotation).
  expect_equal(
    env$rotate(c(1 + 2i, 3 - 1i), 1i),
    c(-2 + 1i, 1 + 3i)
  )

  cm <- matrix(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i), nrow = 2)
  expect_equal(env$cmat_id(cm), cm)

  # complex_row_vector round-trips as a 1xn matrix -- the same shape
  # RcppEigen already uses for a plain (real) row_vector.
  crv <- c(1 + 1i, 2 + 2i)
  expect_equal(env$crv_id(crv), matrix(crv, nrow = 1))

  # tuple(complex_matrix, int) argument; array[] tuple(complex, real)
  # return -- an unnamed list of unnamed lists (AoS), picking diagonal
  # entries of the matrix slot and ignoring the int slot.
  expect_equal(
    env$mixed_out(list(cm, 7L)),
    list(list(cm[1, 1], 1), list(cm[2, 2], 2))
  )

  # data-qualified array[] tuple(real, array[] int) argument: a list of
  # tuple-shaped lists in, a plain scalar out.
  expect_equal(
    env$tup_arr_in(list(list(1.5, c(1L, 2L)), list(2.5, c(3L)))),
    1.5 + 1 + 2 + 2.5 + 3
  )

  # array[,] tuple(int, real): list (over first index) of lists (over
  # second index) of tuples -- exact round trip through an identity
  # function.
  deep_in <- list(
    list(list(1L, 1.5), list(2L, 2.5)),
    list(list(3L, 3.5), list(4L, 4.5))
  )
  expect_equal(env$deep_id(deep_in), deep_in)

  # `_rng` function taking complex: reproducible with an explicit seed.
  env$newstan_rng_set_seed(1L)
  v1 <- env$czmul_rng(2 + 0i)
  env$newstan_rng_set_seed(1L)
  v2 <- env$czmul_rng(2 + 0i)
  expect_equal(v1, v2)
})

test_that("compiling the same code twice without force_recompile reuses the cached .cpp file", {
  code <- "
functions {
  real cache_reuse_add(real a, real b) { return a + b; }
}
"
  cache_dir <- getOption("newstan_cache_dir")

  env1 <- newstan:::.compile_standalone_functions_environment(code)
  n_after_first <- length(
    list.files(cache_dir, pattern = "^functions_.*\\.cpp$")
  )

  env2 <- newstan:::.compile_standalone_functions_environment(code)
  n_after_second <- length(
    list.files(cache_dir, pattern = "^functions_.*\\.cpp$")
  )

  expect_equal(n_after_first, n_after_second)
  expect_equal(env1$cache_reuse_add(2, 3), 5)
  expect_equal(env2$cache_reuse_add(2, 3), 5)
})

test_that(".newstan_build_functions_env populates a target env with callable functions, wrapping _rng exports", {
  code <- "
functions {
  real build_env_add(real a, real b) { return a + b; }
  real build_env_add_rng(real a, real b) { return a + b + normal_rng(0, 1); }
}
"
  compiled_env <- newstan:::.compile_standalone_functions_environment(code)
  target_env <- new.env()

  newstan:::.newstan_build_functions_env(compiled_env, target_env, global = FALSE)

  expect_true(is.function(target_env$build_env_add))
  expect_equal(target_env$build_env_add(2, 3), 5)

  rng_fn <- target_env$build_env_add_rng
  expect_true(is.function(rng_fn))
  # explicit formals copied from the compiled export: real argument names,
  # not `...`, plus a trailing seed = NULL.
  expect_equal(names(formals(rng_fn)), c("a", "b", "seed"))
  expect_null(formals(rng_fn)$seed)

  # same explicit seed on two separate calls -> identical draws
  expect_equal(rng_fn(1, 2, seed = 42), rng_fn(1, 2, seed = 42))

  # required-argument propagation still works through the wrapper
  expect_error(rng_fn(1))
})

test_that(".newstan_build_functions_env(global = TRUE) assigns into a designated env, never the real .GlobalEnv", {
  code <- "
functions {
  real global_test_add(real a, real b) { return a + b; }
}
"
  compiled_env <- newstan:::.compile_standalone_functions_environment(code)
  target_env <- new.env()
  test_global <- new.env()

  newstan:::.newstan_build_functions_env(
    compiled_env,
    target_env,
    global = TRUE,
    global_env = test_global
  )

  expect_true(is.function(test_global$global_test_add))
  expect_equal(test_global$global_test_add(2, 3), 5)
  expect_false(exists("global_test_add", envir = globalenv(), inherits = FALSE))
})

test_that("rebuilding via .newstan_build_functions_env clears stale bindings from a previous build", {
  code <- "
functions {
  real rebuild_add(real a, real b) { return a + b; }
}
"
  compiled_env <- newstan:::.compile_standalone_functions_environment(code)
  target_env <- new.env()

  newstan:::.newstan_build_functions_env(compiled_env, target_env, global = FALSE)
  assign("stale_binding", 123, envir = target_env)
  expect_true(exists("stale_binding", envir = target_env, inherits = FALSE))

  newstan:::.newstan_build_functions_env(compiled_env, target_env, global = FALSE)

  expect_false(exists("stale_binding", envir = target_env, inherits = FALSE))
  expect_true(is.function(target_env$rebuild_add))
  expect_equal(target_env$rebuild_add(4, 5), 9)
})

# Below: StanModel-level integration tests for `$expose_stan_functions()` /
# `$expose_functions()`.

test_that("StanModel$expose_stan_functions() populates $functions on a compiled model", {
  mod <- stan_model(
    code = "
functions {
  real my_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  result <- withVisible(mod$expose_stan_functions())
  expect_false(result$visible)
  expect_identical(result$value, mod$functions)

  expect_true(is.function(mod$functions$my_add))
  expect_equal(mod$functions$my_add(2, 3), 5)
})

test_that("StanModel$expose_stan_functions(global = TRUE) works and can be verified safely", {
  mod <- stan_model(
    code = "
functions {
  real model_global_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  on.exit(
    if (exists("model_global_add", envir = globalenv(), inherits = FALSE)) {
      rm("model_global_add", envir = globalenv())
    },
    add = TRUE
  )

  expect_no_error(mod$expose_stan_functions(global = TRUE))
  expect_true(is.function(mod$functions$model_global_add))
  expect_equal(mod$functions$model_global_add(2, 3), 5)
  expect_true(exists("model_global_add", envir = globalenv(), inherits = FALSE))
  expect_equal(get("model_global_add", envir = globalenv())(2, 3), 5)
})

test_that("a second StanModel$expose_stan_functions() call does not error and still works", {
  mod <- stan_model(
    code = "
functions {
  real model_twice_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  mod$expose_stan_functions()
  first_env <- mod$functions
  expect_equal(mod$functions$model_twice_add(2, 3), 5)

  expect_no_error(mod$expose_stan_functions())
  expect_identical(mod$functions, first_env)
  expect_equal(mod$functions$model_twice_add(4, 5), 9)
})

test_that("StanModel$expose_functions() alias works identically to $expose_stan_functions()", {
  mod <- stan_model(
    code = "
functions {
  real model_alias_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  result <- mod$expose_functions()
  expect_identical(result, mod$functions)
  expect_true(is.function(mod$functions$model_alias_add))
  expect_equal(mod$functions$model_alias_add(2, 3), 5)
})

test_that("expose_stan_functions() works on a compile = FALSE model without compiling it", {
  mod <- stan_model(
    code = "
functions {
  real never_compiled_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile = FALSE
  )

  expect_false(mod$is_compiled())

  mod$expose_stan_functions()

  expect_true(is.function(mod$functions$never_compiled_add))
  expect_equal(mod$functions$never_compiled_add(2, 3), 5)
  # The most important assertion in this batch: exposing functions must not
  # trigger a full model compile as a side effect.
  expect_false(mod$is_compiled())
})

test_that("expose_stan_functions() errors for a Stan program with no functions block", {
  mod <- stan_model(
    code = "
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile = FALSE
  )

  expect_error(mod$expose_stan_functions())
})

# Below: `compile_standalone = TRUE` integration tests -- the combined-TU
# path, where the model's own compile appends the exposed-functions wrapper
# section directly into the model's translation unit (see
# .compile_stan_model_environment()'s `standalone_functions` argument).

test_that("compile_standalone = TRUE exposes functions with zero extra compilation, and the model still works normally", {
  mod <- stan_model(
    code = "
functions {
  real my_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  # No call to $expose_stan_functions() anywhere above -- functions must
  # already be populated as a side effect of $compile() / stan_model().
  expect_true(is.function(mod$functions$my_add))
  expect_equal(mod$functions$my_add(2, 3), 5)

  # Most important correctness check: the combined TU must not break normal
  # model compilation/ODR-safety.
  expect_true(mod$is_compiled())
  result <- mod$sample(
    data = list(),
    chains = 1,
    iter_warmup = 10,
    iter_sampling = 10,
    refresh = 0,
    show_messages = FALSE
  )
  expect_equal(result$return_codes(), 0L)
  expect_true("theta" %in% posterior::variables(result$draws()))
  expect_equal(posterior::ndraws(result$draws()), 10L)
})

test_that("a second compile_standalone model with identical code hits the on-disk cache and still populates $functions", {
  code <- "
functions {
  real cache_hit_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))
  cache_dir <- getOption("newstan_cache_dir")

  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "newstan"
  )

  mod1 <- stan_model(
    code = code,
    compile_standalone = TRUE,
    precompiled_headers = FALSE
  )
  expect_true(mod1$is_compiled())
  expect_equal(mod1$functions$cache_hit_add(2, 3), 5)
  calls_after_first <- call_count
  expect_gt(calls_after_first, 0L)

  cpp_files_after_first <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_length(cpp_files_after_first, 1L)

  # Fresh R6 object, identical code: resolves to a cache hit under the
  # hood (stanc() is not invoked again), but the post-compile hook that
  # populates $functions must still run.
  mod2 <- stan_model(
    code = code,
    compile_standalone = TRUE,
    precompiled_headers = FALSE
  )
  expect_true(mod2$is_compiled())
  expect_equal(call_count, calls_after_first)
  expect_true(is.function(mod2$functions$cache_hit_add))
  expect_equal(mod2$functions$cache_hit_add(4, 5), 9)

  cpp_files_after_second <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_equal(cpp_files_after_second, cpp_files_after_first)
})

test_that("compile_standalone participates in the model cache key (distinct .cpp files)", {
  code <- "
functions {
  real hash_sep_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  cache_root <- withr::local_tempdir()
  withr::local_options(newstan_cache_dir = file.path(cache_root, "models"))
  cache_dir <- getOption("newstan_cache_dir")

  mod_plain <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod_plain$is_compiled())
  cpp_files_after_plain <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_length(cpp_files_after_plain, 1L)

  # Same Stan code, `compile_standalone = TRUE`: the generated .cpp content
  # differs (it has the appended wrapper section), so this must be a
  # distinct cache entry, not a reuse of the plain model's.
  mod_standalone <- stan_model(
    code = code,
    compile_standalone = TRUE,
    precompiled_headers = FALSE
  )
  expect_true(mod_standalone$is_compiled())
  cpp_files_after_standalone <- list.files(cache_dir, pattern = "[.]cpp$")
  expect_length(cpp_files_after_standalone, 2L)
})

test_that("compile_standalone errors on a Stan function named run_model (reserved-name collision)", {
  code <- "
functions {
  real run_model(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  expect_error(
    stan_model(code = code, compile_standalone = TRUE),
    "run_model.*reserved"
  )
})

test_that("compile_standalone works together with external_cpp: model, external fn, and plain fn all callable", {
  code <- paste(
    "functions {",
    "  real external_mean(real x);",
    "  real double_it(real x) { return 2 * x; }",
    "}",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  mod <- stan_model(
    code = code,
    external_cpp = test_path("test-models/external_mean.hpp"),
    compile_standalone = TRUE
  )

  expect_true(mod$is_compiled())
  expect_true(is.function(mod$functions$double_it))
  expect_equal(mod$functions$double_it(3), 6)
  # external_mean's C++ implementation (test-models/external_mean.hpp) just
  # returns its argument unchanged.
  expect_true(is.function(mod$functions$external_mean))
  expect_equal(mod$functions$external_mean(7), 7)
})

test_that("expose_stan_functions(global = TRUE) on a compile_standalone model uses the fast path and sets globals", {
  mod <- stan_model(
    code = "
functions {
  real standalone_global_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  on.exit(
    if (exists("standalone_global_add", envir = globalenv(), inherits = FALSE)) {
      rm("standalone_global_add", envir = globalenv())
    },
    add = TRUE
  )

  # A compile_standalone model's compiled_env_ already provides
  # newstan_exposed_functions(), so $expose_stan_functions() must take the
  # fast path and never call the separate-TU compile helper.
  testthat::local_mocked_bindings(
    .compile_standalone_functions_environment = function(...) {
      stop("must not recompile: compile_standalone model already has newstan_exposed_functions")
    },
    .package = "newstan"
  )

  expect_no_error(mod$expose_stan_functions(global = TRUE))
  expect_true(is.function(mod$functions$standalone_global_add))
  expect_equal(mod$functions$standalone_global_add(2, 3), 5)
  expect_true(exists("standalone_global_add", envir = globalenv(), inherits = FALSE))
  expect_equal(get("standalone_global_add", envir = globalenv())(2, 3), 5)
})

test_that("$compile(force_recompile = TRUE) on a compile_standalone model keeps $functions live and correct", {
  mod <- stan_model(
    code = "
functions {
  real force_recompile_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  expect_equal(mod$functions$force_recompile_add(2, 3), 5)

  mod$compile(force_recompile = TRUE)

  expect_true(is.function(mod$functions$force_recompile_add))
  expect_equal(mod$functions$force_recompile_add(4, 5), 9)
})

test_that("compile_standalone = TRUE combined-TU model exposes a tuple function correctly", {
  mod <- stan_model(
    code = "
functions {
  tuple(real, real) combined_tuple_fn(real a) { return (a, a * 2); }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  expect_true(mod$is_compiled())
  expect_true(is.function(mod$functions$combined_tuple_fn))
  expect_equal(mod$functions$combined_tuple_fn(3), list(3, 6))
})
