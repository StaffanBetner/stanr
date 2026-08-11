local_test_context()

init_test_cache("expose-functions-1")

# Tests for the AST-based wrapper generator `.stanr_functions_to_cpp_wrappers`.
# It runs stanc's debug-ast and emits one `extern "C" SEXP <fn>_sexp(...)`
# wrapper per function (using stanr::as_cpp / stanr::as_sexp) plus a registry.

wrapper_for <- function(code) {
  stanr:::.stanr_functions_to_cpp_wrappers(code)
}

test_that("scalar/rng/void functions: name/type extraction, registry", {
  result <- wrapper_for(
    "
functions {
  real my_add(real a, real b) { return a + b; }
  real my_add_rng(real a, real b) { return a + b + normal_rng(0, 1); }
  void say_hello() { print(\"hello\"); }
}
"
  )

  expect_equal(result$functions$name, c("my_add", "my_add_rng", "say_hello"))
  expect_equal(result$functions$is_rng, c(FALSE, TRUE, FALSE))

  code <- result$code
  # Each wrapper is an extern "C" SEXP routine.
  expect_match(code, "extern \"C\" SEXP my_add_sexp", fixed = TRUE)
  expect_match(code, "extern \"C\" SEXP my_add_rng_sexp", fixed = TRUE)
  expect_match(code, "extern \"C\" SEXP say_hello_sexp", fixed = TRUE)

  # stanr::as_cpp fills the args; stanr::as_sexp returns the result.
  expect_match(code, "stanr::as_cpp<double>(a_sexp)", fixed = TRUE)
  expect_match(code, "stanr::as_sexp(model_namespace::my_add(a, b, pstream__))", fixed = TRUE)

  # RNG wrapper takes a seed and builds a fresh RNG.
  expect_match(code, "SEXP seed_sexp", fixed = TRUE)
  expect_match(
    code,
    "model_namespace::my_add_rng(a, b, base_rng__, pstream__)",
    fixed = TRUE
  )

  # Void wrapper returns R_NilValue.
  expect_match(code, "model_namespace::say_hello(pstream__);", fixed = TRUE)
  expect_match(code, "return R_NilValue;", fixed = TRUE)

  # Registry carries name / is_rng / args.
  expect_match(code, 'cpp11::named_arg("name") = names', fixed = TRUE)
  expect_match(code, 'cpp11::named_arg("is_rng") = is_rng', fixed = TRUE)
  expect_match(code, 'cpp11::named_arg("args") = args', fixed = TRUE)
  expect_match(
    code,
    "extern \"C\" SEXP stanr_exposed_functions",
    fixed = TRUE
  )
})

test_that("vector/matrix argument and return types map to Eigen", {
  result <- wrapper_for(
    "
functions {
  vector vec_add(vector a, vector b) { return a + b; }
  matrix mat_mult(matrix a, matrix b) { return a * b; }
}
"
  )

  expect_equal(result$functions$name, c("vec_add", "mat_mult"))
  expect_equal(result$functions$is_rng, c(FALSE, FALSE))

  code <- result$code
  expect_match(
    code,
    "stanr::as_cpp<Eigen::Matrix<double,-1,1>>(a_sexp)",
    fixed = TRUE
  )
  expect_match(
    code,
    "stanr::as_cpp<Eigen::Matrix<double,-1,-1>>(a_sexp)",
    fixed = TRUE
  )
})

test_that("array/nested-vector/int/row_vector types map correctly", {
  result <- wrapper_for(
    "
functions {
  array[] real arr_fun(array[] real x, int n) { return x; }
  row_vector rv_fun(row_vector x) { return x; }
  array[,] int int_arr_fun(array[,] int x) { return x; }
}
"
  )

  expect_equal(result$functions$name, c("arr_fun", "rv_fun", "int_arr_fun"))
  code <- result$code
  expect_match(code, "stanr::as_cpp<std::vector<double>>(x_sexp)", fixed = TRUE)
  expect_match(code, "stanr::as_cpp<int>(n_sexp)", fixed = TRUE)
  expect_match(
    code,
    "stanr::as_cpp<Eigen::Matrix<double,1,-1>>(x_sexp)",
    fixed = TRUE
  )
  expect_match(
    code,
    "stanr::as_cpp<std::vector<std::vector<int>>>(x_sexp)",
    fixed = TRUE
  )
})

test_that("tuple-returning functions are exposed with std::tuple types", {
  result <- wrapper_for(
    "
functions {
  tuple(real, real) two_things(real a) { return (a, a + 1); }
  real keep_me(real a) { return a * 2; }
}
"
  )

  expect_equal(result$functions$name, c("two_things", "keep_me"))
  code <- result$code
  # two_things takes a real and returns a tuple; the wrapper wraps the result.
  expect_match(code, "stanr::as_cpp<double>(a_sexp)", fixed = TRUE)
  expect_match(code, "stanr::as_sexp(model_namespace::two_things(a, pstream__))", fixed = TRUE)
  expect_match(code, "model_namespace::keep_me(a, pstream__)", fixed = TRUE)
})

test_that("a program with no functions block returns empty", {
  result <- wrapper_for(
    "
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )
  expect_null(result$functions)
  expect_equal(result$code, character())
})

test_that("generated code includes the interop headers", {
  code <- wrapper_for(
    "
functions {
  real my_add(real a, real b) { return a + b; }
}
"
  )$code
  expect_match(code, "#include <cpp11.hpp>", fixed = TRUE)
  expect_match(code, "#include <stanr/cpp11_tuple_interop.hpp>", fixed = TRUE)
  expect_match(code, "#include <stan/model/model_header.hpp>", fixed = TRUE)
})

withr::deferred_run()