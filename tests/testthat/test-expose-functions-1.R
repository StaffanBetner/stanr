local_test_context()

init_test_cache("expose-functions-1")

test_that("scalar/rng/void functions: stripping, name extraction, registry", {
  cpp_code <- stanc(
    "
functions {
  real my_add(real a, real b) { return a + b; }
  real my_add_rng(real a, real b) { return a + b + normal_rng(0, 1); }
  void say_hello() { print(\"hello\"); }
}
",
    standalone_functions = TRUE
  )

  result <- stanr:::.stanr_process_standalone_cpp(
    cpp_code,
    reserved_names = character()
  )

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
  expect_match(result$wrapper_section, "stanr_rng_set_seed", fixed = TRUE)
  expect_match(
    result$wrapper_section,
    "stanr_exposed_functions",
    fixed = TRUE
  )
})

test_that("multi-line signatures wrapped mid-parameter are collapsed correctly", {
  cpp_code <- stanc(
    "
functions {
  vector vec_add(vector a, vector b) { return a + b; }
  matrix mat_mult(matrix a, matrix b) { return a * b; }
}
",
    standalone_functions = TRUE
  )

  result <- stanr:::.stanr_process_standalone_cpp(
    cpp_code,
    reserved_names = character()
  )

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
  cpp_code <- stanc(
    "
functions {
  array[] real arr_fun(array[] real x, int n) { return x; }
  row_vector rv_fun(row_vector x) { return x; }
  array[,] int int_arr_fun(array[,] int x) { return x; }
}
",
    standalone_functions = TRUE
  )

  result <- stanr:::.stanr_process_standalone_cpp(
    cpp_code,
    reserved_names = character()
  )

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
  cpp_code <- stanc(
    "
functions {
  tuple(real, real) two_things(real a) { return (a, a + 1); }
  real keep_me(real a) { return a * 2; }
}
",
    standalone_functions = TRUE
  )

  result <- stanr:::.stanr_process_standalone_cpp(
    cpp_code,
    reserved_names = character()
  )

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
  cpp_code <- stanc(
    "
functions {
  real overload(real a) { return a; }
  real overload(real a, real b) { return a + b; }
}
",
    standalone_functions = TRUE
  )

  expect_warning(
    result <- stanr:::.stanr_process_standalone_cpp(
      cpp_code,
      reserved_names = character()
    ),
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
  cpp_code <- stanc(
    "
functions {
  real my_add(real a, real b) { return a + b; }
}
",
    standalone_functions = TRUE
  )

  expect_error(
    stanr:::.stanr_process_standalone_cpp(
      cpp_code,
      reserved_names = c("my_add", "run_model")
    ),
    "my_add.*reserved"
  )
})

test_that("a program with no [[stan::function]] markers errors", {
  expect_error(
    stanr:::.stanr_process_standalone_cpp(
      "#include <stan/model/model_header.hpp>\nnamespace model_namespace {\n}\n",
      reserved_names = character()
    ),
    "no functions"
  )
})

test_that("a program whose only function returns a tuple is exposed successfully", {
  cpp_code <- stanc(
    "
functions {
  tuple(real, real) two_things(real a) { return (a, a + 1); }
}
",
    standalone_functions = TRUE
  )

  result <- stanr:::.stanr_process_standalone_cpp(
    cpp_code,
    reserved_names = character()
  )

  expect_equal(result$functions$name, "two_things")
  expect_match(
    result$wrapper_section,
    "std::tuple<double, double> two_things(const double& a) {",
    fixed = TRUE
  )
})

test_that("wrapper_section excludes model_namespace; full_code includes it", {
  cpp_code <- stanc(
    "
functions {
  real my_add(real a, real b) { return a + b; }
}
",
    standalone_functions = TRUE
  )

  result <- stanr:::.stanr_process_standalone_cpp(
    cpp_code,
    reserved_names = character()
  )

  expect_no_match(
    result$wrapper_section,
    "namespace model_namespace",
    fixed = TRUE
  )
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
    lengths(regmatches(
      result$full_code,
      gregexpr("namespace model_namespace", result$full_code)
    )),
    1L
  )
  expect_match(result$full_code, "// [[Rcpp::export]]", fixed = TRUE)
})

withr::deferred_run()
