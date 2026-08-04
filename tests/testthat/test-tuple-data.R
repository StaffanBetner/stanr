local_test_context()

init_test_cache("tuple-data")

# Coverage for the tuple/complex data & init interop: the pure-R flattener
# (`R/tuples.R`), its wiring into `.newstan_run_service()` /
# `fit$unconstrain_variables()`, and the windowed-complex storage in
# `src/r_data_context.cpp`.

# --- .newstan_flatten_tuple_values(): pure-R unit tests, no compilation ------

test_that("the fast path returns data/init values unchanged when nothing is a list", {
  values <- list(N = 4L, y = c(1, 0, 1, 0), z = 1 + 2i)
  out <- .newstan_flatten_tuple_values(values, list())
  expect_identical(out, values)
})

test_that("flattening a tuple-array variable reproduces the worked example exactly", {
  # array[2] tuple(complex_vector[3], real) acv
  declared <- list(
    acv = list(
      type = data.frame(type = c("complex", "real"), dimensions = c(1, 0)),
      dimensions = 1
    )
  )
  cv1 <- complex(real = 1:3, imaginary = 11:13)
  cv2 <- complex(real = 4:6, imaginary = 14:16)
  values <- list(acv = list(list(cv1, 10), list(cv2, 20)))

  out <- .newstan_flatten_tuple_values(values, declared)

  expect_equal(as.complex(out$acv.1), c(cv1, cv2))
  expect_equal(dim(out$acv.1), c(2L, 3L))
  expect_equal(attr(out$acv.1, "newstan_array_dims"), 1L)
  expect_equal(out$acv.2, c(10, 20))
  expect_null(dim(out$acv.2))
})

test_that("a plain (non-array) tuple flattens to per-slot scalars/vectors", {
  declared <- list(
    td = list(
      type = data.frame(type = c("real", "real"), dimensions = c(0, 1)),
      dimensions = 0
    )
  )
  out <- .newstan_flatten_tuple_values(
    list(td = list(1.5, c(2, 3))),
    declared
  )
  expect_equal(out$td.1, 1.5)
  expect_equal(out$td.2, c(2, 3))
  expect_null(dim(out$td.1))
})

test_that("array[2,3] tuple(int, real) flattens in blocked-AoS order", {
  declared <- list(
    g = list(
      type = data.frame(type = c("int", "real"), dimensions = c(0, 0)),
      dimensions = 2
    )
  )
  # Canonical shape: list (over first index, size 2) of lists (over second
  # index, size 3) of tuples.
  value <- list(
    list(list(11L, 1.1), list(12L, 1.2), list(13L, 1.3)),
    list(list(21L, 2.1), list(22L, 2.2), list(23L, 2.3))
  )
  out <- .newstan_flatten_tuple_values(list(g = value), declared)

  # Hand-computed blocked-AoS order: enclosing-array elements enumerated
  # column-major (first index fastest) -- (1,1),(2,1),(1,2),(2,2),(1,3),(2,3).
  expect_equal(out$g.1, c(11L, 21L, 12L, 22L, 13L, 23L), ignore_attr = TRUE)
  expect_equal(out$g.2, c(1.1, 2.1, 1.2, 2.2, 1.3, 2.3), ignore_attr = TRUE)
  expect_equal(dim(out$g.1), c(2L, 3L))
  expect_true(is.integer(out$g.1))
})

test_that("nested tuple(real, array[2] tuple(real, complex)) flattens correctly", {
  inner_df <- data.frame(type = c("real", "complex"), dimensions = c(0, 0))
  declared <- list(
    nt = list(
      type = data.frame(
        type = I(list("real", inner_df)),
        dimensions = c(0, 1)
      ),
      dimensions = 0
    )
  )
  value <- list(100, list(list(1, 1 + 2i), list(2, 3 + 4i)))
  out <- .newstan_flatten_tuple_values(list(nt = value), declared)

  expect_equal(out$nt.1, 100)
  expect_equal(out$nt.2.1, c(1, 2))
  expect_equal(out$nt.2.2, c(1 + 2i, 3 + 4i), ignore_attr = TRUE)
  expect_equal(attr(out$nt.2.2, "newstan_array_dims"), 1L)
})

test_that("real-valued input is accepted where a tuple slot declares complex", {
  declared <- list(
    tad = list(
      type = data.frame(type = c("int", "complex"), dimensions = c(0, 0)),
      dimensions = 1
    )
  )
  out <- .newstan_flatten_tuple_values(
    list(tad = list(list(1L, 2), list(3L, 4))),
    declared
  )
  expect_true(is.complex(out$tad.2))
  expect_equal(out$tad.2, as.complex(c(2, 4)), ignore_attr = TRUE)
})

test_that("a shape mismatch across enclosing array elements errors", {
  declared <- list(
    g = list(
      type = data.frame(type = "real", dimensions = 1),
      dimensions = 1
    )
  )
  expect_error(
    .newstan_flatten_tuple_values(
      list(g = list(list(c(1, 2)), list(c(1, 2, 3)))),
      declared
    ),
    "inconsistent value shapes"
  )
})

test_that("a named tuple list errors", {
  declared <- list(
    td = list(
      type = data.frame(type = c("real", "real"), dimensions = c(0, 1)),
      dimensions = 0
    )
  )
  expect_error(
    .newstan_flatten_tuple_values(
      list(td = list(a = 1.5, b = c(2, 3))),
      declared
    ),
    "unnamed list"
  )
})

test_that("a list for a variable not declared as a tuple errors", {
  declared <- list(x = list(type = "real", dimensions = 0))
  expect_error(
    .newstan_flatten_tuple_values(list(x = list(1, 2)), declared),
    "not declared as a tuple"
  )
  # Also covers a missing declaration entirely (same error class).
  expect_error(
    .newstan_flatten_tuple_values(list(zzz = list(1, 2)), declared),
    "not declared as a tuple"
  )
})

test_that("a dotted-name collision with a generated leaf errors", {
  declared <- list(
    td = list(
      type = data.frame(type = c("real", "real"), dimensions = c(0, 1)),
      dimensions = 0
    )
  )
  expect_error(
    .newstan_flatten_tuple_values(
      list(td = list(1.5, c(2, 3)), `td.1` = 99),
      declared
    ),
    "collide|already exist"
  )
})

test_that("wrong tuple arity errors", {
  declared <- list(
    td = list(
      type = data.frame(type = c("real", "real"), dimensions = c(0, 1)),
      dimensions = 0
    )
  )
  expect_error(
    .newstan_flatten_tuple_values(
      list(td = list(1.5, c(2, 3), 99)),
      declared
    ),
    "length 2"
  )
})

test_that("a non-rectangular tuple array errors", {
  declared <- list(
    g = list(
      type = data.frame(type = c("int", "real"), dimensions = c(0, 0)),
      dimensions = 2
    )
  )
  value <- list(
    list(list(11L, 1.1), list(12L, 1.2), list(13L, 1.3)),
    list(list(21L, 2.1), list(22L, 2.2))
  )
  expect_error(
    .newstan_flatten_tuple_values(list(g = value), declared),
    "not a rectangular tuple array"
  )
})

# --- Codegen-contract battery: compiled model, exact-equality draw echo -----
#
# Pins the stanc var_context reading contracts against upgrades. If this
# section fails after a stanc upgrade, suspect blocked-AoS tuple array
# flattening or windowed vals_c for tuple-slot complex leaves codegen drift
# first, not a Stan-library API change.

# Builds the expected (native dotted/colon name -> real-valued scalar) table
# for every leaf the battery model echoes. Names are the *native* flat
# constrained-parameter form -- never the bracketed form -- so the actual
# bracket strings used to index the draws always come from calling
# `.newstan_bracket_names()` itself (below), not from a hand-typed guess.
.newstan_battery_expected <- function(data) {
  complex_entry <- function(prefix, value) {
    stats::setNames(
      c(Re(value), Im(value)),
      c(paste0(prefix, ".real"), paste0(prefix, ".imag"))
    )
  }

  out <- c(
    complex_entry("zd_out", data$zd),
    complex_entry("zv_out.1", data$zv[1]),
    complex_entry("zv_out.2", data$zv[2]),
    complex_entry("za_out.1", data$za[1]),
    complex_entry("za_out.2", data$za[2])
  )
  for (j in 1:2) {
    for (i in 1:2) {
      out <- c(out, complex_entry(paste0("zm_out.", i, ".", j), data$zm[i, j]))
    }
  }
  out <- c(
    out,
    stats::setNames(data$td[[1]], "td_out:1"),
    stats::setNames(data$td[[2]][1], "td_out:2.1"),
    stats::setNames(data$td[[2]][2], "td_out:2.2")
  )
  for (e in 1:2) {
    out <- c(
      out,
      stats::setNames(data$tad[[e]][[1]], paste0("tad_out.", e, ":1")),
      complex_entry(paste0("tad_out.", e, ":2"), data$tad[[e]][[2]])
    )
  }
  for (e in 1:2) {
    cv <- data$acv[[e]][[1]]
    for (k in 1:3) {
      out <- c(out, complex_entry(paste0("acv_out.", e, ":1.", k), cv[k]))
    }
    out <- c(
      out,
      stats::setNames(data$acv[[e]][[2]], paste0("acv_out.", e, ":2"))
    )
  }
  for (i in 1:2) {
    for (j in 1:2) {
      out <- c(
        out,
        stats::setNames(
          data$t2d[[i]][[j]][[1]],
          paste0("t2d_out.", i, ".", j, ":1")
        ),
        stats::setNames(
          data$t2d[[i]][[j]][[2]],
          paste0("t2d_out.", i, ".", j, ":2")
        )
      )
    }
  }
  out <- c(out, stats::setNames(data$nt[[1]], "nt_out:1"))
  for (e in 1:2) {
    out <- c(
      out,
      stats::setNames(data$nt[[2]][[e]][[1]], paste0("nt_out:2.", e, ":1")),
      complex_entry(paste0("nt_out:2.", e, ":2"), data$nt[[2]][[e]][[2]])
    )
  }
  out
}

test_that("the battery model echoes every input exactly via generated quantities", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()

  fit <- mod$sample(
    data = data,
    iter_warmup = 2,
    iter_sampling = 2,
    chains = 1,
    seed = 42,
    show_messages = FALSE
  )
  expect_equal(fit$return_codes(), 0L)

  expected <- .newstan_battery_expected(data)
  # The expected keys are native dotted/colon names; the actual lookup keys
  # come from calling the real `.newstan_bracket_names()` on them, not from
  # a hand-typed bracket string.
  bracket_keys <- .newstan_bracket_names(names(expected))

  draws <- fit$draws(format = "draws_df")
  actual_names <- posterior::variables(draws)
  missing <- setdiff(bracket_keys, actual_names)
  expect_equal(missing, character())

  # Plain data.frame subsetting avoids `draws_df`'s metadata-preserving `[`
  # (which drops/warns when required columns like `.chain` aren't selected).
  draws_plain <- as.data.frame(draws)
  first_draw <- as.list(draws_plain[1, bracket_keys])
  expect_equal(
    unname(vapply(first_draw, as.numeric, numeric(1))),
    unname(expected)
  )
  # Every draw is identical: the echoed generated quantities don't depend on
  # the sampled parameter `x`.
  last_draw <- as.list(draws_plain[nrow(draws_plain), bracket_keys])
  expect_equal(
    unname(vapply(last_draw, as.numeric, numeric(1))),
    unname(expected)
  )

  # No stray/uncovered "*_out" columns: the expected table accounts for
  # every leaf the battery model declares.
  out_names <- actual_names[
    grepl("^(zd|zv|zm|za|td|tad|acv|t2d|nt)_out", actual_names)
  ]
  expect_equal(length(out_names), length(expected))
})

test_that("a list for a variable not declared as a tuple errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$zd <- list(1, 2)
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE
    ),
    "not declared as a tuple"
  )
})

test_that("wrong tuple arity errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$td <- list(1.5, c(2.5, 3.5), 99)
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE
    ),
    "length 2"
  )
})

test_that("a named tuple list errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$td <- list(a = 1.5, b = c(2.5, 3.5))
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE
    ),
    "unnamed list"
  )
})

test_that("a dotted-name collision errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data[["td.1"]] <- 42
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE
    ),
    "collide|already exist"
  )
})

test_that("a non-rectangular tuple array errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$t2d <- list(
    list(list(11L, 1.1), list(12L, 1.2)),
    list(list(21L, 2.1))
  )
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE
    ),
    "not a rectangular tuple array"
  )
})

# --- Init / unconstrain round trips for tuple and complex parameters --------

test_that("unconstrain_variables() is the identity for unbounded tuple/complex parameters", {
  mod <- test_model("tuple_complex_unbounded")
  fit <- mod$sample(
    data = list(),
    iter_warmup = 2,
    iter_sampling = 2,
    chains = 1,
    seed = 1,
    show_messages = FALSE,
    init = list(t = list(0.5, c(1, 2)), z = 0.1 + 0.2i)
  )
  expect_equal(fit$return_codes(), 0L)

  upars <- fit$unconstrain_variables(list(
    t = list(0.5, c(1, 2)),
    z = 0.1 + 0.2i
  ))
  expect_equal(as.numeric(upars), c(0.5, 1, 2, 0.1, 0.2))
})

test_that("sample() accepts a tuple/complex init list end to end", {
  mod <- test_model("tuple_complex_unbounded")
  fit <- mod$sample(
    data = list(),
    iter_warmup = 1,
    iter_sampling = 1,
    chains = 1,
    seed = 7,
    show_messages = FALSE,
    init = list(t = list(0, c(0, 0)), z = 0 + 0i)
  )
  expect_equal(fit$return_codes(), 0L)
})

withr::deferred_run()
