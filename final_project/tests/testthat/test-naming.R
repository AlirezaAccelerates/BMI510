# Test case to check if all functions are implemented
library(testthat)

test_that("All functions are implemented", {
  expect_true(is.function(rando))
  expect_true(is.function(is_min))
  expect_true(is.function(is_max))
  expect_true(is.function(rep_mat))
  expect_true(is.function(classes))
  expect_true(is.function(df_scale))
  expect_true(is.function(log_likelihood_norm))
  expect_true(is.function(log_likelihood_unif))
  expect_true(is.function(log_likelihood_chisq))
  expect_true(is.function(log_likelihood_f))
  expect_true(is.function(log_likelihood_t))
  expect_true(is.function(sensitivity))
  expect_true(is.function(specificity))
  expect_true(is.function(precision))
  expect_true(is.function(recall))
  expect_true(is.function(accuracy))
  expect_true(is.function(f1))
  expect_true(is.function(minimum_n_per_group))
  expect_true(is.function(r2))
  expect_true(is.function(adj_R2))
})

# Test case to check if all functions are implemented with the correct arguments

test_that("All functions are implemented with the correct arguments", {
  # Check if function exists and has the correct arguments
  check_function <- function(fun_name, expected_args) {
    expect_true(is.function(get(fun_name)))
    expect_equal(names(formals(get(fun_name))), expected_args)
  }

  check_function("rando", c("x", "n", "replace"))
  check_function("is_min", c("x", "na.rm"))
  check_function("is_max", c("x", "na.rm"))
  check_function("rep_mat", c("x", "M", "N"))
  check_function("classes", "x")
  check_function("df_scale", c("x", "center", "scale"))
  check_function("log_likelihood_norm", c("x", "mean", "sd"))
  check_function("log_likelihood_unif", c("x", "min", "max"))
  check_function("log_likelihood_chisq", c("x", "df"))
  check_function("log_likelihood_f", c("x", "df1", "df2"))
  check_function("log_likelihood_t", c("x", "df"))
  check_function("sensitivity", c("pred", "truth"))
  check_function("specificity", c("pred", "truth"))
  check_function("precision", c("pred", "truth"))
  check_function("recall", c("pred", "truth"))
  check_function("accuracy", c("pred", "truth"))
  check_function("f1", c("pred", "truth"))
  check_function("minimum_n_per_group", c("d", "power"))
  check_function("r2", c("pred", "truth"))
  check_function("adj_R2", c("pred", "truth", "n_p"))
})
