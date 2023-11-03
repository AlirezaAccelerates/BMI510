library(testthat)
library(tibble)
library(bmi510)
# Test case for the `rando` function
test_that("rando", {
  # Test sampling from an atomic vector without replacement
  vec1 <- 1:5
  vec1_sample <- rando(vec1, n = 3, replace = FALSE)
  expect_length(vec1_sample, 3)
  expect_true(all(vec1_sample %in% vec1))
  expect_true(anyDuplicated(vec1_sample) == 0)

  # Test sampling from an atomic vector with replacement
  vec2 <- c("A", "B", "C", "D", "E")
  vec2_sample <- rando(vec2, n = 5, replace = TRUE)
  expect_length(vec2_sample, 5)
  expect_true(all(vec2_sample %in% vec2))

  # Test sampling from a data frame without replacement
  df1 <- data.frame(a = 1:5, b = 6:10)
  df1_sample <- rando(df1, n = 3, replace = FALSE)
  expect_equal(nrow(df1_sample), 3)
  expect_true(all(df1_sample$a %in% df1$a))
  expect_true(all(df1_sample$b %in% df1$b))
  expect_true(anyDuplicated(df1_sample) == 0)

  # Test sampling from a data frame with replacement
  df2 <- data.frame(a = 11:15, b = 16:20)
  df2_sample <- rando(df2, n = 5, replace = TRUE)
  expect_equal(nrow(df2_sample), 5)
  expect_true(all(df2_sample$a %in% df2$a))
  expect_true(all(df2_sample$b %in% df2$b))

  # Test sampling from a matrix without replacement
  mat1 <- matrix(1:9, nrow = 3, ncol = 3)
  mat1_sample <- rando(mat1, n = 3, replace = FALSE)
  expect_equal(nrow(mat1_sample), 3)
  expect_true(all(mat1_sample[, 1] %in% mat1[, 1]))
  expect_true(all(mat1_sample[, 2] %in% mat1[, 2]))
  expect_true(all(mat1_sample[, 3] %in% mat1[, 3]))
  expect_true(anyDuplicated(mat1_sample) == 0)

  # Test sampling from a matrix with replacement
  mat2 <- matrix(11:19, nrow = 3, ncol = 3)
  mat2_sample <- rando(mat2, n = 5, replace = TRUE)
  expect_equal(nrow(mat2_sample), 5)
  expect_true(all(mat2_sample[, 1] %in% mat2[, 1]))
  expect_true(all(mat2_sample[, 2] %in% mat2[, 2]))
  expect_true(all(mat2_sample[, 3] %in% mat2[, 3]))

  # Test error message for empty input vector
  empty_vec <- numeric(0)
  expect_error(rando(empty_vec), "x must have at least one element")

  # Test error message for empty input data frame
  empty_df <- data.frame()
  expect_error(rando(empty_df), "x must have at least one row")

  # Test error message for invalid input type
  invalid_input <- list(1:5, 6:10)
  expect_error(rando(invalid_input))
})

# First, make sure the testthat package is installed
# install.packages("testthat")
library(testthat)

# Define the test cases
test_that("is_min function works as expected", {
  # Test case 1: simple vector without NA values
  expect_equal(is_min(c(1, 2, 3, 4, 5)), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  # Test case 2: vector with NA values and na.rm = TRUE (default)
  expect_equal(is_min(c(NA, 1, 2, 3, 4, 5)), c(TRUE, FALSE, FALSE, FALSE, FALSE))

  # Test case 3: vector with NA values and na.rm = FALSE
  expect_equal(is_min(c(NA, 1, 2, 3, 4, 5), na.rm = FALSE),
               c(NA, NA, NA, NA, NA, NA))

  # Test case 4: all elements are equal
  expect_equal(is_min(c(3, 3, 3)), c(TRUE, TRUE, TRUE))

  # Test case 5: vector with negative values
  expect_equal(is_min(c(-5,-3, 0, 2, 4)), c(TRUE, FALSE, FALSE, FALSE, FALSE))
})

# Define the test cases
test_that("is_max function works as expected", {
  # Test case 1: simple vector without NA values
  expect_equal(is_max(c(1, 2, 3, 4, 5)), c(FALSE, FALSE, FALSE, FALSE, TRUE))

  # Test case 2: vector with NA values and na.rm = TRUE (default)
  expect_equal(is_max(c(NA, 1, 2, 3, 4, 5)), c(FALSE, FALSE, FALSE, FALSE, TRUE))

  # Test case 3: vector with NA values and na.rm = FALSE
  expect_equal(is_max(c(NA, 1, 2, 3, 4, 5), na.rm = FALSE), c(NA, NA, NA, NA, NA, NA))

  # Test case 4: all elements are equal
  expect_equal(is_max(c(3, 3, 3)), c(TRUE, TRUE, TRUE))

  # Test case 5: vector with negative values
  expect_equal(is_max(c(-5, -3, 0, 2, 4)), c(FALSE, FALSE, FALSE, FALSE, TRUE))
})


test_that("rep_mat function works as expected", {
  # Create a matrix
  mat <- matrix(1:4, nrow = 2, ncol = 2)

  # Expected output
  expected_mat <-
    matrix(
      c(1, 2, 1, 2, 3, 4, 3, 4, 1, 2, 1, 2, 3, 4, 3, 4, 1, 2, 1, 2, 3, 4, 3, 4),
      nrow = 4,
      ncol = 6,
    )

  # Test rep_mat function
  replicated_mat <- rep_mat(mat, M = 2, N = 3)
  expect_equal(replicated_mat, expected_mat)

  # Test when input is a dataframe
  df <- as.data.frame(mat)
  replicated_df <- rep_mat(df, M = 2, N = 3)
  expect_equal(replicated_df, as.data.frame(expected_mat))

  # Test default values of M and N
  replicated_mat_default <- rep_mat(mat)
  expect_equal(replicated_mat_default, mat)

  # Test error when input is neither a matrix nor a dataframe
  incorrect_input <- list(a = 1, b = 2)
  expect_error(rep_mat(incorrect_input), "x must be a dataframe or matrix")
})

test_that("classes function works as expected", {
  # Create a dataframe
  df <-
    data.frame(
      a = 1:3,
      b = c("one", "two", "three"),
      stringsAsFactors = FALSE
    )

  # Test classes function for a dataframe
  column_classes <- classes(df)
  expect_equal(column_classes, c(a = "integer", b = "character"))

  # Test classes function for an object (a numeric vector)
  num_class <- classes(1:3)
  expect_equal(num_class, "integer")

  # Test classes function for a single factor object
  factor_obj <- factor(c("a", "b", "c"))
  factor_class <- classes(factor_obj)
  expect_equal(factor_class, "factor")
})

test_that("df_scale function works with data.frame as expected", {
  # Create a dataframe
  df <-
    data.frame(
      a = 1:5,
      b = c("one", "two", "three", "four", "five"),
      c = 11:15
    )

  # Test df_scale function
  scaled_df <- df_scale(df)
  expected_scaled_df <-
    data.frame(a = scale(df$a),
               b = df$b,
               c = scale(df$c))
  expect_equal(scaled_df, expected_scaled_df)

  # Test df_scale function without centering
  scaled_df_no_center <- df_scale(df, center = FALSE)
  expected_scaled_df_no_center <-
    data.frame(
      a = scale(df$a, center = FALSE),
      b = df$b,
      c = scale(df$c, center = FALSE)
    )
  expect_equal(scaled_df_no_center, expected_scaled_df_no_center)

  # Test df_scale function without scaling
  scaled_df_no_scale <- df_scale(df, scale = FALSE)
  expected_scaled_df_no_scale <-
    data.frame(
      a = scale(df$a, scale = FALSE),
      b = df$b,
      c = scale(df$c, scale = FALSE)
    )
  expect_equal(scaled_df_no_scale, expected_scaled_df_no_scale)
})

test_that("df_scale function works with tibble as expected", {
  # Create a tibble
  df <-
    tibble(
      a = 1:5,
      b = c("one", "two", "three", "four", "five"),
      c = 11:15
    )

  # Test df_scale function
  scaled_df <- df_scale(df)
  expected_scaled_df <-
    tibble(a = as.numeric(scale(df$a)),
           b = df$b,
           c = as.numeric(scale(df$c)))
  expect_equal(scaled_df, expected_scaled_df)

  # Test df_scale function without centering
  scaled_df_no_center <- df_scale(df, center = FALSE)
  expected_scaled_df_no_center <-
    tibble(a = as.numeric(scale(df$a, center = FALSE)),
           b = df$b,
           c = as.numeric(scale(df$c, center = FALSE)))
  expect_equal(scaled_df_no_center, expected_scaled_df_no_center)

  # Test df_scale function without scaling
  scaled_df_no_scale <- df_scale(df, scale = FALSE)
  expected_scaled_df_no_scale <-
    tibble(a = as.numeric(scale(df$a, scale = FALSE)),
           b = df$b,
           c = as.numeric(scale(df$c, scale = FALSE)))
  expect_equal(scaled_df_no_scale, expected_scaled_df_no_scale)
})
test_that("log_likelihood_norm function works as expected", {
  # Generate a normally distributed sample
  set.seed(42)
  x <- rnorm(100, mean = 5, sd = 2)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(dnorm(x, mean = 5, sd = 2, log = TRUE))

  # Calculate the estimated log-likelihood
  mean_est <- mean(x)
  sd_est <- sd(x)
  est_log_likelihood <- log_likelihood_norm(x, mean_est, sd_est)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood - true_log_likelihood) < 1)

  x <- rep(5, 100)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(dnorm(x, mean = 5, sd = 0, log = TRUE))

  # Calculate the estimated log-likelihood
  est_log_likelihood <- log_likelihood_norm(x, mean = 5, sd = 0)

  # Check if the estimated log-likelihood is equal to the true log-likelihood
  expect_equal(est_log_likelihood, true_log_likelihood)
  set.seed(42)
  x <- rnorm(100, mean = -10, sd = 5)

  # Calculate the true log-likelihood
  true_log_likelihood <-
    sum(dnorm(
      x,
      mean = -10,
      sd = 5,
      log = TRUE
    ))

  # Calculate the estimated log-likelihood
  mean_est <- mean(x)
  sd_est <- sd(x)
  est_log_likelihood <- log_likelihood_norm(x, mean_est, sd_est)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood - true_log_likelihood) < 1)
  set.seed(42)
  x <- rnorm(10, mean = 2, sd = 0.5)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(dnorm(x, mean = 2, sd = 0.5, log = TRUE))

  # Calculate the estimated log-likelihood
  mean_est <- 2
  sd_est <- 0.5
  est_log_likelihood <- log_likelihood_norm(x, mean_est, sd_est)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood - true_log_likelihood) == 0)

  set.seed(42)
  x <- rnorm(100, mean = 0, sd = 3)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(dnorm(x, mean = 0, sd = 3, log = TRUE))

  # Calculate the estimated log-likelihood
  mean_est <- mean(x)
  sd_est <- sd(x)
  est_log_likelihood <- log_likelihood_norm(x, mean_est, sd_est)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood - true_log_likelihood) < 1)

})

test_that("log_likelihood_unif function works as expected", {
  # Generate a sample from a uniform distribution
  set.seed(42)
  x <- runif(100, min = 2, max = 5)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(dunif(x, min = 2, max = 5, log = TRUE))

  # Calculate the estimated log-likelihood for the correct uniform distribution
  est_log_likelihood_correct <- log_likelihood_unif(x, min = 2, max = 5)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood_correct - true_log_likelihood) < 1)

  # Calculate the estimated log-likelihood for a different uniform distribution
  est_log_likelihood_different <- log_likelihood_unif(x, min = 0, max = 10)

  # Check if the log-likelihood for the different distribution is less than the correct one
  expect_true(est_log_likelihood_different < est_log_likelihood_correct)
})

test_that("log_likelihood_chisq function works as expected", {
  # Generate a sample from a chi-squared distribution
  set.seed(42)
  x <- rchisq(100, df = 5)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(dchisq(x, df = 5, log = TRUE))

  # Calculate the estimated log-likelihood for the correct chi-squared distribution
  est_log_likelihood_correct <- log_likelihood_chisq(x, df = 5)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood_correct - true_log_likelihood) < 1)

  # Calculate the estimated log-likelihood for a different chi-squared distribution
  est_log_likelihood_different <- log_likelihood_chisq(x, df = 10)

  # Check if the log-likelihood for the different distribution is less than the correct one
  expect_true(est_log_likelihood_different < est_log_likelihood_correct)
})

test_that("log_likelihood_f function works as expected", {
  # Generate a sample from an F distribution
  set.seed(42)
  x <- rf(100, df1 = 5, df2 = 10)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(df(x, df1 = 5, df2 = 10, log = TRUE))

  # Calculate the estimated log-likelihood for the correct F distribution
  est_log_likelihood_correct <- log_likelihood_f(x, df1 = 5, df2 = 10)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood_correct - true_log_likelihood) < 1)

  # Calculate the estimated log-likelihood for a different F distribution
  est_log_likelihood_different <- log_likelihood_f(x, df1 = 10, df2 = 5)

  # Check if the log-likelihood for the different distribution is less than the correct one
  expect_true(est_log_likelihood_different < est_log_likelihood_correct)
})

test_that("log_likelihood_t function works as expected", {
  # Generate a sample from a t distribution
  set.seed(42)
  x <- rt(100, df = 5)

  # Calculate the true log-likelihood
  true_log_likelihood <- sum(dt(x, df = 5, log = TRUE))

  # Calculate the estimated log-likelihood for the correct t distribution
  est_log_likelihood_correct <- log_likelihood_t(x, df = 5)

  # Check if the estimated log-likelihood is close enough to the true log-likelihood
  expect_true(abs(est_log_likelihood_correct - true_log_likelihood) < 1)

  # Calculate the estimated log-likelihood for a different t distribution
  est_log_likelihood_different <- log_likelihood_t(x, df = 10)

  # Check if the log-likelihood for the different distribution is less than the correct one
  expect_true(est_log_likelihood_different < est_log_likelihood_correct)
})

test_that("sensitivity function works as expected", {
  # Define predicted and true values
  pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  # Calculate sensitivity
  expected_sensitivity <- 2 / 3 # 2 true positives / (2 true positives + 1 false negative)

  # Check if the calculated sensitivity is equal to the expected value
  expect_equal(sensitivity(pred, truth), expected_sensitivity)

  # Test with another set of predicted and true values
  pred <- c(FALSE, TRUE, TRUE, FALSE, TRUE)
  truth <- c(TRUE, TRUE, TRUE, FALSE, FALSE)

  # Calculate sensitivity
  expected_sensitivity <- 2 / 3 # 2 true positives / (2 true positives + 1 false negative)

  # Check if the calculated sensitivity is equal to the expected value
  expect_equal(sensitivity(pred, truth), expected_sensitivity)

  # Test with edge cases
  pred <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  # Check if the calculated sensitivity is equal to 1 (all true positives)
  expect_equal(sensitivity(pred, truth), 1)

  pred <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  # Check if the calculated sensitivity is equal to 0 (all false negatives)
  expect_equal(sensitivity(pred, truth), 0)
})

test_that("specificity function works as expected", {
  # Define predicted and true values
  pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  # Calculate specificity
  expected_specificity <- 1 / 2 # 1 true negative / (1 true negative + 1 false negative)

  # Check if the calculated specificity is equal to the expected value
  expect_equal(specificity(pred, truth), expected_specificity)

  # Test with edge cases
  pred <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  # Check if the calculated specificity is equal to NaN (no true or false negatives)
  expect_true(is.nan(specificity(pred, truth)))

  pred <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  truth <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

  # Check if the calculated specificity is equal to 1 (all true negatives)
  expect_equal(specificity(pred, truth), 1)
})

test_that("precision function works as expected", {
  # Define predicted and true values
  pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  # Calculate precision
  expected_precision <- 2 / 3 # 2 true positives / (2 true positives + 1 false positive)

  # Check if the calculated precision is equal to the expected value
  expect_equal(precision(pred, truth), expected_precision)

  # Test with edge cases
  pred <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  # Check if the calculated precision is equal to 1 (all true positives)
  expect_equal(precision(pred, truth), 1)

  pred <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  truth <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

  # Check if the calculated precision is equal to NaN (no true or false positives)
  expect_true(is.nan(precision(pred, truth)))
})

test_that("recall function works as expected", {
  # Define predicted and true values
  pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  # Calculate recall
  expected_recall <- 2 / 3 # 2 true positives / (2 true positives + 1 false negative)

  # Check if the calculated recall is equal to the expected value
  expect_equal(recall(pred, truth), expected_recall)

  # Test with edge cases
  pred <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  # Check if the calculated recall is equal to 1 (all true positives)
  expect_equal(recall(pred, truth), 1)

  pred <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  truth <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

  # Check if the calculated recall is equal to NaN (no true positives or false negatives)
  expect_true(is.nan(recall(pred, truth)))
})

test_that("accuracy function works as expected", {
  # Define predicted and true values
  pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  # Calculate accuracy
  expected_accuracy <- 3 / 5 # 3 correct predictions / 5 total predictions

  # Check if the calculated accuracy is equal to the expected value
  expect_equal(accuracy(pred, truth), expected_accuracy)

  # Test with edge cases
  pred <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  # Check if the calculated accuracy is equal to 1 (all predictions correct)
  expect_equal(accuracy(pred, truth), 1)

  pred <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  truth <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

  # Check if the calculated accuracy is equal to 1 (all predictions correct)
  expect_equal(accuracy(pred, truth), 1)

  pred <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  truth <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

  # Check if the calculated accuracy is equal to 0 (no predictions correct)
  expect_equal(accuracy(pred, truth), 0)

  pred <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  # Check if the calculated accuracy is equal to 0 (no predictions correct)
  expect_equal(accuracy(pred, truth), 0)
})

test_that("f1 function works as expected", {
  # Test with 100% match
  pred <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  expect_equal(f1(pred, truth), 1)

  # Test with 0% match
  pred <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  truth <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(f1(pred, truth), 0)

  # Test with partial match
  pred <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  truth <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  expect_equal(f1(pred, truth), 0.8)

  # Test with different length inputs
  pred <- c(TRUE, FALSE, TRUE, FALSE)
  truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  expect_equal(f1(pred, truth), 0)

  # Test with all true positives and false negatives
  pred <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  truth <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  expect_equal(f1(pred, truth), 0.75)

  # Test with all false positives and true negatives
  pred <- c(FALSE, FALSE, FALSE, TRUE, TRUE)
  truth <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  expect_equal(f1(pred, truth), 0)

  # Test with numeric input
  pred <- c(1, 0, 1, 0, 1)
  truth <- c(1, 0, 1, 0, 1)
  expect_equal(f1(pred, truth), 1)
})


test_that("minimum_n_per_group function works as expected", {
  # Test with default power
  d <- 0.5
  power <- 0.8
  min_n <- minimum_n_per_group(d, power)
  result <- power.t.test(n = min_n, d = d, power = NULL, type = "two.sample")
  expect_true(result$power >= power)

  # Test with custom power
  d <- 0.5
  power <- 0.9
  min_n <- minimum_n_per_group(d, power)
  result <- power.t.test(n = min_n, d = d, power = NULL, type = "two.sample")
  expect_true(result$power >= power)

  # Test with edge cases
  d <- 0.1
  power <- 0.95
  min_n <- minimum_n_per_group(d, power)
  result <- power.t.test(n = min_n, d = d, power = NULL, type = "two.sample")
  expect_true(result$power >= power)

  d <- 1
  power <- 0.99
  min_n <- minimum_n_per_group(d, power)
  result <- power.t.test(n = min_n, d = d, power = NULL, type = "two.sample")
  expect_true(result$power >= power)
})

test_that("r2 function works as expected", {
  # Test perfect prediction
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(1, 2, 3, 4, 5)
  expect_equal(r2(pred, truth), 1)

  # Test no correlation
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(5, 4, 3, 2, 1)
  expect_true(abs(r2(pred, truth)) == 3)

  # Test partial correlation
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(1.5, 2.5, 3, 4, 5)
  expect_true(r2(pred, truth) > 0 && r2(pred, truth) < 1)

  # Test edge case
  pred <- c(1, 1, 1, 1, 1)
  truth <- c(1, 1, 1, 1, 1)
  expect_equal(r2(pred, truth), NaN)
})

test_that("adj_R2 function works as expected", {
  # Test 1: perfect prediction
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(1, 2, 3, 4, 5)
  n_p <- 1
  expect_equal(adj_R2(pred, truth, n_p), 1)

  # Test 4: all true values are the same
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(3, 3, 3, 3, 3)
  n_p <- 1
  expect_equal(adj_R2(pred, truth, n_p), -Inf)
})


test_that("adj_R2 function works as expected", {
  # Test 1: perfect prediction
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(1, 2, 3, 4, 5)
  n_p <- 1
  expect_equal(adj_R2(pred, truth, n_p), 1)

  # Test 2: perfect inverse prediction
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(5, 4, 3, 2, 1)
  n_p <- 1
  r_squared <- r2(pred, truth)
  n <- length(pred)
  expected_adj_r2 <- 1 - (1 - r_squared) * ((n - 1) / (n - n_p - 1))
  expect_equal(adj_R2(pred, truth, n_p), expected_adj_r2)

  # Test 3: random prediction
  pred <- c(1, 3, 2, 5, 4)
  truth <- c(2, 4, 1, 5, 3)
  n_p <- 2
  r_squared <- r2(pred, truth)
  n <- length(pred)
  expected_adj_r2 <- 1 - (1 - r_squared) * ((n - 1) / (n - n_p - 1))
  expect_equal(adj_R2(pred, truth, n_p), expected_adj_r2)

  # Test 4: all true values are the same
  pred <- c(1, 2, 3, 4, 5)
  truth <- c(3, 3, 3, 3, 3)
  n_p <- 1
  r_squared <- r2(pred, truth)
  n <- length(pred)
  expected_adj_r2 <- 1 - (1 - r_squared) * ((n - 1) / (n - n_p - 1))
  expect_equal(adj_R2(pred, truth, n_p), expected_adj_r2)
})

