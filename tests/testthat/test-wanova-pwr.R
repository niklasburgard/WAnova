test_that("wanova_pwr.test returns correct sample size and power", {
  # Define input values
  n <- c(10, 10, 10, 10)
  means <- c(1, 0, 0, -1)
  sd <- c(1, 1, 1, 1)
  target_power <- 0.90
  alpha <- 0.05

  # Run the function
  result <- wanova_pwr.test(n = n, means = means, sd = sd, power = target_power, alpha = alpha)

  # Check that the result is a list
  expect_type(result, "list")

  # Check that sample size is numeric and greater than the initial sample size
  expect_true(is.numeric(result$n))
  expect_gt(result$n, 10)

  # Check that the returned alpha is correct
  expect_equal(result$alpha, alpha)

  # Check that the returned power is approximately the target power
  expect_equal(result$power, target_power, tolerance = 0.01)
})

test_that("wanova_pwr.test handles unequal variances", {
  # Define input values with unequal standard deviations
  n <- c(10, 10, 10)
  means <- c(2, 1, 0)
  sd <- c(1, 2, 3)
  target_power <- 0.80
  alpha <- 0.05

  # Run the function
  result <- wanova_pwr.test(n = n, means = means, sd = sd, power = target_power, alpha = alpha)

  # Check that the result is a list
  expect_type(result, "list")

  # Check that the function handles unequal sample sizes correctly
  expect_true(length(result$n) == length(n))

  # Check that power is approximately the target power
  expect_equal(result$power, target_power, tolerance = 0.01)
})
