# Test for the `wanova_pwr.test` function
test_that("wanova_pwr.test calculates sample size correctly", {
  # Example 1: Simple test case
  n <- c(10, 10, 10, 10)
  means <- c(1, 0, 0, -1)
  sd <- c(1, 1, 1, 1)
  result <- wanova_pwr.test(n, means, sd, power = 0.90, alpha = 0.05)

  expect_is(result, "wsize")
  expect_named(result, c("n", "alpha", "power"))
  expect_equal(length(result$n), length(n))
  expect_equal(result$alpha, 0.05)
  expect_true(result$power >= 0.90)

  # Check if the sample size is a single value when all sizes are the same
  expect_true(length(unique(result$n)) == 1)

  # Example 2: Edge case with different initial sample sizes
  n <- c(5, 10, 15, 20)
  means <- c(2, 1, 0, -1)
  sd <- c(2, 1.5, 1, 0.5)
  result <- wanova_pwr.test(n, means, sd, power = 0.80, alpha = 0.01)

  expect_is(result, "wsize")
  expect_named(result, c("n", "alpha", "power"))
  expect_equal(result$alpha, 0.01)
  expect_true(result$power >= 0.80)

  # Check if the sample size varies
  expect_true(length(unique(result$n)) > 1)

  # Example 3: Test with default parameters
  result <- wanova_pwr.test(n = c(10, 10, 10), means = c(1, 0, 0), sd = c(1, 1, 1))

  expect_is(result, "wsize")
  expect_named(result, c("n", "alpha", "power"))
  expect_equal(result$alpha, 0.05) # Default alpha
  expect_true(result$power >= 0.90) # Default power

  # Check if the sample size is a single value when all sizes are the same
  expect_true(length(unique(result$n)) == 1)
})

# Test for the `print.wsize` function
test_that("print.wsize prints results correctly", {
  # Capture the output of the print function
  n <- c(10, 10, 10)
  means <- c(1, 0, 0)
  sd <- c(1, 1, 1)
  result <- wanova_pwr.test(n, means, sd, power = 0.90, alpha = 0.05)

  output <- capture.output(print(result))

  expect_true(grepl("Approximate sample size determinations for Welch’s F-test", output[1]))
  expect_true(grepl(sprintf("             n = %d", result$n), output[2]))
  expect_true(grepl(sprintf("     sig.level = %.2f", result$alpha), output[3]))
  expect_true(grepl(sprintf("         power = %.2f", result$power), output[4]))
  expect_true(grepl("NOTE: Same sample size for k-level groups", output[5]))
})
