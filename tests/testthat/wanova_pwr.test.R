# Test for wanova_pwr.test function
test_that("wanova_pwr.test computes the correct sample size and power", {
  # Test input
  n <- c(10, 10, 10, 10)
  means <- c(1, 0, 0, -1)
  sd <- c(1, 1, 1, 1)

  # Expected values
  expected_n <- c(10, 10, 10, 10)  # Adjust based on the actual expected result
  expected_alpha <- 0.05
  expected_power <- 0.90

  result <- wanova_pwr.test(n, means, sd, power = expected_power, alpha = expected_alpha)

  # Test if the result is a list and contains the correct elements
  expect_type(result, "list")
  expect_true("n" %in% names(result))
  expect_true("alpha" %in% names(result))
  expect_true("power" %in% names(result))

  # Test the values
  expect_equal(result$n, expected_n)  # Adjust based on the actual expected result
  expect_equal(result$alpha, expected_alpha)
  expect_equal(result$power, expected_power, tolerance = 0.01)
})

# Test for print.wsize function
test_that("print.wsize outputs the correct format", {
  # Create a result object from wanova_pwr.test
  n <- c(10, 10, 10, 10)
  means <- c(1, 0, 0, -1)
  sd <- c(1, 1, 1, 1)
  result <- wanova_pwr.test(n, means, sd, power = 0.90, alpha = 0.05)

  # Capture output
  output <- capture.output(print(result))

  # Test for the presence of key phrases
  expect_true(any(grepl("Approximate sample size determinations for Welch’s F-test in one-way heteroscedastic ANOVA", output)))
  expect_true(any(grepl(sprintf("             n = %d", result$n[1]), output)))  # Adjust if result$n is different
  expect_true(any(grepl(sprintf("     sig.level = %.2f", result$alpha), output)))
  expect_true(any(grepl(sprintf("         power = %.2f", result$power), output)))
  expect_true(any(grepl("   alternative = two-sided", output)))
  expect_true(any(grepl(sprintf("NOTE: %s sample size for %d levels", if(length(result$n) == 1) "Equal" else "Different", length(result$n)), output)))
})
