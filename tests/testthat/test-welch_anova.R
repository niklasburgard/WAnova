# Sample data for testing
levels <- c("probe_a", "probe_b", "probe_c")
n <- c(10, 9, 8)
means <- c(43.00000, 33.44444, 35.75000)
sd <- c(4.027682, 9.302031, 16.298554)

# Tests for the welch_anova.test function
test_that("welch_anova.test returns expected structure", {
  result <- welch_anova.test(levels, n, means, sd)

  # Use expect_s3_class instead of expect_is
  expect_s3_class(result, "wAnova")
  expect_true(is.list(result))
  expect_named(result, c("variables", "response", "n", "k", "f_value", "df_between", "df_within", "p_value", "omega_sq"))
})

test_that("welch_anova.test calculates correct values", {
  result <- welch_anova.test(levels, n, means, sd)

  # Check the number of groups
  expect_equal(result$k, length(levels))

  # Check that df_between is calculated correctly
  expect_equal(result$df_between, length(levels) - 1)

  # Check the F-value, degrees of freedom, and p-value
  expect_true(result$f_value > 0)
  expect_true(result$df_within > 0)
  expect_true(result$p_value > 0 & result$p_value <= 1)

  # Test different effect size calculations
  result_AnL <- welch_anova.test(levels, n, means, sd, effsize = "AnL")
  result_kirk <- welch_anova.test(levels, n, means, sd, effsize = "Kirk")
  result_can <- welch_anova.test(levels, n, means, sd, effsize = "CaN")

  # Ensure omega_sq is different for different methods
  expect_true(result_AnL$omega_sq != result_kirk$omega_sq)
  expect_true(result_AnL$omega_sq != result_can$omega_sq)
})

test_that("summary.wAnova prints expected output", {
  result <- welch_anova.test(levels, n, means, sd)

  # Capture the printed output
  summary_output <- capture.output(summary(result))

  # Check that specific lines are present
  expect_true(any(grepl("One-way fixed effects Welch ANOVA", summary_output)))
  expect_true(any(grepl("F value", summary_output)))
  expect_true(any(grepl("Adj. omega squared est.:", summary_output)))

  # Check that the p-value is printed correctly
  expect_true(any(grepl(sprintf("%.6f", result$p_value), summary_output)))
})
