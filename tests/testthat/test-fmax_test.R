test_that("fmax_test calculates correctly for equal variances", {
  levels <- c("Group1", "Group2", "Group3")
  n <- c(10, 10, 10)  # Equal sample sizes
  sd <- c(4.0, 4.0, 4.0)  # Equal standard deviations

  result <- fmax_test(levels = levels, n = n, sd = sd)

  # Calculate expected values manually
  df_expected <- mean(n) - 1
  k_expected <- length(levels)
  fmax_expected <- max(sd^2) / min(sd^2)
  pval_expected <- SuppDists::pmaxFratio(fmax_expected, df_expected, k_expected, lower.tail = FALSE)

  expect_equal(result$fmax, fmax_expected, tolerance = 1e-6)
  expect_equal(result$df, df_expected)
  expect_equal(result$k, k_expected)
  expect_equal(result$pval, pval_expected, tolerance = 1e-6)
})
