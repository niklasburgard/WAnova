test_that("games_howell.test returns expected structure", {
  levels <- c("probe_a", "probe_b", "probe_c")
  n <- c(10, 9, 8)
  means <- c(43, 33.44444, 35.75)
  sd <- c(4.027682, 9.302031, 16.298554)

  result <- games_howell.test(levels, n, means, sd)

  expect_s3_class(result, "wAnova_gh")
  expect_type(result, "list")
  expect_true(all(c('Comparison', 'M.diff', 'SE', 'Low', 'Upp', 't', 'df', 'pval', 'signif') %in% colnames(result)))
  expect_equal(nrow(result), choose(length(levels), 2)) # Number of comparisons
})

test_that("games_howell.test returns correct calculations", {
  levels <- c("probe_a", "probe_b", "probe_c")
  n <- c(10, 9, 8)
  means <- c(43, 33.44444, 35.75)
  sd <- c(4.027682, 9.302031, 16.298554)

  result <- games_howell.test(levels, n, means, sd)

  # Check if the mean differences are correctly calculated
  expected_diff <- c(33.44444 - 43, 35.75 - 43, 35.75 - 33.44444)
  expect_equal(as.numeric(result$M.diff), round(expected_diff, 3))

  # Check if the p-values are within a reasonable range
  expect_true(all(result$pval >= 0 & result$pval <= 1))
})

test_that("games_howell.test handles edge cases", {
  # Test with two levels
  levels <- c("probe_a", "probe_b")
  n <- c(10, 9)
  means <- c(43, 33.44444)
  sd <- c(4.027682, 9.302031)

  result <- games_howell.test(levels, n, means, sd)
  expect_s3_class(result, "wAnova_gh")
  expect_equal(nrow(result), 1) # Should be only one comparison
})
