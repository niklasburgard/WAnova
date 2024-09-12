# tests/testthat/test-welch_anova_mc.R

library(testthat)
library(WAnova) # Assuming the package is named WAnova

test_that("welch_anova.mc computes correct proportions of normality and homoscedasticity", {
  means <- c(50, 55, 60)
  sds <- c(10, 12, 15)
  n <- c(30, 35, 40)
  result <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = 100)

  expect_s3_class(result, "simres")
  expect_true(is.numeric(result$norm_prop))
  expect_true(is.numeric(result$homosc_prop))
  expect_true(result$norm_prop >= 0 && result$norm_prop <= 1)
  expect_true(result$homosc_prop >= 0 && result$homosc_prop <= 1)
})

test_that("welch_anova.mc returns correct proportions when adj = FALSE", {
  means <- c(50, 55, 60)
  sds <- c(10, 12, 15)
  n <- c(30, 35, 40)
  result <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = 100, adj = FALSE)

  expect_s3_class(result, "simres")
  expect_true(is.numeric(result$norm_prop))
  expect_true(is.numeric(result$homosc_prop))
  expect_true(result$norm_prop >= 0 && result$norm_prop <= 1)
  expect_true(result$homosc_prop >= 0 && result$homosc_prop <= 1)
})

test_that("welch_anova.mc works with a custom simulation function", {
  means <- c(50, 55, 60)
  sds <- c(10, 12, 15)
  n <- c(30, 35, 40)
  custom_sim_func <- function(n, mean, sd) {
    rnorm(n = n, mean = mean, sd = sd)
  }
  result <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = 100, sim_func = custom_sim_func)

  expect_s3_class(result, "simres")
  expect_true(is.numeric(result$norm_prop))
  expect_true(is.numeric(result$homosc_prop))
  expect_true(result$norm_prop >= 0 && result$norm_prop <= 1)
  expect_true(result$homosc_prop >= 0 && result$homosc_prop <= 1)
})
