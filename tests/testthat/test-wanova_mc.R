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

test_that("welch_anova.mc works for different distributions", {
  # Define test parameters
  means <- c(50, 55, 60)
  sds <- c(10, 12, 15)
  n <- c(30, 35, 40)
  alpha <- 0.05
  n_sim <- 1000

  # Test for uniform distribution
  custom_uniform_func <- function(n, mean, sd) {
    runif(n, min = mean - sd, max = mean + sd)
  }
  result_uniform <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = n_sim, alpha = alpha, sim_func = custom_uniform_func)
  expect_is(result_uniform, "simres")
  expect_true(result_uniform$norm_prop >= 0 && result_uniform$norm_prop <= 1)
  expect_true(result_uniform$homosc_prop >= 0 && result_uniform$homosc_prop <= 1)

  # Test for exponential distribution
  custom_exp_func <- function(n, mean, sd) {
    rate <- 1 / mean
    rexp(n, rate = rate)
  }
  result_exponential <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = n_sim, alpha = alpha, sim_func = custom_exp_func)
  expect_is(result_exponential, "simres")
  expect_true(result_exponential$norm_prop >= 0 && result_exponential$norm_prop <= 1)
  expect_true(result_exponential$homosc_prop >= 0 && result_exponential$homosc_prop <= 1)
})
