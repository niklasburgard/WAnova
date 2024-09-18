#' @title welch_anova.mc
#'
#' @description Monte Carlo simulation to assess residual normality and homoscedasticity
#' across multiple groups by generating simulated data based on provided means,
#' standard deviations, and sample sizes.
#'
#' @param means Vector of means for each group.
#' @param sd Vector of standard deviations for each group.
#' @param n Vector of sample sizes for each group.
#' @param n_sim Number of simulations to perform. Defaults to 1000.
#' @param sim_func Function used for generating random samples. Defaults to `rnorm()` for normal distributions.
#' @param alpha Significance level for normality and homoscedasticity tests. Defaults to 0.05.
#' @param adj Logical, if TRUE applies continuity correction for proportions. Defaults to TRUE.
#'
#' @return A list of class "simres" containing the proportion of simulations where
#' residuals are normally distributed and homoscedastic.
#'
#' The default assumption is that random numbers are drawn from a normally distributed sample using `rnorm()`.
#' If your data follow a different distribution, you can provide a custom function through the `sim_func` parameter
#' The custom function should take three arguments: `n` (sample size), `mean`, and `sd` (standard deviation), and should return a vector of random values.
#' For example, to simulate data from a uniform distribution, you can pass:
#' `sim_func = function(n, mean, sd) { runif(n, min = mean - sd, max = mean + sd) }`.
#'
#' You can also apply a continuity correction `(r+1)/(N+1)` to the resulting proportions by setting `adj = TRUE` (default).
#' This correction improves the accuracy of estimates when proportions are small or close to 1. Without the correction,
#' the original proportion estimate is calculated as the ratio of simulations where the residuals meet the assumption of
#' normality or homoscedasticity to the total number of simulations, without adjustment.
#'
#' @examples
#' \donttest{
#' # Simulating normal distribution data
#' means <- c(50, 55, 60)
#' sds <- c(10, 12, 15)
#' n <- c(30, 35, 40)
#' result <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = 1000, alpha = 0.05)
#' print(result)
#'
#' # Simulating uniform distribution data
#' custom_sim_func <- function(n, mean, sd) {
#'   runif(n, min = mean - sd, max = mean + sd)
#' }
#' result_uniform <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = 1000, alpha = 0.05, sim_func = custom_sim_func)
#' print(result_uniform)
#'
#' # Simulating exponential distribution data
#' custom_exp_func <- function(n, mean, sd) {
#'   rate <- 1 / mean
#'   rexp(n, rate = rate)
#' }
#' result_exponential <- welch_anova.mc(means = means, sd = sds, n = n, n_sim = 1000, alpha = 0.05, sim_func = custom_exp_func)
#' print(result_exponential)
#'}
#'
#' @export
welch_anova.mc <- function(means, sd, n, n_sim = 1000, sim_func = NULL, alpha = 0.05, adj = TRUE) {

  shapiro_wilk_test <- function(residuals) {
    p_value <- stats::shapiro.test(unlist(residuals))$p.value
    return(p_value)
  }

  homoscedasticity_test <- function(residuals) {
    n_groups <- length(residuals)
    data_long <- data.frame(
      residual = unlist(residuals),
      group = factor(rep(1:n_groups, sapply(residuals, length)))
    )
    p_value <- car::leveneTest(residual ~ group, data=data_long)
    return(p_value$`Pr(>F)`[1])
  }

  if(is.null(sim_func)){
    sim_func <- function(n, mean, sd) {
      stats::rnorm(n = n, mean = mean, sd = sd)
    }
  }

  normality_p_values_vector <- numeric(n_sim)
  homoscedasticity_p_values_vector <- numeric(n_sim)

  for (i in 1:n_sim) {
    simulated_data_list <- lapply(1:length(means), function(j) {
      sim_func(n[j], means[j], sd[j])
    })

    simulated_data_list <- lapply(simulated_data_list, stats::na.omit)
    means_of_groups <- sapply(simulated_data_list, mean)
    residuals_list <- mapply(function(data, mean) data - mean, simulated_data_list, means_of_groups, SIMPLIFY = FALSE)

    normality_p_values_vector[i] <- all(shapiro_wilk_test(residuals_list) > alpha)
    homoscedasticity_p_values_vector[i] <- homoscedasticity_test(residuals_list)
  }

  norm_prop <- (sum(normality_p_values_vector > alpha) + 1) / (n_sim + 1)
  homosc_prop <- (sum(homoscedasticity_p_values_vector > alpha) + 1) / (n_sim + 1)

  if (adj == FALSE){
    norm_prop <- mean(normality_p_values_vector > alpha)
    homosc_prop <- mean(homoscedasticity_p_values_vector > alpha)
  }

  result <- list(
    norm_prop = norm_prop,
    homosc_prop = homosc_prop
  )

  class(result) <- c('simres', class(result))
  return(result)
}

#' @export
print.simres <- function(x, ...) {
  norm_prop <- x$norm_prop
  homosc_prop <- x$homosc_prop

  cat(sprintf("Proportion of simulations where p-value > alpha (suggesting normality): %.3f (%.2f%%)\n", norm_prop, norm_prop * 100))
  cat(sprintf("Proportion of simulations where p-value > alpha (suggesting homoscedasticity): %.3f (%.2f%%)\n", homosc_prop, homosc_prop * 100))
}
