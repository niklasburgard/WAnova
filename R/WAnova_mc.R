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
#' @param sim_func Function used for generating random samples. Defaults to rnorm().
#' @param alpha Significance level for normality and homoscedasticity tests. Defaults to 0.05.
#' @param adj Logical, if TRUE applies continuity correction for proportions. Defaults to TRUE.
#'
#' @return A list of class "simres" containing the proportion of simulations where
#' residuals are normally distributed and homoscedastic.
#'
#' @examples
#' \donttest{
#' means <- c(50, 55, 60)
#' sds <- c(10, 12, 15)
#' n <- c(30, 35, 40)
#' result <- mc_residuals(means = means, sds = sds, n = n, n_sim = 1000, alpha = 0.05)
#' print(result)
#' }
#'
#' @export
welch_anova.mc <- function(means, sd, n, n_sim = 1000, sim_func = NULL, alpha = 0.05, adj = TRUE) {

  shapiro_wilk_test <- function(data) {
    residuals <- sweep(data, 2, colMeans(data))
    combined_residuals <- as.vector(residuals)
    p_values <- shapiro.test(combined_residuals)$p.value
    return(p_values)
  }

  homoscedasticity_test <- function(data) {
    n_groups <- ncol(data)
    data_long <- data.frame(
      value = as.vector(data),
      group = factor(rep(1:n_groups, each = nrow(data)))
    )
    test_result <- leveneTest(value ~ group, data=data_long)
    return(test_result$`Pr(>F)`[1])
  }

  if(is.null(sim_func)){
    sim_func <- function(n, mean, sd) {
      rnorm(n = n, mean = mean, sd = sd)
    }
  }

  normality_p_values_vector <- numeric(n_sim)
  homoscedasticity_p_values_vector <- numeric(n_sim)

  for (i in 1:n_sim) {
    simulated_data <- sapply(1:length(means), function(j) {
      sim_func(n[j], means[j], sd[j])
    })

    normality_p_values_vector[i] <- shapiro_wilk_test(simulated_data)
    homoscedasticity_p_values_vector[i] <- homoscedasticity_test(simulated_data)
  }

  r_norm_prop <- sum(normality_p_values_vector > alpha)
  n_norm_prop <- length(normality_p_values_vector)
  r_homosc_prop <- sum(homoscedasticity_p_values_vector > alpha)
  n_homosc_prop <- length(homoscedasticity_p_values_vector)
  norm_prop <- (r_norm_prop+1)/(n_norm_prop+1)
  homosc_prop <- (r_homosc_prop+1)/(n_homosc_prop+1)

  if (adj == FALSE){
    norm_prop <- mean(normality_p_values_vector > alpha)
    homosc_prop <- mean(homoscedasticity_p_values_vector > alpha)
  }

  result <- list(
    norm_prop = norm_prop,
    homosc_prop = homosc_prop)

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
