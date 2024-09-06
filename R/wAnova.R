#' @title welch_anova.test
#'
#' @description Welch's one-way ANOVA with fixed effects (between subjects analysis)
#' from summary statistics with unequal (unknown) variances.
#'
#'
#' @param levels Vector with level names of the independent variable
#' @param n Vector with sample size for each level
#' @param means Vector with sample mean for each level
#' @param sd Vector with sample standard deviation for each level
#' @param effsize Options "AnL" (standard), "Kirk", "CaN" (see below)
#'
#' @return A list of class "wAnova" that contains the F-value, df-between, df-within,
#' p-value and effect size omega-squared that can later be converted to a
#' table-styled summary using summary()
#'
#' The adjusted omega squared estimator of the effect size is either calculated by the formula
#' of of Albers and Lakens, Kirk or Caroll and Nordholm (Further notes see README file)
#'
#' Albers, C., & Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of Experimental Social Psychology, 74, 187–195.
#' Kirk, R. E. (1996). Practical significance: A concept whose time has come. Educational and Psychological Measurement, 56(5), 746-759.
#' Carroll, R. M., & Nordholm, L. A. (1975). Sampling characteristics of Kelley's epsilon and Hays' omega Educational and Psychological Measurement, 35(3), 541-554.
#'
#' @examples
#' \donttest{
#' probe_data <- data.frame(
#'   group = c("probe_a", "probe_b", "probe_c"),
#'   size = c(10, 9, 8),
#'   mean = c(43.00000, 33.44444, 35.75000),
#'   sd = c(4.027682, 9.302031, 16.298554)
#' )
#' result <- welch_anova.test(
#'   levels = probe_data$group,
#'   n = probe_data$size,
#'   means = probe_data$mean,
#'   sd = probe_data$sd,
#'   effsize = "Kirk"
#' )
#' summary(result)
#'}
#'
#' @export
welch_anova.test <- function(levels, n, means, sd, effsize = "AnL") {
  n <- as.numeric(n)
  response = deparse(substitute(means))
  means <- as.numeric(means)
  sd <- as.numeric(sd)

  k <- length(levels)
  var <- sd^2
  wt <- n / var
  wt_gm <- sum(wt * means) / sum(wt)

  adj_ess <- sum(wt * (means - wt_gm)^2)
  adj_ms <- adj_ess/(k-1)
  lambda <- sum((1 - wt / sum(wt))^2 / (n - 1))

  f_stat <- adj_ms / (1 + 2 * lambda * (k - 2) / (k^2 - 1))
  df_between <- k - 1
  df_within <- (k^2 - 1) / (3 * lambda)
  p_value <- stats::pf(f_stat, df_between, df_within, lower.tail = FALSE)

  omega_sq <- switch(effsize,
                     "CaN" = (f_stat - 1) / (((sum(n) - k + 1) / (k - 1)) + f_stat),
                     "Kirk" = ((f_stat - 1) * df_between) / ((df_between * (f_stat - 1)) + sum(n)),
                     "AnL" = (f_stat - 1) / (((df_within + 1) / df_between) + f_stat)
  )


  result <- list(
    variables  = deparse(substitute(levels)),
    response   = response,
    n          = n,
    k          = k,
    f_value    = f_stat,
    df_between = df_between,
    df_within  = df_within,
    p_value    = p_value,
    omega_sq   = omega_sq)

  class(result) <- c('wAnova', class(result))
  return(result)
}

#' @export
summary.wAnova <- function(object, ...) {
  response <- object$response
  variables <- object$variables
  n <- sum(object$n)
  k <- object$k
  f_value <- object$f_value
  df_between <- object$df_between
  df_within <- object$df_within
  p_value <- object$p_value
  omega_sq <- object$omega_sq

  sig_code <- cut(p_value,
                  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                  labels = c("***", "**", "*", ".", " "),
                  right = FALSE)

  cat("One-way fixed effects Welch ANOVA (between subjects)\n\n")
  cat(sprintf(" Data: %s and %s\n\n", response, variables))
  cat(sprintf(" %-8s %-4s %-7s %-9s\n", "F value", "df1", "df2", "p-value"))
  cat(sprintf(" %-8.2f %-4d %-7.2f %-9.6f %s\n\n", f_value, df_between, df_within, p_value, sig_code))
  cat(sprintf(" Adj. omega squared est.: %.2f\n---\n", omega_sq))
  cat("Significance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
}
