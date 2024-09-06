#' @title wanova_pwr.test
#'
#' @description This function computes the approximate sample size required to achieve a desired power level for Welch's F-test in a one-way heteroscedastic ANOVA. The function takes the initial sample size, group means, and standard deviations, and iteratively determines the necessary sample size to meet the specified power and significance level.
#'
#' @param n A numeric vector of initial sample sizes for each group.
#' @param means A numeric vector of the means for each group.
#' @param sd A numeric vector of the standard deviations for each group.
#' @param power Desired power of the test. Default is 0.90.
#' @param alpha Significance level for the test. Default is 0.05.
#'
#' @return A list containing:
#' \item{n}{The adjusted sample sizes for each group. If all adjusted sizes are the same, it returns a single value.}
#' \item{alpha}{The significance level used in the computation.}
#' \item{power}{The calculated power of the test, rounded to two decimal places.}
#'
#' @details The function adjusts the sample sizes iteratively until the desired power level is achieved.
#' It uses the F-distribution to determine critical values and compute the power of the test.
#' The output includes the sample size required for each group to achieve the specified power.
#'
#' Reference: Levy, K. J. (1978a). Some empirical power results associated with
#' Welch's robust analysis of variance technique. Journal of Statistical Computation and Simulation, 8, 43-48.
#'
#' @examples
#' \dontrun{
#' # Example usage of the wsize function
#' n <- c(10, 10, 10, 10)
#' means <- c(1, 0, 0, -1)
#' sd <- c(1, 1, 1, 1)
#' result <- wanova_pwr.test(n, means, sd, power = 0.90, alpha = 0.05)
#' print(result)
#'}
#'
#' @seealso \code{\link{print.wsize}}
#'
#' @export
wanova_pwr.test <- function(n, means, sd, power = 0.90, alpha = 0.05) {
  k <- length(means)
  var <- sd^2
  n_ratio <- n / n[1]
  df_between <- k - 1
  n_init <- 5
  apower <- 0

  while (apower < power) {
    n_init <- n_init + 1
    n <- n_init * n_ratio
    wt <- n / var
    wt_gm <- sum(wt * means) / sum(wt)
    ess_adj <- sum(wt * (means - wt_gm)^2)
    lambda <- sum(((1 - wt / sum(wt))^2) / (n - 1))
    df_within <- (k^2 - 1) / (3 * lambda)
    fcrit <- stats::qf(1 - alpha, df_between, df_within)
    apower <- 1 - stats::pf(fcrit, df_between, df_within, ess_adj)
  }

  if (length(unique(n)) == 1) {
    n <- unique(n)
  }

  result <- list(
    k       = k,
    n       = n,
    alpha   = alpha,
    power   = round(apower, 2)
  )

  class(result) <- c('wsize', class(result))

  return(result)
}

#' Print Method for `wsize` Objects
#'
#' Custom print method for objects of class `wsize`.
#' Displays the results of the sample size determination in a user-friendly format.
#'
#' @param x An object of class \code{wsize}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return Prints the sample size determination results to the console.
#'
#' @examples
#' \dontrun{
#' # Print the results from the wsize function
#' n <- c(10, 10, 10, 10)
#' means <- c(1, 0, 0, -1)
#' sd <- c(1, 1, 1, 1)
#' result <- wanova_pwr.test(n, means, sd, power = 0.90, alpha = 0.05)
#' print(result)
#'}
#'
#' @export
print.wsize <- function(x, ...) {
  k <- x$k
  n <- x$n
  alpha <- x$alpha
  power <- x$power

  cat("\nApproximate sample size determinations for Welch's F-test in one-way heteroscedastic ANOVA\n")
  cat("\n")
  cat(sprintf("             n = %d\n", n))
  cat(sprintf("     sig.level = %.2f\n", alpha))
  cat(sprintf("         power = %.2f\n", power))
  cat("   alternative = two-sided")
  if (length(n) == 1) {
    cat("\n---\nNOTE: Equal sample size for %d levels\n", k)
  } else {
    cat("\n---\nNOTE: Different sample sizes for %d levels\n", length(n))
  }
}
