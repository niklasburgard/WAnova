#' @title fmax.test
#'
#' @description
#' Performs Hartley's Fmax test for homogeneity of variances. This test assesses whether
#' the variances of multiple groups are equal, given that the sample sizes are the same.
#' It calculates the ratio of the maximum to the minimum variance and compares it to a
#' critical value from the Fmax distribution to determine if there is significant
#' deviation from homogeneity.
#'
#' @param levels a vector of group labels or identifiers.
#' @param n A numeric vector of sample sizes for each group.
#' @param sd A numeric vector of standard deviations for each group.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{fmax}{The ratio of the maximum to minimum variance.}
#'   \item{df}{Degrees of freedom associated with the test.}
#'   \item{k}{Number of groups.}
#'   \item{pval}{The p-value of the test.}
#' }
#'
#'
#' The levels parameter is used determine the number of groups. The variances are computed as the squares of sd.
#' Note: Applicable results assume normally distributed data with equal sample sizes
#'
#' Null Hypothesis: Assumes homogeneity of variances, which means all groups have the same variance.
#' Alternative Hypothesis: Assumes that not all group variances are equal. This hypothesis is
#' supported if the p-value is below the significance level.
#'
#' @examples
#' \donttest{
#' probe_data <- data.frame(
#'   group = c("probe_a", "probe_b", "probe_c"),
#'   size = c(10, 10, 10),  # Equal sample sizes
#'   mean = c(43.00000, 33.44444, 35.75000),
#'   sd = c(4.027682, 9.302031, 16.298554)
#' )
#'
#' # Perform Hartley's Fmax test
#' result <- fmax_test(
#'   levels = probe_data$group,
#'   n = probe_data$size,
#'   sd = probe_data$sd
#' )
#' }
#'
#' @export
fmax_test <- function(levels,n,sd){
  if (any(n != mean(n))) {
    warning("Hartley's Fmax-test assumes euqal sample sizes.")
  }
  df <- mean(n)-1
  k <- length(levels)
  fmax <- max(sd^2)/min(sd^2)
  pval <- SuppDists::pmaxFratio(fmax, df, k, lower.tail = FALSE)

  result <- list(
    fmax = fmax,
    df = df,
    k = k,
    pval = pval
  )

  class(result) <- c('fmax_test', class(result))
  return(result)
}

#' @export
print.fmax_test <- function(x, ...) {
  fmax <- x$fmax
  df <- x$df
  k <- x$k
  pval <- x$pval

  sig_code <- cut(pval,
                  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                  labels = c("***", "**", "*", ".", " "),
                  right = FALSE)

  cat("Hartley's Fmax Test\n\n")
  cat(sprintf(" %-5s %-5s %-5s %-8s %-6s\n", "Fmax", "df", "k", "p-value", "Sig. Code"))
  cat(sprintf(" %-5.2f %-5.2f %-5.2f %-8.6f %s\n\n", fmax, df, k, pval, sig_code))
  cat("\n---\n")
  cat("Significance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat("Note: Results applicable to normally distributed data with equal sample sizes.\n")
}
