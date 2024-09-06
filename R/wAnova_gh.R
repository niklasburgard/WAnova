#' @title games_howell.test
#'
#' @description Games-Howell post hoc test for multiple comparisons after Welch's ANOVA.
#'
#' @param levels A vector containing the level names of the independent variable.
#' @param n A vector containing the sample sizes for each level.
#' @param means A vector containing the sample means for each level.
#' @param sd A vector containing the sample standard deviations for each level.
#' @param conf.level The confidence level for the interval. Defaults to 0.95.
#'
#' @return A data frame of class "wAnova_gh" containing the pairwise comparisons with mean differences,
#' standard errors, confidence intervals, t-values, degrees of freedom, and p-values.
#' Significance codes are also included.
#'
#' @examples
#' \donttest{
#' probe_data <- data.frame(
#'   group = c("probe_a", "probe_b", "probe_c"),
#'   size = c(10, 9, 8),
#'   mean = c(43.00000, 33.44444, 35.75000),
#'   sd = c(4.027682, 9.302031, 16.298554)
#' )
#' result <- games_howell.test(probe_data$group, probe_data$size, probe_data$mean, probe_data$sd)
#' print(result)
#' }
#'
#' @export
games_howell.test <- function(levels, n, means, sd, conf.level = 0.95) {
  unique_levels <- unique(levels)
  combs <- utils::combn(unique_levels, 2)
  var <- stats::setNames(sd^2, levels)
  n <- stats::setNames(as.numeric(n), levels)
  means <- stats::setNames(as.numeric(means), levels)

  statistics <- t(apply(combs, 2, function(comb) {
    m_diff <- means[comb[2]] - means[comb[1]]
    var1 <- var[comb[1]]
    var2 <- var[comb[2]]
    n1 <- n[comb[1]]
    n2 <- n[comb[2]]

    se <- sqrt(0.5 * (var1 / n1 + var2 / n2))
    t <- abs(m_diff) / se
    df <- ((var1 / n1) + (var2 / n2))^2 /
      (((var1 / n1)^2 / (n1 - 1)) + ((var2 / n2)^2 / (n2 - 1)))
    pval <- stats::ptukey(t * sqrt(2), length(unique_levels), df, lower.tail = FALSE)
    conf_margin <- stats::qtukey(conf.level, length(unique_levels), df) * se
    lower <- m_diff - conf_margin
    upper <- m_diff + conf_margin

    c(paste(comb[1], ":", comb[2]), m_diff, se, lower, upper, t, df, pval)
  }))

  result <- as.data.frame(statistics, stringsAsFactors = FALSE)
  colnames(result) <- c('Comparison', 'M.diff', 'SE', 'Low', 'Upp', 't', 'df', 'pval')
  result[c(2:8)] <- lapply(result[c(2:8)], function(x) round(as.numeric(x), 3))

  result$signif <- cut(result$pval,
                       breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                       labels = c("***", "**", "*", ".", ""),
                       right = FALSE)

  class(result) <- c('wAnova_gh', class(result))
  return(result)
}

#' @export
print.wAnova_gh <- function(x, ...) {
  cat("Games-Howell Post Hoc Test for Multiple Comparisons\n\n")

  widths <- sapply(x, function(col) max(nchar(as.character(col))))
  col_names <- sprintf(paste0("%-", widths, "s"), colnames(x))
  cat(paste(col_names, collapse = "\t"), "\n")

  formatted_rows <- apply(x, 1, function(row) {
    formatted_row <- sprintf(paste0("%-", widths, "s"), row)
    paste(formatted_row, collapse = "\t")
  })

  cat(formatted_rows, sep = "\n")
}
