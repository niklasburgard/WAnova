#' @title games_howell.test
#'
#' @description Games-Howell post hoc test for multiple comparison after Welch's Anova
#'
#'
#' @param object Vector with level names of the independent variable
#' @param n Vector with sample size for each level
#' @param means Vector with sample mean for each level
#' @param sd Vector with sample standard deviation for each level
#' @param conf.level Optional (standard set to 0.95)
#'
#' @return A dataframe of class "wAnova_gh" that contains the pairwise comparison with mean differences,
#' standard error, confidence interval, t-value, df, p-value
#'
#' @examples
#' probe_data <- data.frame(
#' group = c("probe_a", "probe_b", "probe_c"),
#' size = c(10, 9, 8),
#' mean = c(43.00000, 33.44444, 35.75000),
#' sd = c(4.027682, 9.302031, 16.298554)
#' )
#' result <- games_howell.test(probe_data$group, probe_data$size, probe_data$mean, probe_data$sd)
#' result
#' #data.frame(result)
#'
#' @export
games_howell.test <- function(levels, n, means, sd, conf.level = 0.95) {
  combs        = combn(unique(levels), 2)
  var          = sd^2
  names(var)   = levels
  names(n)     = levels
  names(means) = levels

  statistics <- t(apply(combs, 2, function(comb){
    m.diff = means[comb[2]] - means[comb[1]]
    var1   = var[comb[1]]
    var2   = var[comb[2]]
    n1     = n[comb[1]]
    n2     = n[comb[2]]

    t    <- abs(m.diff) / sqrt((var1 / n1) + (var2 / n2))
    df   <- ((var1 / n1) + (var2 / n2))^2 /(((var1 / n1)^2 / (n1 - 1)) + ((var2 / n2)^2 / (n2 - 1)))
    p_value <- ptukey(t * sqrt(2), length(unique(levels)), df, lower.tail = FALSE)
    se   <- sqrt(0.5 * (var1 / n1 + var2 / n2))
    upp  <- m.diff + qtukey(conf.level, length(unique(levels)), df) * se
    low  <- m.diff - qtukey(conf.level, length(unique(levels)), df) * se

    c(paste(comb[1], ":", comb[2]), m.diff, se, low, upp, t, df, p_value)
  }))

  result <- as.data.frame(statistics, stringsAsFactors = FALSE)
  colnames(result) <- c('Comparison', 'M.diff', 'SE', 'Low', 'Upp', 't', 'df', 'pval')
  result[c(2, 3:ncol(result))] <- lapply(result[c(2, 3:ncol(result))], function(x) round(as.numeric(x), 3))
  result$signif <- cut(result$pval,
                       breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                       labels = c("***", "**", "*", ".", ""))
  class(result) <- c('wAnova_gh', class(result))
  return(result)
}

#' @export
print.wAnova_gh <- function(obj, ...) {
  cat("Games-Howell Post Hoc Test for Multiple Comparisons\n\n")
  widths <- sapply(obj, function(col) max(nchar(as.character(col))))

  col_names <- sprintf(paste0("%-", widths, "s"), colnames(obj))
  cat(paste(col_names, collapse = "\t"), "\n")

  formatted_rows <- apply(obj, 1, function(row) {
    formatted_row <- sprintf(paste0("%-", widths, "s"), row)
    paste(formatted_row, collapse = "\t")
  })

  cat(formatted_rows, sep = "\n")
  NextMethod("print")
}
