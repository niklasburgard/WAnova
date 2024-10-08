% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wAnova.R
\name{welch_anova.test}
\alias{welch_anova.test}
\title{welch_anova.test}
\usage{
welch_anova.test(levels, n, means, sd, effsize = "AnL")
}
\arguments{
\item{levels}{Vector with level names of the independent variable}

\item{n}{Vector with sample size for each level}

\item{means}{Vector with sample mean for each level}

\item{sd}{Vector with sample standard deviation for each level}

\item{effsize}{Options "AnL" (standard), "Kirk", "CaN" (see below)}
}
\value{
A list of class "wAnova" that contains the F-value, df-between, df-within,
p-value and effect size omega-squared that can later be converted to a
table-styled summary using summary()

The adjusted omega squared estimator of the effect size is either calculated by the formula
of of Albers and Lakens, Kirk or Caroll and Nordholm (Further notes see README file)

Albers, C., & Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of Experimental Social Psychology, 74, 187–195.
Kirk, R. E. (1996). Practical significance: A concept whose time has come. Educational and Psychological Measurement, 56(5), 746-759.
Carroll, R. M., & Nordholm, L. A. (1975). Sampling characteristics of Kelley's epsilon and Hays' omega Educational and Psychological Measurement, 35(3), 541-554.
}
\description{
Welch's one-way ANOVA with fixed effects (between subjects analysis)
from summary statistics with unequal (unknown) variances.
}
\examples{
\donttest{
probe_data <- data.frame(
  group = c("probe_a", "probe_b", "probe_c"),
  size = c(10, 9, 8),
  mean = c(43.00000, 33.44444, 35.75000),
  sd = c(4.027682, 9.302031, 16.298554)
)
result <- welch_anova.test(
  levels = probe_data$group,
  n = probe_data$size,
  means = probe_data$mean,
  sd = probe_data$sd,
  effsize = "Kirk"
)
summary(result)
}

}
