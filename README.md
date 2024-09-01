# WAnova

'WAnova' is an R package that provides functions for conducting Welch's ANOVA and Games-Howell post hoc tests based on summary statistics. These tests are particularly useful when working with unequal variances and sample sizes.

## Installation

You can install the development version of 'WAnova' from GitHub with:
```
# If you don't have devtools installed, first install it:
install.packages("devtools")

# Then, install the WAnova package from GitHub:
devtools::install_github("niklasburgard/WAnova")
```

## Features

+ ***Welch's ANOVA***: Perform Welch's ANOVA based on summary statistics (mean, standard deviation, sample size).
+ ***Games-Howell Post Hoc Test***: Conduct the Games-Howell post hoc test for multiple comparisons after Welch's ANOVA.
+ ***Effect Size Calculations:***: Calculate adjusted omega squared effect sizes for Welch's ANOVA using different methods (Hays, Kirk, Carroll & Nordholm).

## Usage

### Parameters

welch_anova.test(levels, n, means, sd, effsize = "Hays")
games_howell.test(levels, n, means, sd, conf.level = 0.95)

***levels*** Vector with level names of the independent variable
***n***      Vector with sample size for each level
***means***  Vector with sample mean for each level
***sd***     Vector with sample standard deviation for each level
***effsize** Options "Hays", "Kirk", "CaN"
***conf.level*** The confidence level for the interval

References:
Hays, W. L. (1973). Statistics for the social sciences (2nd ed.). Holt, Rinehart and Winston.
Kirk, R. E. (1996). Practical significance: A concept whose time has come. Educational and Psychological Measurement, 56(5), 746-759.
Carroll, R. M., & Nordholm, L. A. (1975). Sampling characteristics of Kelley's epsilon and Hays' omega Educational and Psychological Measurement, 35(3), 541-554.

### Example: Welch's ANOVA
```
library(WAnova)

# Example data
probe_data <- data.frame(
  group = c("probe_a", "probe_b", "probe_c"),
  size  = c(10, 9, 8),
  mean  = c(43.00000, 33.44444, 35.75000),
  sd    = c(4.027682, 9.302031, 16.298554)
)

# Perform Welch's ANOVA
result <- welch_anova.test(
  levels  = probe_data$group,
  n       = probe_data$size,
  means   = probe_data$mean,
  sd      = probe_data$sd,
  effsize = "Kirk"
)

# Print summary
summary(result)
```

### Example: Games-Howell Post Hoc Test
```
library(WAnova)

# Conduct Games-Howell post hoc test
posthoc_result <- games_howell.test(
  levels = probe_data$group,
  n      = probe_data$size,
  means  = probe_data$mean,
  sd     = probe_data$sd
)

# Print results
print(posthoc_result)
```

### Citing WAnova

To cite the WAnova package in publications, please use:
```
Niklas A. Burgard (2023). WAnova: Welch's Anova from Summary Statistics. R package version 0.1.1. https://CRAN.R-project.org/package=WAnova
```
You can also find a BibTeX entry for LaTeX users:
```
@Manual{,
  title = {WAnova: Welch's Anova from Summary Statistics},
  author = {Niklas A. Burgard},
  year = {2023},
  note = {R package version 0.1.0},
  url = {https://CRAN.R-project.org/package=WAnova},
}
```
