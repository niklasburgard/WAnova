# WAnova

'WAnova' is an R package that provides functions for conducting Welch's ANOVA and Games-Howell post hoc tests based on summary statistics. These tests are particularly useful when working with unequal variances and sample sizes.

## Table of content

1. [WAnova](#wanova)
2. [Installation](#installation)
3. [Features](#features)
4. [Usage](#usage)
    1. [Parameters](#parameters)
    2. [Example: Hartley's Fmax](#example-hartleys-fmax)
    3. [Example: Welch's ANOVA](#example-welchs-anova)
    4. [Example: Games-Howell Post Hoc Test](#example-games-howell-post-hoc-test)
5. [Citing WAnova](#citing-wanova)

## Installation

You can install the latest version of 'WAnova' from GitHub with:
```
# If you don't have devtools installed, first install it:
install.packages("devtools")

# Then, install the WAnova package from GitHub:
devtools::install_github("niklasburgard/WAnova")

# Import packages if not already installed:
packages <- c("utils", "stats", "SuppDists")
install.packages(setdiff(packages, rownames(installed.packages())))
```

## Features

+ ***Hartley's Fmax***: Perform Hartley's Fmax to check homogeneity of variances.
+ ***Welch's ANOVA***: Perform Welch's ANOVA based on summary statistics (mean, standard deviation, sample size).
+ ***Games-Howell Post Hoc Test***: Conduct the Games-Howell post hoc test for multiple comparisons after Welch's ANOVA.
+ ***Effect Size Calculations:***: Calculate adjusted omega squared effect sizes for Welch's ANOVA using different methods ("AnL","Kirk","CaN").
+ ***Sample Size Estimation***: Caclulate the sample sizes based on desired Significance level and power

## Usage

### Parameters

fmax.test(levels,n,sd)  
welch_anova.test(levels, n, means, sd, effsize = c("AnL","Kirk","CaN")  
games_howell.test(levels, n, means, sd, conf.level = 0.95)
wanova_pwr.test(n, means, sd, power = 0.90, alpha = 0.05)

***levels*** Vector with level names of the independent variable  
***n***  Vector with sample size for each level  
***means*** Vector with sample mean for each level  
***sd*** Vector with sample standard deviation for each level  
**effsize** Options "AnL", "Kirk", "CaN"  
***conf.level*** Confidence level used in the computation
***power*** Significance level for the test
***alpha*** Desired power of the test

### Example: Hartley's Fmax
```
library(WAnova)
library(SuppDists)

# Example data
probe_data <- data.frame(
 group = c("probe_a", "probe_b", "probe_c"),
 size = c(10, 10, 10),  # Equal sample sizes
 mean = c(43.00000, 33.44444, 35.75000),
 sd = c(4.027682, 9.302031, 16.298554)
)

# Perform Hartley's Fmax
result <- fmax_test(
 levels = probe_data$group,
 n = probe_data$size,
 sd = probe_data$sd
)

#Print results
print(result)
```
***Note:*** Applicable results assume normally distributed data with equal sample sizes.  

Null Hypothesis: Assumes homogeneity of variances, which means all groups have the same variance.  
Alternative Hypothesis: Assumes that not all group variances are equal. This hypothesis is
supported if the p-value is below the significance level.

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
***Note:*** Omega squared can range from -1 to 1, with zero indicating no effect. When the observed F is less than one, omega squared will be negative. It has been suggested that values of .01, .06 and .14 represent small, medium and large effects, respectively (Kirk 1996).

***Note***: Traditional omega squared assumes homogeneity of variance, using parameters calculated in a traditional ANOVA with unweighted means. There are three methods—Kirk ("***Kirk***"), Carroll and Nordholm ("***CaN***"), and Albers and Lakens ("***AnL***")—to estimate omega squared using summary statistics, all of which yield the same result when based on unweighted means.  
When applying parameters derived from Welch's ANOVA, the ***Kirk*** and ***CaN*** methods produce an adjusted omega squared that reflects the weighted means from the F-statistic but do not account for the corrected within-group degrees of freedom associated with those weighted means.  
The ***AnL*** method further adjusts omega squared to incorporate these corrected degrees of freedom, aligning with the design of Welch's ANOVA and providing a more accurate measure, making it the preferred approach.

***References:***  
***Welch, B. L. (1951)***. On the comparison of several mean values: an alternative approach. Biometrika 38.3/4, 330-336.  
Hays, W. L. (1973). Statistics for the social sciences (2nd ed.). Holt, Rinehart and Winston, 486.  
***Kirk, R. E. (1996)***. Practical significance: A concept whose time has come. Educational and Psychological Measurement, 56(5), 746-759.  
***Carroll, R. M., & Nordholm, L. A. (1975)***. Sampling characteristics of Kelley's epsilon and Hays' omega Educational and Psychological Measurement, 35(3), 541-554.  
***Albers, C., & Lakens, D. (2018)***. When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of Experimental Social Psychology, 74, 187–195.

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

***References:***  
***Games, P. A., & Howell, J. F. (1976)***. Pairwise Multiple Comparison Procedures with Unequal N’s and/or Variances: A Monte Carlo Study. Journal of Educational and Behavioural Statistics, 1, 113-125.

### Example: Approximate Sample Size Determination for Welch’s F-Test
```
library(WAnova)

n <- c(10, 10, 10, 10)
means <- c(1, 0, 0, -1)
sd <- c(1, 1, 1, 1)
result <- wsize(n, means, sd, power = 0.90, alpha = 0.05)
print(result)
```

***References:***  
***Levy, K. J. (1978a)***. Some empirical power results associated with Welch’s robust analysis of variance technique. Journal of Statistical Computation and Simulation, 8, 43–48.  
***Show-Li, J., & Gwowen, S. (2014)***. Sample size determinations for Welch's test in one-way heteroscedastic ANOVA . British Psychological Society, 67(1), 72-93.

### Citing WAnova

To cite the WAnova package in publications, please use:
```
Niklas Burgard (2023). WAnova: Welch's Anova from Summary Statistics. R package version 0.2.0. https://github.com/niklasburgard/WAnova
```
You can also find a BibTeX entry for LaTeX users:
```
@Manual{,
  title = {WAnova: Welch's Anova from Summary Statistics},
  author = {Niklas Burgard},
  year = {2023},
  note = {R package version 0.2.0},
  url = {https://github.com/niklasburgard/WAnova},
}
```
