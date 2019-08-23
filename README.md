
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presize

![travis](https://travis-ci.com/CTU-Bern/presize.svg?branch=master)
[![codecov](https://codecov.io/github/CTU-Bern/presize/branch/master/graphs/badge.svg)](https://codecov.io/github/CTU-Bern/presize)

**Warning: R-package in development.**

The goal of presize is to provide functions for precision based sample
size calculation. For a given sample size, the functions will return the
precision (half the width of the confidence interval), and vice versa.

## Installation

You can install presize from github with:

``` r
# install.packages("devtools")
devtools::install_github("CTU-Bern/presize")
```

## Overview

presize will provide functions for

  - descriptive statistics
      - mean (`prec_mean`)
      - proportion (`prec_prop`)
      - rate (`prec_rate`)
  - absolute and relative differences
      - mean difference (`prec_meandiff`)
      - risk difference (`prec_riskdiff`)
      - odds ration (`prec_or`)
      - risk ratio (`prec_riskratio`)
      - rate ratio
      - hazard ratio
  - correlation measures
      - correlation coefficient (`prec_cor`)
      - Cohens kappa
      - ICC (`prec_icc`)
      - limit of agreement from Bland Altman plot
  - diagnostic measures
      - sens
      - spec
      - sens and spec (`prec_sens_spec`)
      - positive LR
      - negative LR
      - AUC

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(presize)

# calculate sample size for a proportion of 0.2, or 0.4 with a precision of 0.2
prec_prop(p = c(.2, .4), n = 10, method = "wilson")
#> 
#>      precision for a proportion with wilson confidence interval. 
#> 
#>     p      padj  n conf.width conf.level        lwr       upr
#> 1 0.2 0.2832598 10  0.4531554       0.95 0.05668215 0.5098375
#> 2 0.4 0.4277533 10  0.5191459       0.95 0.16818033 0.6873262
#> 
#> NOTE: padj is the adjusted proportion, from which the ci is calculated.
```
