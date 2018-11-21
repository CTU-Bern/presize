
<!-- README.md is generated from README.Rmd. Please edit that file -->
presize
=======

**Warning: R-package in development.**

The goal of presize is to provide functions for precision based sample size calculation. For a given sample size, the functions will return the precision (half the width of the confidence interval), and vice versa.

Installation
------------

You can install presize from github with:

``` r
# install.packages("devtools")
devtools::install_github("a-lenz/presize")
```

Overview
--------

presize will provide functions for

-   descriptive statistics
    -   mean (`prec_mean`)
    -   proportion (`prec_prop`)
    -   rate (`prec_rate`)
-   absolute and relative differences
    -   mean difference
    -   risk difference
    -   odds ration
    -   risk ratio
    -   rate ratio
    -   hazard ratio
-   correlation measures
    -   correlation coefficient
    -   Cohens kappa
    -   ICC
    -   limit of agreement from Bland Altman plot
-   diagnostic measures
    -   sens
    -   spez
    -   positive LR
    -   negative LR
    -   AUC

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(presize)

# calculate sample size for a proportion of 0.2 with a precision of 0.2
prec_prop(p = .2, prec = .2, method = "wilson")
#> 
#>      Sample size or precision for a proportion withwilson.
#> 95% confidence level was chosen. 
#> 
#>               p = 0.2
#>               n = 13.44511
#>            prec = 0.2
#>            padj = 0.2666667
#>              lo = 0.06666666
#>              hi = 0.4666667
#> 
#> NOTE: padj is the adjusted proportion, from which the ci is calculated.
# n = 13.4
```
