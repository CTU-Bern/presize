
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presize

![travis](https://travis-ci.com/CTU-Bern/presize.svg?branch=master)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/CTU-Bern/presize?branch=master&svg=true)](https://ci.appveyor.com/project/CTU-Bern/presize)
[![codecov](https://codecov.io/github/CTU-Bern/presize/branch/master/graphs/badge.svg)](https://codecov.io/github/CTU-Bern/presize)
[![](https://img.shields.io/badge/dev%20version-0.0.1.9005-blue.svg)](https://github.com/CTU-Bern/presize)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

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
      - rate ratio (`prec_rateratio`) <!--    + hazard ratio -->
  - correlation measures
      - correlation coefficient (`prec_cor`)
      - Cohens kappa (`prec_kappa`)
      - ICC (`prec_icc`)
      - limit of agreement from Bland Altman plot (`prec_lim_agree`)
  - diagnostic measures
      - sens (`prec_sens`<sup>1</sup>)
      - spec (`prec_spec`<sup>1</sup>) <!--    + positive LR -->
        <!--    + negative LR -->
      - AUC (`prec_auc`)

<sup>1</sup> Simple wrappers for `prec_prop`.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(presize)
#> Loading required package: ggplot2
#> Loading required package: kappaSize
#> Loading required package: shiny

# calculate sample size for a proportion of 0.2, or 0.4 with a precision of 0.2
prec_prop(p = c(.2, .4), n = 10, method = "wilson")
#> 
#>      precision for a proportion with Wilson confidence interval. 
#> 
#>     p      padj  n conf.width conf.level        lwr       upr
#> 1 0.2 0.2832598 10  0.4531554       0.95 0.05668215 0.5098375
#> 2 0.4 0.4277533 10  0.5191459       0.95 0.16818033 0.6873262
#> 
#> NOTE: padj is the adjusted proportion, from which the ci is calculated.
```

THe problem being addressed is ‘how wide is the confidence interval
width given proportions of events of 20 and 40% and only 10
participants’.

## Shiny app

An online interactive version of the package is available
[here](https://ctu-bern.shinyapps.io/presize). The app can also be
launched locally via `launch_presize_app()` in RStudio.

## Funding

`presize` was largely developed at CTU Bern, with collaboration from CTU
Basel. Funding was provided by the Swiss Clinical Trials Organisation.

![](inst/fig/scto_ctu_member_cmyk.jpg)
