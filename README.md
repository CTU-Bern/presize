<!-- README.md is generated from README.Rmd. Please edit that file -->

presize
=======

[![](https://www.r-pkg.org/badges/version/presize?color=green)](https://cran.r-project.org/package=presize)
[![](https://img.shields.io/badge/dev%20version-0.2.0-blue.svg)](https://github.com/CTU-Bern/presize)
![travis](https://travis-ci.com/CTU-Bern/presize.svg?branch=master)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/CTU-Bern/presize?branch=master&svg=true)](https://ci.appveyor.com/project/CTU-Bern/presize)
[![Actions
Status](https://github.com/CTU-Bern/presize/workflows/R-CMD-check/badge.svg)](https://github.com/CTU-Bern/presize/actions)
[![codecov](https://codecov.io/github/CTU-Bern/presize/branch/master/graphs/badge.svg)](https://codecov.io/github/CTU-Bern/presize)

[Bland (2009)](https://www.bmj.com/content/339/bmj.b3985) recommended to
base study sizes on the width of the confidence interval rather the
power of a statistical test. The goal of `presize` is to provide
functions for such precision based sample size calculations. For a given
sample size, the functions will return the precision (width of the
confidence interval), and vice versa.

Installation
------------

`presize` can be installed from CRAN in the usual manner:

    install.packages("presize")

You can install the development version of `presize` from github with:

    # install.packages("remotes")
    remotes::install_github("CTU-Bern/presize")

Note that `remotes` treats any warnings (e.g. that a certain package was
built under a different version of R) as errors. If you see such an
error, run the following line and try again:

    Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

Overview
--------

presize will provide functions for

-   descriptive statistics
    -   mean (`prec_mean`)
    -   proportion (`prec_prop`)
    -   rate (`prec_rate`)
-   absolute and relative differences
    -   mean difference (`prec_meandiff`)
    -   risk difference (`prec_riskdiff`)
    -   odds ration (`prec_or`)
    -   risk ratio (`prec_riskratio`)
    -   rate ratio (`prec_rateratio`) <!--    + hazard ratio -->
-   correlation measures
    -   correlation coefficient (`prec_cor`)
    -   Cohens kappa (`prec_kappa`)
    -   ICC (`prec_icc`)
    -   limit of agreement from Bland Altman plot (`prec_lim_agree`)
-   diagnostic measures
    -   sens (`prec_sens`<sup>1</sup>)
    -   spec (`prec_spec`<sup>1</sup>)
    -   likelihood ratios (`prec_lr`)
        -   positive likelihood ratio (`prec_pos_lr`<sup>2</sup>)
        -   negative likelihood ratio (`prec_neg_lr`<sup>2</sup>)
    -   AUC (`prec_auc`)

<sup>1</sup> Simple wrappers for `prec_prop`.

<sup>2</sup> Wrappers for `prec_lr` with values provided via sens and
spec

Example
-------

This is a basic example which shows you how to solve a common problem:

    library(presize)

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

THe problem being addressed is ‘how wide is the confidence interval
width given proportions of events of 20 and 40% and only 10
participants’.

Shiny app
---------

An online interactive version of the package is available
[here](https://ctu-bern.shinyapps.io/presize). The app can also be
launched locally via `launch_presize_app()` in RStudio.

Funding
-------

`presize` was largely developed at CTU Bern, with collaboration from CTU
Basel. Funding was provided by the Swiss Clinical Trial Organisation.

<img src="inst/fig/SCTO_Platform_Logo_2020_RZ_SM.jpg" width="148" />

<!-- ![](man/fig/scto_ctu_member_cmyk.jpg) -->
