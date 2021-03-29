---
title: '`presize`: An R-package for precision-based sample size calculation in clinical research'
tags:
  - R software
  - clinical trials
  - sample size calculation
authors:
 - name: Alan G Haynes
   orcid: 0000-0003-1374-081X
   affiliation: "1, 2"
 - name: Armando Lenz
   orcid: 0000-0001-5888-0846
   affiliation: "1, 2"
 - name: Odile Stalder
   orcid: 0000-0002-5563-2975
   affiliation: "1, 2"
 - name: Andreas Limacher
   orcid: 0000-0002-9094-9476
   affiliation: "1, 2"
affiliations:
 - name: CTU Bern, University of Bern, Bern, Switzerland
   index: 1
 - name: Statistics and Methodology Platform of the Swiss Clinical Trial Organisation (SCTO), Bern, Switzerland
   index: 2
bibliography: paper.bib
date: 2021-02-11
---


# Statement of need

Sample size calculation is a crucial step in planning a clinical study. A too
small study leads to inconclusive results; a too large study is a waste of
resources. Either case might be unethical. 
Furthermore, @bland2009 called for a focus on the width of confidence intervals 
rather than the power of the test in sample size calculations. 
There are many software packages
for hypothesis-based sample size calculation, such as [Stata](https://www.stata.com/),
[PASS](https://www.ncss.com/software/pass/), and [G*Power](https://www.gpower.hhu.de).
However, many research projects aim at estimation rather than hypothesis testing,
sample size calculation approaches for which are largely missing from other software
packages. 
We therefore developed a comprehensive tool for precision-based sample size calculation.

# Development

We programmed a package in the R programming language that offers sample size
calculation for estimation-based research. We implemented the most common
measures used in descriptive research, including descriptive, absolute and relative 
difference, correlation and diagnostic measures. 
There are two approaches for each measure. First, based on a given sample size,
e.g. for a retrospective data analysis, the precision of an expected measure can
be calculated. Precision is expressed as the confidence interval around the
measure. The level of confidence can be specified; the usual 95%-confidence
interval (CI) is the default. Second, based on a given precision (i.e. CI), the sample size can be calculated. This is mainly of use in the
planning of prospective studies, where the aim is to estimate a measure of
interest with enough confidence. The available functions in the package require
common input arguments; either the sample size or the width of the CI, and the level of the CI. Depending on the function,
further specific input arguments are required such as the expected area under
the curve (AUC) for test accuracy.


For ease-of-use, we also implemented a Shiny application which can be used
[online](https://ctu-bern.shinyapps.io/presize) or from within the R-environment.
Values of parameters can be entered using rulers and numeric fields. Based on
the given parameters, the application will either display the sample size for a
given precision, or vice-versa, the precision for a given sample size. Moreover,
the application also displays the corresponding R-code, which allows copying the
command into the R-environment for further exploration as well as reproducibility.


# Usage

`presize` is available on [CRAN](https://CRAN.R-project.org/package=presize) or [GitHub](https://github.com/CTU-Bern/presize) and can be loaded into the R session using

```r
# installation:
# install.packages("presize") # CRAN
# remotes::install_github('ctu-bern/presize') # development version on GitHub
library(presize)
```
As a brief example, suppose we want to estimate the proportion of hospital admissions with diabetes. 
Diabetes has a prevalence of approximately 10% (@diab). We assume a 
slightly higher proportion of diabetics, 
15%, as diabetes is a risk factor for a wide range of conditions. We want to 
estimate the prevalence of diabetes to within 5% (plus/minus 2.5%). With `presize`,
this is simple. We use the `prec_prop` (precision of a proportion) function and pass 
our 15% and 5% as arguments `p` and `conf.width`:

```r
prec_prop(p = 0.15, conf.width = 0.05)

     sample size for a proportion with Wilson confidence interval. 

     p      padj        n conf.width conf.level       lwr       upr
1 0.15 0.1517077 783.4897       0.05       0.95 0.1267077 0.1767077
```

In the `n` column, we see that we would need to ask 784 (rounding 783.5 up) patients to achieve the desired CI width. It is also possible to calculate the CI width with a given number of participants, for the case that we know roughly how many participants we could include: 

```r
prec_prop(p = 0.15, n = 600)

     sample size for a proportion with Wilson confidence interval. 

     p      padj   n conf.width conf.level       lwr       upr
1 0.15 0.1522266 600 0.05713404       0.95 0.1236596 0.1807936
```

# A limitation

A limitation of the package is that it does not allow calculating the probability 
of a CI, i.e. the probability that a future confidence interval 
will have at least the desired precision. The functions currently return the average 
CI width. In practice, 50% of trials will yield narrower CIs and 
50% will yield wider CIs due to sampling variation. Providing a method to specify 
the coverage probability is one possible avenue 
for further development.

# Summary

We often observe in our consulting activity that researchers try to implement a 
hypothesis-based approach into a project that is in fact purely descriptive. Reasons 
for this might be a lack of methodological understanding, but also a lack of appropriate 
tools. Therefore, we developed a comprehensive and easy-to-use software package in R for precision-based sample size calculation. As far as we know, `presize` is the first 
package that comprises the most common summary measures used in estimation-based 
clinical research. We believe that `presize` will fascilitate the 
adequate use of estimation-based sample size calculation in descriptive research projects.

# Acknowledgements
The development of `presize` was enabled through financial support of the [Swiss 
Clinical Trial Organisation (SCTO)](https://www.scto.ch/en) as part of it's [Statistics and Methodology Platform](https://www.scto.ch/en/network/scto-platforms/statistics-and-methodology.html).
We also wish to thank statisticians of [CTU Bern](https://www.ctu.unibe.ch/) and [CTU Basel](https://www.unispital-basel.ch/ueber-uns/das-universitaetsspital/leitung/direktion/klinische-forschung/) 
for suggestions and testing.

# References

