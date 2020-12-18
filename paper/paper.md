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
   orcid:
   affiliation: "1, 2"
 - name: Andreas Limacher
   orcid: 0000-0002-9094-9476
   affiliation: "1, 2"
affiliations:
 - name: CTU Bern, University of Bern
   index: 1
 - name: Statistics and Methodology Platform of the Swiss Clinical Trial Organisation (SCTO)
   index: 2
bibliography: paper.bib
---



# Statement of need

Sample size calculation is a crucial step in planning a clinical study. A too
small study leads to inconclusive results; a too large study is a waste of
resources. Either case might be unethical. There are many software packages
for hypothesis-based sample size calculation, such as [Stata](https://www.stata.com/),
[PASS](https://www.ncss.com/software/pass/), and [G*Power](https://www.gpower.hhu.de).
However, many research projects aim at estimation rather than hypothesis testing,
sample size calculation approaches for which are largely missing from other software
packages. Furthermore, @bland2009 called for a focus on the width of confidence intervals 
rather than the power of the test in sample size calculations. 
We therefore developed a comprehensive tool for precision-based sample size calculation.

# Development

We programmed a package in the R programming language that offers sample size
calculation for estimation-based research. We implemented the most common
measures used in descriptive research; see Table 1 for a list of functions,
corresponding measures, and methods used for calculation. 
There are two approaches for each measure. First, based on a given sample size,
e.g. for a retrospective data analysis, the precision of an expected measure can
be calculated. Precision is expressed as the confidence interval around the
measure. The level of confidence can be specified; the usual 95%-confidence
interval is the default. Second, based on a given precision (i.e. confidence
interval), the sample size can be calculated. This is mainly of use in the
planning of prospective studies, where the aim is to estimate a measure of
interest with enough confidence. The available functions in the package require
common input arguments; either the sample size or the width of the confidence
interval, and the level of the confidence interval. Depending on the function,
further specific input arguments are required such as the expected area under
the curve (AUC) for test accuracy.


Table 1. Measures, functions and methods provided in presize.

Measure | Function | Methods available 
-------- | ---------- | --------
**Descriptive measures** | |
Mean | `prec_mean` |
Proportion | `prec_prop` | Wilson, Agresti-Coull, exact, Wald [see @brown2001]
Rate | `prec_rate` | Score, variance stabilizing, exact, Wald [see @barker2002]
**Absolute differences** | |
Mean difference | `prec_meandiff` |
Risk difference | `prec_riskdiff` | Newcombe [@newcombe1998], Miettinen-Nurminen [@mn1985], Agresti-Caffo [@ac2000], Wald
**Relative differences** | |
Odds ratio | `prec_or` | Gart, Wolff, independence smoothed logit [see @fll2015]
Risk ratio | `prec_riskratio` | Koopman [@koopman1984], Katz [@kbap1978]
Rate ratio | `prec_rateratio` | Rothman [@rg2018]
**Correlation measures** | |
Correlation coefficient | `prec_cor` | Pearson, Kendall, Spearman [see @bw2000]
Intraclass correlation | `prec_icc` | @bonnett2002
Limit of agreement | `prec_lim_agree` | @ba1986
Cohens kappa | `prec_kappa` | @rd2012
**Diagnostic measures** | |
Sensitivity | `prec_sens` | As per prec_prop
Specificity | `prec_spec` | As per prec_prop
Area under the curve | `prec_auc` | @hm1982



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
# install.packages("presize") # CRAN
# remotes::install_github('ctu-bern/presize') # development version on GitHub
library(presize)
```
Example (e.g. Gynocular)…

# Discussion

We developed a comprehensive and easy-to-use software package for precision-based 
sample size calculation. As far as we know, presize is the first package that comprises 
the most common summary measures used in estimation-based clinical research.
A limitation of the package is that it does not allow calculating the probability 
of a confidence interval, i.e. the probability that a future confidence interval 
will have at least the desired precision. This is closely related to the concept 
of power in hypothesis testing. Stating that the confidence interval around an 
expected value will exclude a certain value with a probability of 80% is equivalent 
to testing that value against the expected value with a power of 80%, i.e. both 
approaches will yield the same sample size. We do not regard it as sensible that 
an estimation-based approach is used for inference and therefore only implemented 
simple calculations that will on average yield a given precision.
We often observe in our consulting activity that researchers try to implement a 
hypothesis-based approach into a project that is in fact purely descriptive. Reasons 
for this might be a lack of methodological understanding, but also a lack of appropriate 
tools. To conclude, we believe that our software package will fascilitate the 
adequate use of estimation-based sample size calculation in descriptive research projects.

# Acknowledgements
The development of `presize` was enabled through financial support of the [Swiss 
Clinical Trial Organisation (SCTO)](https://www.scto.ch/en) as part of it's [Statistics and Methodology Platform](https://www.scto.ch/en/network/scto-platforms/statistics-and-methodology.html).
We also wish to thank statisticians of [CTU Bern](https://www.ctu.unibe.ch/) and [CTU Basel](https://www.unispital-basel.ch/ueber-uns/das-universitaetsspital/leitung/direktion/klinische-forschung/) 
for suggestions and testing.

# References

