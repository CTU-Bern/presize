# Sample size or precision for AUC

Calculate the sample size from AUC, prevalence and confidence interval
width or the expected confidence interval width from AUC, prevalence and
sample size, following Hanley and McNeil (1982).

## Usage

``` r
prec_auc(auc, prev, n = NULL, conf.width = NULL, conf.level = 0.95, ...)
```

## Arguments

  - auc:
    
    AUC value.

  - prev:
    
    prevalence.

  - n:
    
    number of observations.

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

  - ...:
    
    other arguments to `optimize`.

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Sample size is derived by optimizing the difference between the
difference between the lower and upper limits of the confidence interval
and `conf.width`.

## References

Hanley, JA and McNeil, BJ (1982) *The Meaning and Use of the Area under
a Receiver Operating Characteristic (ROC) Curve.* Radiology 148, 29-36

## Examples

``` r
# confidence interval width
N <- 500
prev <- .1
auc <- .65
(prec <- prec_auc(auc, prev, n = N))
#> 
#>      precision for AUC 
#> 
#>    auc   n prev n1  n2       lwr       upr conf.width conf.level
#> 1 0.65 500  0.1 50 450 0.5639623 0.7360377  0.1720755       0.95
cwidth <- prec$conf.width
# sample size
prec_auc(auc, prev, conf.width = cwidth)
#> 
#>      sample size for AUC 
#> 
#>    auc   n prev n1  n2       lwr       upr conf.width conf.level
#> 1 0.65 500  0.1 50 450 0.5639623 0.7360377  0.1720755       0.95
```
