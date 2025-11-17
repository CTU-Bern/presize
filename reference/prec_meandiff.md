# Sample size or precision for a mean difference

`prec_meandiff` returns the sample size or the precision for the
provided mean difference and standard deviations. For paired
differences, use `prec_mean`, as it is equivalent to a simple mean.

## Usage

``` r
prec_meandiff(
  delta,
  sd1,
  sd2 = sd1,
  n1 = NULL,
  r = 1,
  conf.width = NULL,
  conf.level = 0.95,
  variance = c("equal", "unequal"),
  ...
)
```

## Arguments

  - delta:
    
    difference in means between the two groups.

  - sd1:
    
    standard deviation in group 1.

  - sd2:
    
    standard deviation in group 2.

  - n1:
    
    number of patients in group 1.

  - r:
    
    allocation ratio (relative size of group 2 and group 1 (n2 / n1)).

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

  - variance:
    
    `equal` (*default*) or `unequal` variance.

  - ...:
    
    other options to uniroot (e.g. `tol`)

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Exactly one of the parameters `n` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

## Examples

``` r
# mean difference of 5, SD of 2.5, CI width with 20 participants assuming equal variances
prec_meandiff(delta = 5, sd1 = 2.5, n1 = 20, var = "equal")
#> 
#>      precision for mean difference with equal variance 
#> 
#>   delta sd1 sd2 n1 n2 conf.width conf.level      lwr      upr
#> 1     5 2.5 2.5 20 20   3.200848       0.95 3.399576 6.600424
# mean difference of 5, SD of 2.5, number of participants for a CI width of 3,
#  assuming equal variances
prec_meandiff(delta = 5, sd1 = 2.5, conf.width = 3, var = "equal")
#> 
#>      sample size for mean difference with equal variance 
#> 
#>   delta sd1 sd2       n1       n2 conf.width conf.level lwr upr
#> 1     5 2.5 2.5 22.58932 22.58932          3       0.95 3.5 6.5
```
