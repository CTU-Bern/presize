# Sample size or precision for a proportion

`prec_prop` returns the sample size or the precision for the provided
proportion.

## Usage

``` r
prec_prop(
  p,
  n = NULL,
  conf.width = NULL,
  conf.level = 0.95,
  method = c("wilson", "agresti-coull", "exact", "wald"),
  ...
)
```

## Arguments

  - p:
    
    proportion.

  - n:
    
    number of observations.

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

  - method:
    
    The method to use to calculate precision. Exactly one method may be
    provided. Methods can be abbreviated.

  - ...:
    
    other arguments to uniroot (e.g. `tol`).

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements. In the wilson and
agresti-coull formula, the p from which the confidence interval is
calculated is adjusted by a term (i.e. \\(p + term \\pm ci\\)). This
adjusted p is returned in `padj`.

## Details

Exactly one of the parameters `n` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

The wilson, agresti-coull, exact, and wald method are implemented. The
wilson method is suggested for small `n` (\< 40), and the agresti-coull
method is suggested for larger `n` (see reference). The wald method is
not suggested, but provided due to its widely distributed use.

`uniroot` is used to solve `n` for the agresti-coull, wilson, and exact
methods. Agresti-coull can be abbreviated by ac.

## References

Brown LD, Cai TT, DasGupta A (2001) *Interval Estimation for a Binomial
Proportion*, Statistical Science, 16:2, 101-117,
[doi:10.1214/ss/1009213286](https://doi.org/10.1214/ss/1009213286)

## See also

`binom.test`, `binom.confint` in package binom, and `binconf` in package
Hmisc

## Examples

``` r
# CI width for 15\% with 50 participants
prec_prop(0.15, n = 50)
#> Warning: more than one method was chosen, 'wilson' will be used
#> 
#>      precision for a proportion with Wilson confidence interval. 
#> 
#>      p      padj  n conf.width conf.level        lwr       upr
#> 1 0.15 0.1749717 50  0.1971842       0.95 0.07637956 0.2735638
#> 
#> NOTE: padj is the adjusted proportion, from which the ci is calculated.
#> 
# number of participants for 15\% with a CI width of 0.2
prec_prop(0.15, conf.width = 0.2)
#> Warning: more than one method was chosen, 'wilson' will be used
#> 
#>      sample size for a proportion with Wilson confidence interval. 
#> 
#>      p      padj        n conf.width conf.level        lwr       upr
#> 1 0.15 0.1756455 48.58521        0.2       0.95 0.07564555 0.2756455
#> 
#> NOTE: padj is the adjusted proportion, from which the ci is calculated.
#> 
# confidence interval width for a range of scenarios between 10 and 90\% with
#  100 participants via the wilson method
prec_prop(p = 1:9 / 10, n = 100, method = "wilson")
#> 
#>      precision for a proportion with Wilson confidence interval. 
#> 
#>     p      padj   n conf.width conf.level        lwr       upr
#> 1 0.1 0.1147974 100  0.1191365       0.95 0.05522914 0.1743657
#> 2 0.2 0.2110980 100  0.1554622       0.95 0.13336693 0.2888292
#> 3 0.3 0.3073987 100  0.1768997       0.95 0.21894885 0.3958485
#> 4 0.4 0.4036993 100  0.1885961       0.95 0.30940129 0.4979974
#> 5 0.5 0.5000000 100  0.1923369       0.95 0.40383153 0.5961685
#> 6 0.6 0.5963007 100  0.1885961       0.95 0.50200259 0.6905987
#> 7 0.7 0.6926013 100  0.1768997       0.95 0.60415145 0.7810511
#> 8 0.8 0.7889020 100  0.1554622       0.95 0.71117083 0.8666331
#> 9 0.9 0.8852026 100  0.1191365       0.95 0.82563434 0.9447709
#> 
#> NOTE: padj is the adjusted proportion, from which the ci is calculated.
#> 
# number of participants for a range of scenarios between 10 and 90\% with
#  a CI of 0.192 via the wilson method
prec_prop(p = 1:9 / 10, conf.width = .192, method = "wilson")
#> 
#>      sample size for a proportion with Wilson confidence interval. 
#> 
#>     p      padj         n conf.width conf.level       lwr       upr
#> 1 0.1 0.1353927  39.57381      0.192       0.95 0.0393927 0.2313927
#> 2 0.2 0.2167537  64.94554      0.192       0.95 0.1207537 0.3127537
#> 3 0.3 0.3087050  84.41747      0.192       0.95 0.2127050 0.4047050
#> 4 0.4 0.4038339  96.35634      0.192       0.95 0.3078339 0.4998339
#> 5 0.5 0.5000000 100.36478      0.192       0.95 0.4040000 0.5960000
#> 6 0.6 0.5961661  96.35634      0.192       0.95 0.5001661 0.6921661
#> 7 0.7 0.6912950  84.41747      0.192       0.95 0.5952950 0.7872950
#> 8 0.8 0.7832463  64.94554      0.192       0.95 0.6872463 0.8792463
#> 9 0.9 0.8646073  39.57381      0.192       0.95 0.7686073 0.9606073
#> 
#> NOTE: padj is the adjusted proportion, from which the ci is calculated.
#> 
```
