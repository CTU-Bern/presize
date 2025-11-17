# Sample size or precision for a rate ratio

`prec_rateratio` returns the sample size or the precision for the
provided proportions.

## Usage

``` r
prec_rateratio(
  n1 = NULL,
  rate1 = NULL,
  rate2 = 2 * rate1,
  prec.level = NULL,
  r = 1,
  conf.level = 0.95
)
```

## Arguments

  - n1:
    
    number of patients in exposed group.

  - rate1:
    
    event rate in the exposed group.

  - rate2:
    
    event rate in the unexposed group.

  - prec.level:
    
    ratio of the upper limit over the lower limit of the rate ratio
    confidence interval.

  - r:
    
    allocation ratio (relative size of unexposed and exposed cohort
    (`n2` / `n1`)).

  - conf.level:
    
    confidence level.

## Details

Exactly one of the parameters `n1` or `conf.width` must be passed as
NULL, and that parameter is determined from the other. Event rates in
the two groups should also be provided (`rate1, rate2`). If only `rate1`
is provided, `rate2` is assumed to be 2 times `rate1`.

## References

Rothman KJ, Greenland S (2018). *Planning Study Size Based on Precision
Rather Than Power*. Epidemiology, 29:599-603.
[doi:10.1097/EDE.0000000000000876](https://doi.org/10.1097/EDE.0000000000000876)
.

## Examples

``` r
# 20 participants, a rate of 50%  against a rate of 300\%
prec_rateratio(20, .5, 3)
#> 
#>      precision for a rate ratio 
#> 
#>   n1 n2 r ntot rate1 rate2        rr        lwr       upr prec.level conf.level
#> 1 20 20 1   40   0.5     3 0.1666667 0.08533124 0.3255288   3.814884       0.95
# sample size required to attain a CI whose upper limit is not more than 3.81 larger
#  than the lower limit
prec_rateratio(rate1 = .5, rate2 = 3, prec.level = 3.81)
#> 
#>      sample size for a rate ratio 
#> 
#>         n1       n2 r     ntot rate1 rate2        rr        lwr       upr
#> 1 20.03833 20.03833 1 40.07666   0.5     3 0.1666667 0.08538592 0.3253204
#>   prec.level conf.level
#> 1       3.81       0.95
```
