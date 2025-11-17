# Sample size or precision for Cohen's kappa

`prec_kappa` returns the sample size or the precision for the provided
Cohen's kappa coefficient.

## Usage

``` r
prec_kappa(
  kappa,
  n = NULL,
  raters = 2,
  n_category = 2,
  props,
  conf.width = NULL,
  conf.level = 0.95
)
```

## Arguments

  - kappa:
    
    expected value of Cohen's kappa.

  - n:
    
    sample size.

  - raters:
    
    number of raters (maximum of 6).

  - n\_category:
    
    number of categories of outcomes (maximum of 5).

  - props:
    
    expected proportions of each outcome (should have length
    `n_category`).

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

This function wraps the `FixedN` and `CI` functions in the `kappaSize`
package. The `FixedN` functions in `kappaSize` return a one sided
confidence interval. The values that are passed to `kappaSize` ensure
that two-sided confidence intervals are returned, although we assume
that confidence intervals are symmetrical.

## See also

`FixedNBinary`, `FixedN3Cats`, `CIBinary`, `CI3Cats`

## Examples

``` r
# precision based on sample size
#   two categories with proportions of 30 and 70\%, four raters
prec_kappa(kappa = .5, n = 200, raters = 4, n_category = 2, props = c(.3,.7))
#> 
#>      precision for Cohen's kappa 
#> 
#>   kappa   n   lwr   upr conf.width conf.level
#> 1   0.5 200 0.425 0.575       0.15       0.95
# sample size to get a given precision
prec_kappa(kappa = .5, conf.width = .15, raters = 4, n_category = 2,
           props = c(.3,.7))
#> 
#>      sample size for Cohen's kappa 
#> 
#>   kappa   n   lwr   upr conf.width conf.level
#> 1   0.5 198 0.425 0.575       0.15       0.95

# as above, but with two scenarios for kappa
prec_kappa(kappa = c(.5, .75), conf.width = .15, raters = 4, n_category = 2,
           props = c(.3,.7))
#> 
#>      sample size for Cohen's kappa 
#> 
#>   kappa   n   lwr   upr conf.width conf.level
#> 1  0.50 198 0.425 0.575       0.15       0.95
#> 2  0.75 155 0.675 0.825       0.15       0.95
prec_kappa(kappa = c(.5, .75), conf.width = c(.15, 0.3), raters = 4,
           n_category = 2, props = c(.3,.7))
#> 
#>      sample size for Cohen's kappa 
#> 
#>   kappa   n   lwr   upr conf.width conf.level
#> 1  0.50 198 0.425 0.575       0.15       0.95
#> 2  0.75  44 0.600 0.900       0.30       0.95
```
