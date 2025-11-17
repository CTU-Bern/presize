# Sample size or precision for a mean

`prec_mean` returns the sample size or the precision for the provided
mean and standard deviation.

## Usage

``` r
prec_mean(
  mean,
  sd,
  n = NULL,
  conf.width = NULL,
  conf.level = 0.95,
  ...,
  mu = NULL
)
```

## Arguments

  - mean:
    
    mean.

  - sd:
    
    standard deviation.

  - n:
    
    number of observations.

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

  - ...:
    
    other arguments to uniroot (e.g. `tol`).

  - mu:
    
    deprecated argument

## Value

Object of class "presize", a list with `mean` mean, `sd` standard
deviation, `n` sample size, `conf.width` precision (the width of the
confidence interval), `lwr` lower bound of confidence interval, `upr`
upper bound of confidence interval, augmented with method and note
elements.

## Details

Exactly one of the parameters `n` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

The precision is defined as the full width of the confidence interval.
The confidence interval calculated as \\(t(n - 1) \* sd / sqrt(n)\\),
with t(n-1) from the t-distribution with n-1 degrees of freedom.

This function is also suitable for a difference in paired means, as this
reduces to a single value per individual - the difference.

`uniroot` is used to solve `n`.

## Examples

``` r
# mean of 5, SD of 2.5, whats the confidence interval width with 20 participants?
prec_mean(mean = 5, sd = 2.5, n = 20)
#> 
#>      precision for mean 
#> 
#>   mean  sd  n conf.width conf.level      lwr      upr
#> 1    5 2.5 20   2.340072       0.95 3.829964 6.170036
# mean of 5, SD of 2.5, how many participants for CI width of 2.34?
prec_mean(mean = 5, sd = 2.5, conf.width = 2.34)  # approximately the inverse of above
#> 
#>      sample size for mean 
#> 
#>   mean  sd        n conf.width conf.level  lwr  upr
#> 1    5 2.5 20.00108       2.34       0.95 3.83 6.17
```
