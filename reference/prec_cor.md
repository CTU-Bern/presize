# Sample size or precision for correlation coefficient

`prec_cor` returns the sample size or the precision for the given
pearson, spearman, or kendall correlation coefficient.

## Usage

``` r
prec_cor(
  r,
  n = NULL,
  conf.width = NULL,
  conf.level = 0.95,
  method = c("pearson", "kendall", "spearman"),
  ...
)
```

## Arguments

  - r:
    
    desired correlation coefficient.

  - n:
    
    sample size.

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

  - method:
    
    Exactly one of `pearson` (*default*), `kendall`, or `spearman`.
    Methods can be abbreviated.

  - ...:
    
    other options to uniroot (e.g. `tol`)

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Exactly one of the parameters `n` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

Sample size or precision is calculated according to formula 2 in Bonett
and Wright (2000). The use of pearson is only recommended, if \\(n \\ge
25\\). The pearson correlation coefficient assumes bivariate normality.
If the assumption of bivariate normality cannot be met, spearman or
kendall should be considered.

n is rounded up to the next whole number using `ceiling`.

`uniroot` is used to solve n.

## References

Bonett DG, and Wright TA (2000) *Sample size requirements for estimating
Pearson, Kendall and Spearman correlations* Psychometrika 65:23-28.
[doi:10.1007/BF02294183](https://doi.org/10.1007/BF02294183)

## Examples

``` r
# calculate confidence interval width...
# Pearson correlation coefficient
prec_cor(r = 0.5, n = 100)
#> Warning: more than one method was chosen, 'pearson' will be used
#> 
#>      precision for pearson 's correlation coefficient 
#> 
#>     r   n conf.width conf.level       lwr       upr
#> 1 0.5 100  0.2974965       0.95 0.3366433 0.6341398
# Kendall rank correlation coefficient (tau)
prec_cor(r = 0.5, n = 100, method = "kendall")
#> 
#>      precision for kendall 's correlation coefficient 
#> 
#>     r   n conf.width conf.level       lwr       upr
#> 1 0.5 100  0.1980633       0.95 0.3944584 0.5925217
# Spearman's rank correlation coefficient
prec_cor(r = 0.5, n = 100, method = "spearman")
#> 
#>      precision for spearman 's correlation coefficient 
#> 
#>     r   n conf.width conf.level       lwr       upr
#> 1 0.5 100  0.3154053       0.95 0.3258966 0.6413019
# calculate N required for a given confidence interval width...
# Pearson correlation coefficient
prec_cor(r = 0.5, conf.width = .15)
#> Warning: more than one method was chosen, 'pearson' will be used
#> 
#>      sample size for pearson 's correlation coefficient 
#> 
#>     r   n conf.width conf.level       lwr       upr
#> 1 0.5 387       0.15       0.95 0.4213129 0.5712155
# Kendall rank correlation coefficient (tau)
prec_cor(r = 0.5, conf.width = .15, method = "kendall")
#> 
#>      sample size for kendall 's correlation coefficient 
#> 
#>     r   n conf.width conf.level       lwr      upr
#> 1 0.5 172       0.15       0.95 0.4213599 0.571177
# Spearman's rank correlation coefficient
prec_cor(r = 0.5, conf.width = .15, method = "spearman")
#> 
#>      sample size for spearman 's correlation coefficient 
#> 
#>     r   n conf.width conf.level       lwr       upr
#> 1 0.5 435       0.15       0.95 0.4213129 0.5712155
```
