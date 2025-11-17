# Sample size or precision for Cronbach's alpha

`prec_cronb` returns the sample size or the precision for the given
Cronbach's alpha.

## Usage

``` r
prec_cronb(k, calpha, n = NULL, conf.level = 0.95, conf.width = NULL)
```

## Arguments

  - k:
    
    number of measurements/items.

  - calpha:
    
    desired Cronbach's alpha.

  - n:
    
    sample size.

  - conf.level:
    
    confidence level.

  - conf.width:
    
    precision (the full width of the confidence interval).

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Exactly one of the parameters `n` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

Sample size or precision is calculated according to the formula & code
and provided in Bonett and Wright (2014).

n is rounded up to the next whole number using `ceiling`.

## References

Bonett, D. G. and Wright, T. A. (2015) *Cronbach's alpha reliability:
Interval estimation, hypothesis testing, and sample size planning* J.
Organiz. Behav., 36, pages 3– 15.
[doi:10.1002/job.1960](https://doi.org/10.1002/job.1960) . \# k= number
of items

## Examples

``` r
# calculate confidence interval width...
prec_cronb (k=5,calpha=0.7,n= 349,conf.level= 0.95, conf.width= NULL)
#> 
#>      precision for Cronbach's alpha 
#> 
#>   calpha k   n conf.width conf.level       lwr       upr
#> 1    0.7 5 349 0.09999076       0.95 0.6467151 0.7467059
# calculate N required for a given confidence interval width...
prec_cronb (k=5,calpha=0.7,n= NULL,conf.level= 0.95, conf.width= 0.1)
#> 
#>      sample size for Cronbach's alpha 
#> 
#>   calpha k   n conf.width conf.level       lwr       upr
#> 1    0.7 5 349        0.1       0.95 0.6466332 0.7467688
```
