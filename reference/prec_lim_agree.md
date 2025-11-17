# Sample size or precision for limit of agreement on Bland-Altman plots

`prec_lim_agree` returns the sample size or the precision for the limit
of agreement, i.e. the confidence interval around the limit of
agreement, expressed in SD-units. It is an approximation based on the
Normal distribution, instead of a Student t distribution.

## Usage

``` r
prec_lim_agree(n = NULL, conf.width = NULL, conf.level = 0.95)
```

## Arguments

  - n:
    
    sample size.

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Exactly one of the parameters `n` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

The sample size and precision are calculated according to formulae in
Bland & Altman (1986). The CI width is a simple function of the sample
size only.

## References

Bland & Altman (1986) *Statistical methods for assessing agreement
between two methods of clinical measurement* Lancet i(8476):307-310
[doi:10.1016/S0140-6736(86)90837-8](https://doi.org/10.1016/S0140-6736%2886%2990837-8)

## Examples

``` r
# calculate confidence interval width, given N
prec_lim_agree(200)
#> 
#>      precision for limit of agreement 
#> 
#>     n conf.width conf.level
#> 1 200  0.4800912       0.95
# calculate N given, confidence interval width
prec_lim_agree(conf.width = .1)
#> 
#>      sample size for limit of agreement 
#> 
#>          n conf.width conf.level
#> 1 4609.751        0.1       0.95
```
