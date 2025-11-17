# Sample size or precision for an intraclass correlation

`prec_icc` returns the sample size or the precision for the given
intraclass correlation.

## Usage

``` r
prec_icc(rho, k, n = NULL, conf.width = NULL, conf.level = 0.95)
```

## Arguments

  - rho:
    
    desired intraclass correlation.

  - k:
    
    number of observations per n (subject).

  - n:
    
    number of subjects.

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Exactly one of the parameters `n` or `conf.width` must be passed as
NULL, and that parameter is determined from the others.

Sample size or precision is calculated according to formula 3 in Bonett
(2002), which is an approximation. Whether ICC is calculated for a
one-way or a two-way ANOVA does not matter in the approximation. As
suggested by the author, \\(5\*rho\\) is added to n, if \\(k = 2\\) and
\\(rho \\ge 7\\). This makes the assumption that there is no interaction
between rater and subject.

n is rounded up to the next whole number using `ceiling`.

## References

Bonett DG (2002). *Sample size requirements for estimating intraclass
correlations with desired precision*. Statistics in Medicine,
21:1331-1335. [doi:10.1002/sim.1108](https://doi.org/10.1002/sim.1108)

## Examples

``` r
# Bonett (2002) gives an example using 4 raters, with an ICC of 0.85 and want
# a confidence width of 0.2. Bonett calculated that a sample size of 19.2 was
# required. This can be done via
prec_icc(0.85, 4, conf.width = 0.2)
#> 
#>      sample size for intraclass correlation 
#> 
#>    rho k  n conf.width conf.level
#> 1 0.85 4 20        0.2       0.95
#> 
# note that \code{presamp} rounds up to the nearist integer.

# Bonett then goes on to estimate the width given the sample size, finding a
# value 'close to 0.2':
prec_icc(0.85, 4, 20)
#> 
#>      precision for intraclass correlation 
#> 
#>    rho k  n conf.width conf.level
#> 1 0.85 4 20  0.1954993       0.95
#> 
```
