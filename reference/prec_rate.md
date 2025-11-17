# Sample size or precision for a rate

`prec_rate` returns the sample size or the precision for the provided
rate.

## Usage

``` r
prec_rate(
  r,
  x = NULL,
  conf.width = NULL,
  conf.level = 0.95,
  method = c("score", "vs", "exact", "wald"),
  ...
)
```

## Arguments

  - r:
    
    rate or rate ratio.

  - x:
    
    number of events.

  - conf.width:
    
    precision (the full width of the confidence interval). Should not
    exceed 5 times `r`.

  - conf.level:
    
    confidence level.

  - method:
    
    The method to use to calculate precision. Exactly one method may be
    provided. Methods can be abbreviated.

  - ...:
    
    other arguments to uniroot (e.g. `tol`).

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Exactly one of the parameters `r` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

The `score`, variance stabilizing (`vs`), `exact`, and `wald` method are
implemented to calculate the rate and the precision. For few events `x`
(\<5), the exact method is recommended.

If more than one method is specified or the method is miss-specified,
the 'score' method will be used.

`uniroot` is used to solve n for the score and exact method.

## References

Barker, L. (2002) *A Comparison of Nine Confidence Intervals for a
Poisson Parameter When the Expected Number of Events is \\(\\le\\) 5*,
The American Statistician, 56:2, 85-89,
[doi:10.1198/000313002317572736](https://doi.org/10.1198/000313002317572736)

## See also

`poisson.test`

## Examples

``` r
# confidence interval width for a rate of 2.5 events per unit and 20 events,
#  using the score method
prec_rate(2.5, x = 20, met = "score")
#> 
#>      precision for a rate with score confidence interval 
#> 
#>     r     radj  x time conf.width conf.level      lwr      upr
#> 1 2.5 2.740091 20    8   2.243301       0.95 1.618441 3.861742
#> 
#> NOTE: 'x / r' units of time are needed to accumulate 'x' events.
#> 
# number of events to yield a CI width of 2.243 for a rate of 2.5 events per
#  unit and 20 events, using the score method
prec_rate(2.5, conf.width = 2.243, met = "score")
#> 
#>      sample size for a rate with score confidence interval 
#> 
#>     r    radj        x     time conf.width conf.level     lwr     upr
#> 1 2.5 2.74003 20.00514 8.002055      2.243       0.95 1.61853 3.86153
#> 
#> NOTE: 'x / r' units of time are needed to accumulate 'x' events.
#> 
# confidence interval width for a rate of 2.5 events per unit and 20 events,
#  using the exact method
prec_rate(2.5, x = 20, met = "exact")
#> 
#>      precision for a rate with exact confidence interval 
#> 
#>     r radj  x time conf.width conf.level      lwr      upr
#> 1 2.5  2.5 20    8   2.333982       0.95 1.527065 3.861047
#> 
#> NOTE: 'x / r' units of time are needed to accumulate 'x' events.
#> 
# vs and wald have the same conf.width, but different lwr and upr
prec_rate(2.5, x = 20, met = "vs")
#> 
#>      precision for a rate with vs confidence interval 
#> 
#>     r     radj  x time conf.width conf.level      lwr      upr
#> 1 2.5 2.620046 20    8   2.191306       0.95 1.524392 3.715699
#> 
#> NOTE: 'x / r' units of time are needed to accumulate 'x' events.
#> 
prec_rate(2.5, x = 20, met = "wald")
#> 
#>      precision for a rate with wald confidence interval 
#> 
#>     r radj  x time conf.width conf.level      lwr      upr
#> 1 2.5  2.5 20    8   2.191306       0.95 1.404347 3.595653
#> 
#> NOTE: 'x / r' units of time are needed to accumulate 'x' events.
#> 
```
