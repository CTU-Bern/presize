# Sample size or precision for an odds ratio

`prec_or` returns the sample size or the precision for the provided
proportions.

## Usage

``` r
prec_or(
  p1,
  p2,
  n1 = NULL,
  r = 1,
  conf.width = NULL,
  conf.level = 0.95,
  method = c("gart", "woolf", "indip_smooth"),
  ...
)
```

## Arguments

  - p1:
    
    risk among exposed.

  - p2:
    
    risk among unexposed.

  - n1:
    
    number of patients in exposed group.

  - r:
    
    allocation ratio (relative size of unexposed and exposed cohort
    (`n2` / `n1`)).

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level.

  - method:
    
    Exactly one of `indip_smooth` (*default*), `gart`, or `woolf`.
    Methods can be abbreviated.

  - ...:
    
    other arguments to uniroot (e.g. `tol`).

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

Exactly one of the parameters `n1` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

Woolf (`woolf`), Gart (`gart`), and Independence-smoothed logit
(`indip_smooth`) belong to a general family of adjusted confidence
intervals, adding 0 (woolf) to each cell, 0.5 (gart) to each cell, or an
adjustment for each cell based on observed data (independence-smoothed).
In gart and indip\_smooth, estimate of the CI is not possible if \\(p1 =
0\\), in which case the OR becomes 0, but the lower level of the CI is
\> 0. Further, if \\(p1 = 1\\) and \\(p2 \< 1\\), or if \\(p1 \> 0\\)
and \\(p2 = 0\\), the OR becomes \\(\\infty\\), but the upper limit of
the CI is finite. For the approximate intervals, `gart` and
`indip_smooth` are the recommended intervals (Fagerland et al. 2011).

`uniroot` is used to solve n for the woolf, gart, and indip\_smooth
method.

## References

Fagerland MW, Lydersen S, Laake P (2015). *Recommended confidence
intervals for two independent binomial proportions*. Statistical Methods
in Medical Research, 24(2):224-254.
[doi:10.1177/0962280211415469](https://doi.org/10.1177/0962280211415469)
.

## Examples

``` r
# 10\% events in one group, 15\% in the other, 200 participants total
#  (= 100 in each group), estimate confidence interval width
prec_or(p1 = .1, p2 = .15, n1 = 200/2)
#> Warning: more than one method was chosen, 'indip_smooth' will be used
#> 
#>      precision for an odds ratio with indip_smooth confidence interval 
#> 
#>    p1   p2  n1  n2 ntot r        or       lwr      upr conf.width conf.level
#> 1 0.1 0.15 100 100  200 1 0.6296296 0.2707144 1.478198   1.207484       0.95
# formula by Gart
prec_or(p1 = .1, p2 = .15, n1 = 200/2, method = "gart")
#> 
#>      precision for an odds ratio with gart confidence interval 
#> 
#>    p1   p2  n1  n2 ntot r        or       lwr      upr conf.width conf.level
#> 1 0.1 0.15 100 100  200 1 0.6296296 0.2770396 1.478456   1.201417       0.95
# formula by Woolf
prec_or(p1 = .1, p2 = .15, n1 = 200/2, method = "woolf")
#> 
#>      precision for an odds ratio with woolf confidence interval 
#> 
#>    p1   p2  n1  n2 ntot r        or       lwr      upr conf.width conf.level
#> 1 0.1 0.15 100 100  200 1 0.6296296 0.2682267 1.477979   1.209753       0.95

# 10\% odds in one group, 15\% in the other, desired CI width of 0.1,
#  estimate N
prec_or(p1 = .1, p2 = .15, conf.width = .1)
#> Warning: more than one method was chosen, 'indip_smooth' will be used
#> 
#>      sample size for an odds ratio with indip_smooth confidence interval 
#> 
#>    p1   p2       n1       n2    ntot r        or       lwr       upr conf.width
#> 1 0.1 0.15 11570.15 11570.15 23140.3 1 0.6296296 0.5816375 0.6816375        0.1
#>   conf.level
#> 1       0.95
# formula by Gart
prec_or(p1 = .1, p2 = .15, conf.width = .1, method = "gart")
#> 
#>      sample size for an odds ratio with gart confidence interval 
#> 
#>    p1   p2       n1       n2     ntot r        or      lwr      upr conf.width
#> 1 0.1 0.15 11569.79 11569.79 23139.58 1 0.6296296 0.581704 0.681704        0.1
#>   conf.level
#> 1       0.95
# formula by Woolf
prec_or(p1 = .1, p2 = .15, conf.width = .1, method = "woolf")
#> 
#>      sample size for an odds ratio with woolf confidence interval 
#> 
#>    p1   p2       n1       n2     ntot r        or       lwr       upr
#> 1 0.1 0.15 11570.28 11570.28 23140.56 1 0.6296296 0.5816118 0.6816118
#>   conf.width conf.level
#> 1        0.1       0.95
```
