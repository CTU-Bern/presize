# Sample size or precision for risk difference

`prec_riskdiff` returns the risk difference and the sample size or the
precision for the provided proportions.

## Usage

``` r
prec_riskdiff(
  p1,
  p2,
  n1 = NULL,
  conf.width = NULL,
  r = 1,
  conf.level = 0.95,
  method = c("newcombe", "mn", "ac", "wald"),
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

  - conf.width:
    
    precision (the full width of the confidence interval).

  - r:
    
    allocation ratio (relative size of exposed and unexposed cohort
    (`n1` / `n2`)).

  - conf.level:
    
    confidence level.

  - method:
    
    Exactly one of `newcombe` (*default*), `mn` (Miettinen-Nurminen),
    `ac` (Agresti-Caffo), `wald`. Methods can be abbreviated.

  - ...:
    
    other options to uniroot (e.g. `tol`)

## Details

Exactly one of the parameters `n1` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

Newcombe (`newcombe`) proposed a confidence interval based on the wilson
score method for the single proportion (see
[prec\_prop](https://ctu-bern.github.io/presize/reference/prec_prop.md)).
The confidence interval without continuity correction is implemented
from equation 10 in Newcombe (1998).

Miettinen-Nurminen (`mn`) provide a closed from equation for the
restricted maximum likelihood estimate . The implementation is based on
code provided by Yongyi Min on
<https://users.stat.ufl.edu/~aa/cda/R/two-sample/R2/index.html>.

Agresti-Caffo (`ac`) confidence interval is based on the Wald confidence
interval, adding 1 success to each cell of the 2 x 2 table (see Agresti
and Caffo 2000).

`uniroot` is used to solve n for the newcombe, ac, and mn method.

## References

Agresti A (2003) *Categorical Data Analysis*, Second Edition, Wiley
Series in Probability and Statistics,
[doi:10.1002/0471249688](https://doi.org/10.1002/0471249688) .

Agresti A and Caffo B (2000) *Simple and Effective Confidence Intervals
for Proportions and Differences of Proportions Result from Adding Two
Successes and Two Failures*, The American Statistician, 54(4):280-288.

Miettinen O and Nurminen M (1985) *Comparative analysis of two rates*,
Statistics in Medicine, 4:213-226.

Newcombe RG (1998) *Interval estimation for the difference between
independent proportions: comparison of eleven methods*, Statistics in
Medicine, 17:873-890.

Fagerland MW, Lydersen S, and Laake P (2015). *Recommended confidence
intervals for two independent binomial proportions*, Statistical methods
in medical research 24(2):224-254.

## Examples

``` r
# proportions of 40 and 30\%, 50 participants, how wide is the CI?
prec_riskdiff(p1 = .4, p2 = .3, n1 = 50)
#> Warning: more than one method was chosen, 'newcombe' will be used
#> 
#>      precision for a risk difference with newcombe confidence interval 
#> 
#>    p1  p2 n1 n2 ntot r delta         lwr       upr conf.width conf.level
#> 1 0.4 0.3 50 50  100 1   0.1 -0.08510109 0.2759788  0.3610798       0.95
# proportions of 40 and 30\%, 50 participants, how many participants for a CI 0.2 wide?
prec_riskdiff(p1 = .4, p2 = .3, conf.width = .2)
#> Warning: more than one method was chosen, 'newcombe' will be used
#> 
#>      sample size for a risk difference with newcombe confidence interval 
#> 
#>    p1  p2       n1       n2     ntot r delta          lwr       upr conf.width
#> 1 0.4 0.3 169.8394 169.8394 339.6787 1   0.1 -0.001408915 0.1985911        0.2
#>   conf.level
#> 1       0.95

# Validate Newcombe (1998)
prec_riskdiff(p1 = 56/70, p2 = 48/80, n1 = 70, r = 70/80, met = "newcombe")  # Table IIa
#> 
#>      precision for a risk difference with newcombe confidence interval 
#> 
#>    p1  p2 n1 n2 ntot     r delta        lwr       upr conf.width conf.level
#> 1 0.8 0.6 70 80  150 0.875   0.2 0.05243147 0.3338727  0.2814412       0.95
prec_riskdiff(p1 = 10/10, p2 = 0/10, n1 = 10, met = "newcombe")  # Table IIh
#> 
#>      precision for a risk difference with newcombe confidence interval 
#> 
#>   p1 p2 n1 n2 ntot r delta       lwr upr conf.width conf.level
#> 1  1  0 10 10   20 1     1 0.6075094   1  0.3924906       0.95

# multiple scenarios
prec_riskdiff(p1 = c(56/70, 9/10, 6/7, 5/56),
              p2 = c(48/80, 3/10, 2/7, 0/29),
              n1 = c(70, 10, 7, 56),
              r = c(70/80, 1, 1, 56/29),
              method = "wald")
#> 
#>      precision for a risk difference with wald confidence interval 
#> 
#>           p1        p2 n1 n2 ntot        r      delta        lwr       upr
#> 1 0.80000000 0.6000000 70 80  150 0.875000 0.20000000 0.05750490 0.3424951
#> 2 0.90000000 0.3000000 10 10   20 1.000000 0.60000000 0.26052428 0.9394757
#> 3 0.85714286 0.2857143  7  7   14 1.000000 0.57142857 0.14811614 0.9947410
#> 4 0.08928571 0.0000000 56 29   85 1.931034 0.08928571 0.01460024 0.1639712
#>   conf.width conf.level
#> 1  0.2849902       0.95
#> 2  0.6789514       0.95
#> 3  0.8466249       0.95
#> 4  0.1493709       0.95
```
