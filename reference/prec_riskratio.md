# Sample size or precision for risk ratio

`prec_riskratio` returns the risk ratio and the sample size or the
precision for the provided proportions.

## Usage

``` r
prec_riskratio(
  p1,
  p2,
  n1 = NULL,
  r = 1,
  conf.width = NULL,
  conf.level = 0.95,
  method = c("koopman", "katz"),
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
    
    Exactly one of `koopman` (*default*), `katz`. Methods can be
    abbreviated.

  - ...:
    
    other arguments to uniroot (e.g. `tol`).

## Details

Exactly one of the parameters `n1` or `conf.width` must be passed as
NULL, and that parameter is determined from the other.

Koopman (`koopman`) provides an asymptotic score confidence interval
that is always consistent with Pearsons chi-squared test. It is the
recommended interval (Fagerland et al.).

Katz (`katz`) use a logarithmic transformation to calculate the
confidence interval. The CI cannot be computed if one of the proportions
is zero. If both proportions are 1, the estimate of the standard error
becomes zero, resulting in a CI of \[1, 1\].

`uniroot` is used to solve n for the katz, and koopman method.

## References

Fagerland MW, Lydersen S, and Laake P (2015). *Recommended confidence
intervals for two independent binomial proportions*, Statistical methods
in medical research 24(2):224-254.

Katz D, Baptista J, Azen SP, and Pike MC (1978) *Obtaining Confidence
Intervals for the Risk Ratio in Cohort Studies*, Biometrics 34:469-474.

Koopman PAR (1984) *Confidence Intervals for the Ratio of Two Binomial
Proportions*, Biometrics 40:513-517.

## Examples

``` r
# Validate function with example in Fagerland et al. (2015), Table 5.
prec_riskratio(p1 = 7/34, p2 = 1/34, n1 = 34, r = 1, met = "katz")
#> 
#>      precision for a relative risk with katz confidence interval 
#> 
#>          p1         p2 n1 n2 ntot r rr       lwr      upr conf.width conf.level
#> 1 0.2058824 0.02941176 34 34   68 1  7 0.9096055 53.86951    52.9599       0.95
# 7 (0.91 to 54)
prec_riskratio(p1 = 7/34, p2 = 1/34, n1 = 34, r = 1, met = "koopman")
#> 
#>      precision for a relative risk with koopman confidence interval 
#> 
#>          p1         p2 n1 n2 ntot r rr      lwr      upr conf.width conf.level
#> 1 0.2058824 0.02941176 34 34   68 1  7 1.220853 42.57571   41.35486       0.95
# 7 (1.21 to 43)

# Validate the Koopman method with example in Koopman (1984)
prec_riskratio(p1 = 36/40, p2 = 16/80, n1 = 40, r = 2, met = "koopman")
#> 
#>      precision for a relative risk with koopman confidence interval 
#> 
#>    p1  p2 n1 n2 ntot r  rr      lwr      upr conf.width conf.level
#> 1 0.9 0.2 40 80  120 2 4.5 2.939572 7.152241   4.212668       0.95
# 4.5 (2.94 to 7.15)
```
