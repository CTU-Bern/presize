# Sample size and precision of sensitivity and specificity

Because sensitivity (true positives/total number of positives) and
specificity (true negatives/total number of negatives) are simple
proportions, these functions act as wrappers for `prec_prop`.

## Usage

``` r
prec_sens(
  sens,
  n = NULL,
  ntot = NULL,
  prev = NULL,
  conf.width = NULL,
  round = "ceiling",
  ...
)

prec_spec(
  spec,
  n = NULL,
  ntot = NULL,
  prev = NULL,
  conf.width = NULL,
  round = "ceiling",
  ...
)
```

## Arguments

  - sens, spec:
    
    proportions.

  - n:
    
    number of observations.

  - ntot:
    
    total sample size.

  - prev:
    
    prevalence of cases/disease (i.e. proportion of `ntot` with the
    disease).

  - conf.width:
    
    precision (the full width of the confidence interval).

  - round:
    
    string, round calculated `n` up (`ceiling`) or down (`floor`).

  - ...:
    
    options passed to prec\_prop (e.g. method, conf.width, conf.level).

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

If `ntot` and `prev` are given, they are used to calculate `n`.

## Note

Calculated `n` can take on non-integer numbers, but `prec_prop` requires
integers, so the calculated `n` is rounded according to the approach
indicated in `round`.

## See also

`prec_prop`

## Examples

``` r
  # confidence interval width with n
  prec_sens(.6, 50)
#> Warning: more than one method was chosen, 'wilson' will be used
#> 
#>      precision for a sensitivity with Wilson confidence interval. 
#> 
#>   sens   sensadj  n prev ntot conf.width conf.level       lwr       upr
#> 1  0.6 0.5928652 50   NA   NA  0.2621017       0.95 0.4618144 0.7239161
#> 
#> NOTE: sensadj is the adjusted sensitivity, from which the ci is calculated.
#>       n is the number of positives, ntot the full sample
#> 
  # confidence interval width with ntot and prevalence (assuming 50% prev)
  prec_sens(.6, ntot = 100, prev = .5)
#> estimating n from 'ntot' and 'prev'
#> Warning: more than one method was chosen, 'wilson' will be used
#> 
#>      precision for a sensitivity with Wilson confidence interval. 
#> 
#>   sens   sensadj  n prev ntot conf.width conf.level       lwr       upr
#> 1  0.6 0.5928652 50  0.5  100  0.2621017       0.95 0.4618144 0.7239161
#> 
#> NOTE: sensadj is the adjusted sensitivity, from which the ci is calculated.
#>       n is the number of positives, ntot the full sample
#> 
  # sample size with confidence interval width
  prec_sens(.6, conf.width = 0.262)
#> Warning: more than one method was chosen, 'wilson' will be used
#> 
#>      sample size for a sensitivity with Wilson confidence interval. 
#> 
#>   sens   sensadj        n prev ntot conf.width conf.level       lwr       upr
#> 1  0.6 0.5928708 50.04169   NA   NA      0.262       0.95 0.4618708 0.7238708
#> 
#> NOTE: sensadj is the adjusted sensitivity, from which the ci is calculated.
#>       n is the number of positives, ntot the full sample
#> 
```
