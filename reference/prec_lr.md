# Sample size or precision for likelihood ratios

These functions calculate the precision or sample size for likelihood
ratios (LRs). `prec_lr` is a generalized method for that can be used for
positive and negative LRs as well as conditional LRs.

`prec_pos_lr` is a wrapper to `prec_lr` to ease calculations for
positive likelihood ratios by allowing sensitivity and specificity to be
given explicitly.

`prec_neg_lr` is a wrapper to `prec_lr` to ease calculations for
negative likelihood ratios by allowing sensitivity and specificity to be
given explicitly.

## Usage

``` r
prec_lr(prev, p1, p2, n = NULL, conf.width = NULL, conf.level = 0.95, ...)

prec_pos_lr(
  prev,
  sens,
  spec,
  n = NULL,
  conf.width = NULL,
  conf.level = 0.95,
  ...
)

prec_neg_lr(
  prev,
  sens,
  spec,
  n = NULL,
  conf.width = NULL,
  conf.level = 0.95,
  ...
)
```

## Arguments

  - prev:
    
    disease/case prevalence in the study group.

  - p1:
    
    proportion of positives in group 1 (e.g. sensitivity).

  - p2:
    
    proportion of positives in group 2 (e.g. 1 - specificity).

  - n:
    
    total group size.

  - conf.width:
    
    precision (the full width of the confidence interval).

  - conf.level:
    
    confidence level (defaults to 0.95).

  - ...:
    
    other arguments to uniroot (e.g. `tol`).

  - sens:
    
    sensitivity.

  - spec:
    
    specificity.

## Value

Object of class "presize", a list of arguments (including the computed
one) augmented with method and note elements.

## Details

These functions implement formula 10 from Simel et al 1991. `prec_lr` is
a generalized function allowing for many scenarios, while `prec_pos_lr`
and `prec_neg_lr` are specific to positive and negative likelihood
ratios in the 2\*2 setting (e.g. disease status and test
positive/negative).

For the positive likelihood ratio (LR+), in a 2x2 style experiment, `p1`
should be sensitivity, `p2` should be 1-specificity. Alternatively, use
`prec_pos_lr`.

For the negative likelihood ratio (LR-), in a 2x2 style experiment, `p1`
should be 1-sensitivity, `p2` should be specificity. Alternatively, use
`prec_neg_lr`.

For conditional likelihood ratios with 3x2 tables, such as positive or
negative tests against inconclusive ones (yields), `p1` would be the
proportion of positive or negative tests in the diseased group and `p2`
would be the proportion of positive or negative tests in the
non-diseased group.

## Functions

  - `prec_pos_lr()`: "Positive likelihood ratio"

  - `prec_neg_lr()`: "Negative likelihood ratio"

## References

Simel, DL, Samsa, GP and Matchar, DB (1991) *Likelihood ratios with
confidence: Sample size estimation for diagnostic test studies.* J Clin
Epidemiol 44(8), 763-770

## Examples

``` r
# equal numbers of diseased/non-diseased, 80% sens, 73% spec, 74 participants total
prec_lr(.5, .8, .27, 74)
#> 
#>      precision for likelihood ratios 
#> 
#>   prev  p1   p2  n n1 n2       lr      lwr     upr conf.width conf.level
#> 1  0.5 0.8 0.27 74 37 37 2.962963 1.703046 5.15497   3.451925       0.95

# Simel et al 1991, problem 1 - LR+ CI width from N
# Sensitivity of a new test is at least 80%, specificity is 73% and the LR+
# is 2.96 (= 0.8/(1-0.73)). We have as many diseased as not diseased
# (n1 = n2, n = 2*n1 = 146.8, prevalence = .5)
prec_lr(prev = .5, p1 = .8, p2 = 1-.73, n = 146.8)
#> 
#>      precision for likelihood ratios 
#> 
#>   prev  p1   p2     n   n1   n2       lr      lwr      upr conf.width
#> 1  0.5 0.8 0.27 146.8 73.4 73.4 2.962963 1.999739 4.390147   2.390407
#>   conf.level
#> 1       0.95
prec_pos_lr(prev = .5, sens = .8, spec = .73, n = 146.8)
#> 
#>      precision for positive likelihood ratio 
#> 
#>   prev  p1   p2     n   n1   n2       lr      lwr      upr conf.width
#> 1  0.5 0.8 0.27 146.8 73.4 73.4 2.962963 1.999739 4.390147   2.390407
#>   conf.level
#> 1       0.95

# problem 1 of Simel et al actually derives n1 rather than the width of the
# confidence interval (ie N from CI width). If we know that the lower limit
# of the CI should be 2.0, the confidence interval width is approximately
# exp(2*(log(2.96) - log(2))) = 2.19 (approximate because the CI Of the LR
# is only symetrical on the log(LR) scale), which we can put in conf.width
prec_lr(prev = .5, p1 = .8, p2 = 1-.73, conf.width = 2.2)
#> 
#>      sample size for likelihood ratios 
#> 
#>   prev  p1   p2        n       n1       n2       lr      lwr      upr
#> 1  0.5 0.8 0.27 172.0183 86.00915 86.00915 2.962963 2.060562 4.260562
#>   conf.width conf.level
#> 1        2.2       0.95
# same, but using the wrapper to specify sens and spec
prec_pos_lr(prev = .5, sens = .8, spec = .73, conf.width = 2.2)
#> 
#>      sample size for positive likelihood ratio 
#> 
#>   prev  p1   p2        n       n1       n2       lr      lwr      upr
#> 1  0.5 0.8 0.27 172.0183 86.00915 86.00915 2.962963 2.060562 4.260562
#>   conf.width conf.level
#> 1        2.2       0.95

# Simel et al 1991, problem 2 - LR- CI width from N
# p1 = 1 - sens = .1, p2 = spec = .5
# n1 = n2, n = 160, prev = .5
prec_lr(prev = .5, p1 = .1, p2 = .5, n = 160)
#> 
#>      precision for likelihood ratios 
#> 
#>   prev  p1  p2   n n1 n2  lr       lwr       upr conf.width conf.level
#> 1  0.5 0.1 0.5 160 80 80 0.2 0.1000195 0.3999219  0.2999024       0.95
# same, but using the wrapper to specify sens and spec
prec_neg_lr(prev = .5, sens = .9, spec = .5, n = 160)
#> 
#>      precision for negative likelihood ratio 
#> 
#>   prev  p1  p2   n n1 n2  lr       lwr       upr conf.width conf.level
#> 1  0.5 0.1 0.5 160 80 80 0.2 0.1000195 0.3999219  0.2999024       0.95
```
