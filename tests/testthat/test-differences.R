context("Differences")


test_that("errors", {
  expect_error(prec_riskratio(.1, 1.1, r = 1, conf.width = .1))
  expect_error(prec_riskratio(1.1, .1, r = 1, conf.width = .1))
  expect_error(prec_riskratio(1.1, r = 1, conf.width = .1))
  expect_error(prec_riskratio(1.1, r = 1))
  expect_error(prec_riskratio(p1 = 36/40, p2 = 16/80,
                              n1 = 40, r = 2, met = "koopman"), NA)
  expect_error(prec_riskratio(p1 = 36/40, p2 = 16/80,
                              n1 = 40, r = 2, met = "katz"), NA)
  expect_error(prec_riskratio(p1 = 36/40, p2 = 16/80,
                              conf.width = .2, r = 2, met = "koopman"), NA)
  expect_error(prec_riskratio(p1 = 36/40, p2 = 16/80,
                              conf.width = .2, r = 2, met = "katz"), NA)
  expect_warning(prec_riskratio(p1 = 36/40, p2 = 16/80,
                              n1 = 40, r = 2))
  expect_warning(prec_riskratio(p1 = 36/40, p2 = 16/80,
                              n1 = 40, r = 2, method = "foo"))

  expect_error(prec_rateratio(rate1 = 1, rate2 = NULL))
  expect_error(prec_rateratio(rate1 = NULL, rate2 = NULL))
  expect_error(prec_rateratio(rate1 = 1, rate2 = 1))
  expect_error(prec_rateratio(20, rate1 = 1, rate2 = 1), regexp = NA)

  expect_error(prec_meandiff("foo"), "'delta'")
  expect_error(prec_meandiff(2, "foo"), "'sd1'")
  expect_error(prec_meandiff(2, 1, "foo"), "'sd2'")
  expect_error(prec_meandiff(2, 1, r = "foo"), "'r'")
  expect_error(prec_meandiff(2, 1, r = 2), "one of 'n', and 'conf.width'")
  expect_error(prec_meandiff(2, 1, n = 50, variance = "equal"), NA)
  expect_error(prec_meandiff(2, 1, n = 50, variance = "unequal"), NA)
  expect_message(prec_meandiff(2, 1, n = 50))
  expect_error(prec_meandiff(2, 1, conf.width = .5, variance = "equal"), NA)
  expect_error(prec_meandiff(2, 1, conf.width = .5, variance = "unequal"), NA)
  expect_message(prec_meandiff(2, 1, 2, conf.width = .5),
                 "Variance was changed")
  expect_error(prec_meandiff(2, 1, 2, conf.width = .5, variance = "foo"),
               "variance is not correctly specified")
  expect_message(prec_meandiff(2, 1, 2, conf.width = .5, variance = "equal"),
                 "equal variance was chosen")
  expect_error(prec_meandiff(2, .5, n1 = 50, variance = "equal"), NA)
  expect_error(prec_meandiff(2, .5, conf.width = .1, variance = "equal"), NA)
  expect_error(prec_meandiff(2, .5, n1 = 50, variance = "unequal"), NA)
  expect_error(prec_meandiff(2, .5, conf.width = .1, variance = "unequal"), NA)

  expect_error(prec_riskdiff())
  expect_error(prec_riskdiff(n = 100))
  expect_error(prec_riskdiff(n = 100, p1 = 2), "numeric in")
  expect_error(prec_riskdiff(n = 100, p1 = .2))
  expect_error(prec_riskdiff(n = 100, p1 = .2, p2 = 2), "numeric in")
  expect_warning(prec_riskdiff(n = 100, p1 = .2, p2 = .4))
  expect_error(prec_riskdiff(n = 100, p1 = .2, p2 = .4, method = "new"), NA)
  expect_error(prec_riskdiff(n = 100, p1 = .2, p2 = .4, method = "mn"), NA)
  expect_error(prec_riskdiff(n = 100, p1 = .2, p2 = .4, method = "ac"), NA)
  expect_error(prec_riskdiff(n = 100, p1 = .2, p2 = .4, method = "wald"), NA)
  expect_error(prec_riskdiff(conf.width = .1,
                             p1 = .2, p2 = .4, method = "new"), NA)
  expect_error(prec_riskdiff(conf.width = .1,
                             p1 = .2, p2 = .4, method = "mn"), NA)
  expect_error(prec_riskdiff(conf.width = .1,
                             p1 = .2, p2 = .4, method = "ac"), NA)
  expect_error(prec_riskdiff(conf.width = .1,
                             p1 = .2, p2 = .4, method = "wald"), NA)
  expect_warning(prec_riskdiff(n = 100, p1 = .2, p2 = .4, method = "foo"), "Method 'foo' is not available")

  expect_error(prec_or(), "exactly one")
  expect_error(prec_or(n1 = 50), "p1")
  expect_error(prec_or(n1 = 50, p1 = 2), "p1' must be numeric in")
  expect_error(prec_or(n1 = 50, p1 = .5), "p2")
  expect_error(prec_or(n1 = 50, p1 = .5, p2 = 2), "p2' must be numeric in")
  expect_warning(prec_or(n1 = 50, p1 = .5, p2 = .8))
  expect_error(prec_or(n1 = 50, p1 = .5, p2 = .8, method = "indip_smooth"), NA)
  expect_error(prec_or(n1 = 50, p1 = .5, p2 = .8, method = "woolf"), NA)
  expect_error(prec_or(n1 = 50, p1 = .5, p2 = .8, method = "gart"), NA)
  expect_warning(prec_or(conf.width = .2, p1 = .5, p2 = .8))
  expect_error(prec_or(conf.width = .2, p1 = .5, p2 = .8,
                       method = "indip_smooth"), NA)
  expect_error(prec_or(conf.width = .2, p1 = .5, p2 = .8, method = "woolf"), NA)
  expect_error(prec_or(conf.width = .2, p1 = .5, p2 = .8, method = "gart"), NA)
  expect_warning(prec_or(conf.width = .2, p1 = .5, p2 = .8, method = "foo"), "Method 'foo' is not available")

})

### Mean difference
test_that("mean diff n = stata", {
  
  #     . ciwidth twomeans 0 5, probwidth(0.5) width(.5) sd(3)
  #     note: command arguments (control- and experimental-group mean estimates) do not
  #     affect computations and are used only for display
  #     
  #     Performing iteration ...
  #     
  #     Estimated sample sizes for a two-means-difference CI
  #     Student's t two-sided CI assuming sd1 = sd2 = sd
  # 
  # Study parameters:
  # 
  #         level =     95.00
  #      Pr_width =    0.5000
  #         width =    0.5000
  #            m1 =    0.0000
  #            m2 =    5.0000
  #          diff =    5.0000
  #            sd =    3.0000
  # 
  # Estimated sample sizes:
  # 
  #             N =     2,216
  #   N per group =     1,108
  
  ex <- prec_meandiff(delta = 5, sd1 = 3, sd2 = 3, r = 1, conf.width = .5, conf.level = 0.95, variance = 'equal')
  expect_equal(ex$n1 , 1108 , tolerance = 1, scale = 1)

  ex <- prec_meandiff(delta = 5, sd1 = 3, sd2 = 3, r = 1, n = 1108, conf.level = 0.95, variance = 'equal')
  expect_equal(ex$conf.width , .5 , tolerance = .001, scale = 1)
})

test_that("mean diff n = stata", {
  
  # . ciwidth twomeans 0 5, width(.5) sd1(3) sd2(2) knownsds
  # note: command arguments (control- and experimental-group mean estimates) do not
  # affect computations and are used only for display
  # 
  # Estimated sample sizes for a two-means-difference CI
  # Normal two-sided CI
  # 
  # Study parameters:
  #   
  #   level =     95.00
  # width =    0.5000
  # m1 =    0.0000
  # m2 =    5.0000
  # diff =    5.0000
  # sd1 =    3.0000
  # sd2 =    2.0000
  # 
  # Estimated sample sizes:
  #   
  #   N =     1,600
  # N per group =       800
  
  ex <- prec_meandiff(delta = 5, sd1 = 3, sd2 = 2, r = 1, conf.width = 0.5, conf.level = 0.95, variance = 'equal')
  expect_equal(ex$n1 , 800, tolerance = 3, scale = 1)
  
  ex <- prec_meandiff(delta = 5, sd1 = 3, sd2 = 2, r = 1, n = 800, conf.level = 0.95, variance = 'equal')
  expect_equal(ex$conf.width , .5, tolerance = .001, scale = 1)
})

test_that("mean diff conf int = stata", {
  
  #     . ciwidth twomeans 0 5, n(160) sd(2) probwidth(0.5)
  #     note: command arguments (control- and experimental-group mean estimates) do not
  #     affect computations and are used only for display
  #     
  #     Estimated width for a two-means-difference CI
  #     Student's t two-sided CI assuming sd1 = sd2 = sd
  # 
  # Study parameters:
  # 
  #         level =     95.00
  #             N =       160
  #   N per group =        80
  #      Pr_width =    0.5000
  #            m1 =    0.0000
  #            m2 =    5.0000
  #          diff =    5.0000
  #            sd =    2.0000
  # 
  # Estimated width:
  # 
  #         width =    1.2465
  
  ex <- prec_meandiff(delta = 10, sd1 = 2, sd2 = 2, r = 1, n = 80, conf.level = 0.95, variance = 'equal')
  expect_equal(ex$conf.width , 1.2465 , tolerance = .05, scale = 1)
  
  ex <- prec_meandiff(delta = 10, sd1 = 2, sd2 = 2, r = 1, conf.width = 1.2465, conf.level = 0.95, variance = 'equal')
  expect_equal(ex$n1 , 80 , tolerance = 2, scale = 1)
})


### Risk difference

test_that("risk diff conf int ac = stata", {
  
  # . rdcii 45 15 105 135
  # 
  # Confidence intervals for risk difference
  # 
  # Risk for unexposed (p0): 0.100
  # Risk for exposed (p1): 0.300
  # Risk difference (p1 - p0): 0.200
  # 
  # -----------------------------------------
  #   Method      [95% Conf. Interval]
  # -----------------------------------------
  # Agresti-Caffo         0.110         0.285
  # Newcombe Method 10    0.111         0.287
  # Wallenstein           0.110         0.285
  # Miettinen-Nurminen    0.112         0.289
  # . disp r(lb_ac)
  # .10964857
  # 
  # . disp r(ub_ac)
  # .28529424

  ex <- prec_riskdiff(p1 = 0.3, p2 = 0.1, r = 1, n1 = 150, conf.level = 0.95, method = 'ac')
  expect_equal(c(ex$lwr, ex$upr) , c(.10964857, .28529424), tolerance = .01, scale = 1)
  
  ex <- prec_riskdiff(p1 = 0.3, p2 = 0.1, r = 1, conf.width = .28529424-.10964857, conf.level = 0.95, method = 'ac')
  expect_equal(ex$n1, 150, tolerance = 1, scale = 1)
})  

test_that("risk diff conf int newcombe = stata", {
  
  # . rdcii 45 15 105 135
  # 
  # Confidence intervals for risk difference
  # 
  # Risk for unexposed (p0): 0.100
  # Risk for exposed (p1): 0.300
  # Risk difference (p1 - p0): 0.200
  # 
  # -----------------------------------------
  #   Method      [95% Conf. Interval]
  # -----------------------------------------
  # Agresti-Caffo         0.110         0.285
  # Newcombe Method 10    0.111         0.287
  # Wallenstein           0.110         0.285
  # Miettinen-Nurminen    0.112         0.289
  # . disp r(lb_ne)
  # .11065088
  # 
  # . disp r(ub_ne)
  # .28658921
  
  ex <- prec_riskdiff(p1 = 0.3, p2 = 0.1, r = 1, n1 = 150, conf.level = 0.95, method = 'newcombe')
  expect_equal(c(ex$lwr, ex$upr) , c(.11065088, .28658921), tolerance = .001, scale = 1)
  
  ex <- prec_riskdiff(p1 = 0.3, p2 = 0.1, r = 1, conf.width = .28658921-.11065088, conf.level = 0.95, method = 'newcombe')
  expect_equal(ex$n1, 150, tolerance = 1, scale = 1)
})  

test_that("risk diff conf int nm = stata", {
  
  # .   rdcii 45 15 105 135
  # 
  # Confidence intervals for risk difference
  # 
  # Risk for unexposed (p0): 0.100
  # Risk for exposed (p1): 0.300
  # Risk difference (p1 - p0): 0.200
  # 
  # -----------------------------------------
  #   Method      [95% Conf. Interval]
  # -----------------------------------------
  #   Agresti-Caffo       0.110         0.285
  # Newcombe Method 10    0.111         0.287
  # Wallenstein           0.110         0.285
  # Miettinen-Nurminen    0.112         0.289
  # . disp r(lb_mn)
  # .11200421
  # 
  # . disp r(ub_mn)
  # .28854354
  
  ex <- prec_riskdiff(p1 = 0.3, p2 = 0.1, r = 1, n1 = 150, conf.level = 0.95, method = 'mn')
  expect_equal(c(ex$lwr, ex$upr) , c(.11200421, .28854354), tolerance = .001, scale = 1)
  
  ex <- prec_riskdiff(p1 = 0.3, p2 = 0.1, r = 1, conf.width = .28854354-.11200421, conf.level = 0.95, method = 'mn')
  expect_equal(ex$n1, 150, tolerance = 1, scale = 1)
})
# "Wald not tested"
# prec_riskdiff(p1 = 0.3, p2 = 0.1, r = 1, n1 = 150, conf.level = 0.95, method = 'wald')

### Odds ratio
test_that("odds ratio conf int woolf = stata", {
  
  # Ref. Fagerland 2015
  # Confidence interval
  #                           Lower Upper  Length
  #Woolf logit                 0.99    74    4.31
  #Gart adjusted logit         0.98    38    3.65
  #Independence-smoothed logit 0.99    60    4.11
  
  # . csi 7 1 27 33 , or woolf
  # 
  #                  |   Exposed   Unexposed  |      Total
  #         ---------+------------------------+------------
  #            Cases |         7           1  |          8
  #         Noncases |        27          33  |         60
  #         ---------+------------------------+------------
  #            Total |        34          34  |         68
  #                  |                        |
  #             Risk |  .2058824    .0294118  |   .1176471
  #                  |                        |
  #                  |      Point estimate    |    [95% Conf. Interval]
  #                  |------------------------+------------------------
  #  Risk difference |         .1764706       |    .0291694    .3237718 
  #       Risk ratio |                7       |    .9096055    53.86951 
  #  Attr. frac. ex. |         .8571429       |   -.0993777    .9814366 
  #  Attr. frac. pop |              .75       |
  #       Odds ratio |         8.555556       |    .9904903     73.9003 (Woolf)
  #   +-------------------------------------------------
  #   chi2(1) =     5.10  Pr>chi2 = 0.0239
  # 
  
  ex <- prec_or(p1 = 7/34, p2 = 1/34, n1 = 34, r = 1, conf.width = NULL, conf.level = 0.95, method = "woolf")
  expect_equal(c(ex$lwr, ex$upr) , c(0.99, 74), tolerance = .1, scale = 1)
  expect_equal(c(ex$lwr, ex$upr) , c(.9904903, 73.9003), tolerance = .0001, scale = 1)
  
  ex <- prec_or(p1 = 7/34, p2 = 1/34, r = 1, conf.width = 73.9003-.9904903, conf.level = 0.95, method = "woolf")
  expect_equal(ex$n1 , 34, tolerance = 1, scale = 1)
})

test_that("odds ratio conf int gart = stata", {
  
  # Ref. Fagerland 2015
  # Confidence interval
  #                           Lower Upper  Length
  #Woolf logit                 0.99    74    4.31
  #Gart adjusted logit         0.98    38    3.65
  #Independence-smoothed logit 0.99    60    4.11  
  
  ex <- prec_or(p1 = 7/34, p2 = 1/34, n1 = 34, r = 1, conf.width = NULL, conf.level = 0.95, method = "gart")
  expect_equal(ex$lwr , 0.98, tolerance = .01, scale = 1)
  expect_equal(ex$upr , 38, tolerance = .5, scale = 1)
  
  ex <- prec_or(p1 = 7/34, p2 = 1/34, r = 1, conf.width = 38-.98, conf.level = 0.95, method = "gart")
  expect_equal(ex$n1 , 34, tolerance = 1, scale = 1)
  
})

test_that("odds ratio conf int indip smooth = stata", {
  
    # Ref. Fagerland 2015
    # Confidence interval
    #                           Lower Upper  Length
    #Woolf logit                 0.99    74    4.31
    #Gart adjusted logit         0.98    38    3.65
    #Independence-smoothed logit 0.99    60    4.11
  
  ex <- prec_or(p1 = 7/34, p2 = 1/34, n1 = 34, r = 1, conf.width = NULL, conf.level = 0.95, method = "indip_smooth")
  expect_equal(ex$lwr , 0.99, tolerance = .01, scale = 1)
  expect_equal(ex$upr , 60, tolerance = .5, scale = 1)
  
  ex <- prec_or(p1 = 7/34, p2 = 1/34, r = 1, conf.width = 60-.99, conf.level = 0.95, method = "indip_smooth")
  expect_equal(ex$n1 , 34, tolerance = 1, scale = 1)
})

test_that("Fagerland et al. (2015), Table 5", {
  p1 <- 7/34
  p2 <- 1/34
  n1 <- 34
  r <- 1
  method <- "katz"
  p <- prec_riskratio(p1 = p1, p2 = p2, n1 = n1, r = r, method = method)
  
  expect_equal(class(p), "presize")
  expect_equal(p$p1, p1)
  expect_equal(p$p2, p2)
  expect_equal(p$n1, n1)
  expect_equal(p$conf.level, 0.95)
  expect_equivalent(p$rr, 7)
  expect_equivalent(round(p$lwr, 2), .91)
  expect_equivalent(round(p$upr, 0), 54)
  
  p1 <- 7/34
  p2 <- 1/34
  n1 <- 34
  r <- 1
  method <- "koopman"
  p <- prec_riskratio(p1 = p1, p2 = p2, n1 = n1, r = r, method = method)
  
  expect_equal(class(p), "presize")
  expect_equal(p$p1, p1)
  expect_equal(p$p2, p2)
  expect_equal(p$n1, n1)
  expect_equal(p$conf.level, 0.95)
  expect_equivalent(p$rr, 7)
  expect_equivalent(round(p$lwr, 1), 1.2)
  expect_equivalent(round(p$upr, 0), 43)
  
})

context("rateratio")

test_that("inverse", {
  x <- prec_rateratio(20, rate1 = 1, rate2 = 1)
  y <- prec_rateratio(rate1 = 1, rate2 = 1, prec.level = 3.454197)
  expect_equal(x[1:11], y[1:11], tolerance = .05, scale = 1)
})

test_that("rateratio = stata outputs", {
  
  # . iri 30 60 54308 51477
  # 
  #                  |   Exposed   Unexposed  |      Total
  # -----------------+------------------------+------------
  #            Cases |        30          60  |         90
  #      Person-time |     54308       51477  |     105785
  #      ------------+------------------------+------------
  #                  |                        |
  #   Incidence rate |  .0005524    .0011656  |   .0008508
  #                  |                        |
  #                  |      Point estimate    |    [95% Conf. Interval]
  #                  |------------------------+------------------------
  #  Inc. rate diff. |        -.0006132       |   -.0009682   -.0002581 
  #  Inc. rate ratio |         .4739357       |    .2951285    .7464182 (exact)
  #  Prev. frac. ex. |         .5260643       |    .2535818    .7048715 (exact)
  #  Prev. frac. pop |         .2700714       |
  #                  +-------------------------------------------------
  #   (midp)   Pr(k<=30) =                    0.0003 (exact)
  #   (midp) 2*Pr(k<=30) =                    0.0006 (exact)
  
  ex <- prec_rateratio(n1 = 54308, rate1 = 30/54308, rate2 = 60/51477, r = 51477/54308)
  expect_equal(c(ex$lwr, ex$upr), c(.2951285, .7464182) , tolerance = .05, scale = 1)

  ex <- prec_rateratio(rate1 = 30/54308, rate2 = 60/51477, prec.level = 2.4, r = 51477/54308)
  expect_equal(ex$n1, 54308, tolerance = 150, scale = 1) # 150 patient-year difference is less than 0.3% difference
  
})

