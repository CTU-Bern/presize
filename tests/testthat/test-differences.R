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

  expect_error(prec_rateratio(rate_exp = 1, rate_control = NULL))
  expect_error(prec_rateratio(rate_exp = NULL, rate_control = NULL))
  expect_error(prec_rateratio(rate_exp = 1, rate_control = 1))
  expect_error(prec_rateratio(20, rate_exp = 1, rate_control = 1), regexp = NA)

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
  x <- prec_rateratio(20, rate_exp = 1, rate_control = 1)
  y <- prec_rateratio(rate_exp = 1, rate_control = 1, conf.width = 3.454197)
  expect_equal(x[1:11], y[1:11], tolerance = .05)
})

