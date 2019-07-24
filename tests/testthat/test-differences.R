context("Differences")


test_that("errors", {
  expect_error(prec_riskratio(.1, 1.1, r = 1, conf.width = .1))
  expect_error(prec_riskratio(1.1, .1, r = 1, conf.width = .1))
  expect_error(prec_riskratio(1.1, r = 1, conf.width = .1))

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
  expect_equivalent(round(p$lwr, 2), 1.21)
  expect_equivalent(round(p$upr, 0), 43)

})


test_that("Buderer example (ci width)", {
  sens <- .9
  spec <- .85
  prev <- .2
  ci_width <- .1
  n <- 173
  p <- prec_sens_spec(sens, spec, prev, n = n)

  expect_equal(class(p), "presize")
  expect_equal(p$sens, sens)
  expect_equal(p$spec, spec)
  expect_equal(p$n, n)
  expect_equal(p$conf.level, 0.95)
  expect_equivalent(round(p$conf.width, 1), ci_width)
})




