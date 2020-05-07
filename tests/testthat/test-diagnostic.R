context("Sensitivity/specificity measures")

test_that("throws error", {
  expect_error(prec_sens(1.1, 100, method = "wilson"))
  expect_error(prec_sens(.5, n = 100, ntot = 100, method = "wilson"))
  expect_error(prec_sens(.5, prev = 1.1, ntot = 100, method = "wilson"))
  expect_error(prec_sens(.5, ntot = 100, method = "wilson"))
  expect_error(prec_sens(.5, prev = 100, method = "wilson"))
  expect_message(prec_sens(.5, prev = .1, method = "wilson", conf.width = .1), NA)
  expect_message(prec_sens(.5, prev = .1, ntot = 100, method = "wilson"))
  expect_error(prec_sens(.5, n = 100, ntot = 100, prev = .6, method = "wilson"))
  expect_error(prec_spec(1.1, 100, method = "wilson"))
  expect_error(prec_spec(.5, n = 100, ntot = 100, method = "wilson"))
  expect_error(prec_spec(.5, prev = 1.1, ntot = 100, method = "wilson"))
  expect_error(prec_spec(.5, ntot = 100, method = "wilson"))
  expect_error(prec_spec(.5, prev = 100, method = "wilson"))
  expect_message(prec_spec(.5, prev = .1, method = "wilson", conf.width = .1), NA)
  expect_message(prec_spec(.5, prev = .1, ntot = 100, method = "wilson"))
  expect_error(prec_spec(.5, n = 100, ntot = 100, prev = .6, method = "wilson"))
})

test_that("rounding works", {
  x <- prec_sens(.5, prev = .6, ntot = 52, method = "wilson")
  expect_equal(x$n, 32)
  x <- prec_sens(.5, prev = .6, ntot = 52, method = "wilson", round = "floor")
  expect_equal(x$n, 31)
  x <- prec_spec(.5, prev = .6, ntot = 52, method = "wilson")
  expect_equal(x$n, 21)
  x <- prec_spec(.5, prev = .6, ntot = 52, method = "wilson", round = "floor")
  expect_equal(x$n, 20)
})

test_that("ntot + prev gives same as n", {
  x1 <- prec_sens(.5, prev = .6, ntot = 52, method = "wilson")
  x2 <- prec_sens(.5, n = 32, method = "wilson")

  expect_equal(x1$conf.width, x2$conf.width)
  expect_equal(x1$lwr, x2$lwr)
  expect_equal(x1$upr, x2$upr)
  expect_equal(x1$n, x2$n)

  x1 <- prec_spec(.5, prev = .6, ntot = 52, method = "wilson")
  x2 <- prec_spec(.5, n = 21, method = "wilson")

  expect_equal(x1$conf.width, x2$conf.width)
  expect_equal(x1$lwr, x2$lwr)
  expect_equal(x1$upr, x2$upr)
  expect_equal(x1$n, x2$n)

})



context("AUC")

test_that("errors issued", {
  expect_error(prec_auc(), "exactly one")
  expect_error(prec_auc(n = 20, prev = -1), "0, 1")
  expect_error(prec_auc(n = 20, prev = 2), "0, 1")

  expect_error(prec_auc(.75, .3, 20), NA)
  expect_error(prec_auc(.75, .3, conf.width = .2), NA)

})



context("likelihood ratio")

test_that("errors issued", {
  expect_error(prec_lr(), "exactly one")
  expect_error(prec_lr(n = 20, prev = -1), "0, 1")
  expect_error(prec_lr(n = 20, prev = 2), "0, 1")
  expect_error(prec_lr(.5, n = 20, p1 = 2, p2 = 2), "0, 1")
})

test_that("both methods give the same result", {
  ex1 <- prec_lr(.5, .6, .6, 100)
  ex2 <- prec_lr(.5, .6, .6, conf.width = .65)
  expect_equal(ex1$n, ex2$n, tolerance = 1)
  expect_equal(ex1$conf.width, ex2$conf.width, tolerance = .01)
  expect_equal(ex1$lr, ex2$lr)
  expect_equal(ex1$lwr, ex2$lwr, tolerance = .01)
  expect_equal(ex1$upr, ex2$upr, tolerance = .01)
})

test_that("Simel examples work", {
  # n1 = n2 = 73.4, n = 2*73.4, prev = .5
  # sens = .8, spec = .73 (p2 = 1-spec)
  ex1 <- prec_lr(.5, p1 = .8, p2 = .27, n = 73.4*2)
  expect_equal(ex1$lwr, 2.0, tolerance = .01)
  expect_equal(ex1$lr, 2.96, tolerance = .01)

  # n1 = n2 = 79.9, n = 2*79.9, prev = .5
  # p1 = 1-sens = .1, p2 = spec = .5
  ex2 <- prec_lr(.5, p1 = .1, p2 = .5, n = 2*79.9)
  expect_equal(ex2$upr, .4, tolerance = .0001)
  expect_equal(ex2$lr, .2, tolerance = .0001)

  # n1 = .2*n2, n2 = 98.3, n = 98.3*1.2, prev = .2
  # p1 = sens = .8, p2 = 1-spec = .27
  ex3 <- prec_lr(.2, p1 = .8, p2 = .27, n = 98.3*1.2)
  expect_equal(ex3$lwr, 2, tolerance = .01)
  expect_equal(ex3$lr, 2.96, tolerance = .01)
  expect_equal(ex3$n1/(ex3$n1 + ex3$n2), .2, tolerance = .01)
})
