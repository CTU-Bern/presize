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



