context("Sensitivity/specificity measures")


test_that("errors", {
  expect_error(prec_sens_spec(.1, 1.1, .1, conf.width = .1))
  expect_error(prec_sens_spec(1.1, .1, .1, conf.width = .1))
  expect_error(prec_sens_spec(.1, .1, 1.1, conf.width = .1))
  expect_error(prec_sens_spec(.1, .1, .1, conf.width = 1.1))
})

test_that("Buderer example (n)", {
  sens <- .9
  spec <- .85
  prev <- .2
  ci_width <- .1
  p <- prec_sens_spec(sens, spec, prev, conf.width = ci_width)

  expect_equal(class(p), "presize")
  expect_equal(p$sens, sens)
  expect_equal(p$spec, spec)
  expect_equal(ceiling(p$n), 173)
  expect_equal(p$conf.level, 0.95)
  expect_equivalent(p$conf.width, ci_width)
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


context("Sensitivity")

test_that("throws error", {
  expect_error(prec_sens(1.1, 100, method = "wilson"))
  expect_error(prec_sens(.5, n = 100, ntot = 100, method = "wilson"))
  expect_error(prec_sens(.5, prev = 1.1, ntot = 100, method = "wilson"))
  expect_error(prec_sens(.5, ntot = 100, method = "wilson"))
  expect_error(prec_sens(.5, prev = 100, method = "wilson"))
  expect_error(prec_sens(.5, n = 100, ntot = 100, prev = .6, method = "wilson"))
})

test_that("rounding works", {
  x <- prec_sens(.5, prev = .6, ntot = 52, method = "wilson")
  expect_equal(x$n, 32)
  x <- prec_sens(.5, prev = .6, ntot = 52, method = "wilson", round = "floor")
  expect_equal(x$n, 31)
})

test_that("ntot + prev gives same as n", {
  x1 <- prec_sens(.5, prev = .6, ntot = 52, method = "wilson")
  x2 <- prec_sens(.5, n = 32, method = "wilson")

  expect_equal(x1$conf.width, x2$conf.width)
  expect_equal(x1$lwr, x2$lwr)
  expect_equal(x1$upr, x2$upr)
  expect_equal(x1$n, x2$n)

})


