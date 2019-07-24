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




