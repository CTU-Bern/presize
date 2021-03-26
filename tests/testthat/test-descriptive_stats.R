context("descritive stats")

test_that("no errors with normal settings", {
  expect_error(prec_mean(mu = 5, sd = 2.5, n = 20),
               regexp = NA)
  expect_error(prec_mean(mu = 5, sd = 2.5, conf.width = 2.34),
               regexp = NA)

  expect_warning(prec_rate(2.5, x = 20))
  expect_warning(prec_rate(2.5, conf.width = 2.243))

  expect_error(prec_rate(2.5, x = 20, method = "score"),
               regexp = NA)
  expect_error(prec_rate(2.5, conf.width = 2.243, method = "score"),
               regexp = NA)
  expect_error(prec_rate(2.5, x = 20, method = "vs"),
               regexp = NA)
  expect_error(prec_rate(2.5, conf.width = 2.243, method = "vs"),
               regexp = NA)
  expect_error(prec_rate(2.5, x = 20, method = "exact"),
               regexp = NA)
  expect_error(prec_rate(2.5, conf.width = 2.243, method = "exact"),
               regexp = NA)
  expect_error(prec_rate(2.5, x = 20, method = "wald"),
               regexp = NA)
  expect_error(prec_rate(2.5, conf.width = 2.243, method = "wald"),
               regexp = NA)

  expect_warning(prec_prop(p = 1:9 / 10, n = 100))
  expect_warning(prec_prop(p = 1:9 / 10, conf.width = .192))

  expect_error(prec_prop(p = 1:9 / 10, n = 100,
                         method = "wilson"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, conf.width = .192,
                         method = "wilson"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, n = 100,
                         method = "ac"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, conf.width = .192,
                         method = "ac"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, n = 100,
                         method = "agresti-coull"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, conf.width = .192,
                         method = "agresti-coull"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, n = 100,
                         method = "wald"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, conf.width = .192,
                         method = "wald"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, n = 100,
                         method = "exact"),
               regexp = NA)
  expect_error(prec_prop(p = 1:9 / 10, conf.width = .192,
                         method = "exact"),
               regexp = NA)


})


test_that("errors issued", {
  expect_error(prec_mean("foo", 1))
  expect_error(prec_mean(1, "foo"))
  expect_error(prec_mean(mu = 2, sd = 1, n = 10, conf.width = .5))
  expect_error(prec_rate(2, 1, 1))
  expect_error(prec_prop(.1, conf.width = 2, method = "wilson"), "numeric in")
  expect_error(prec_prop(.1, conf.width = .2, method = "wilson"), NA)
})

test_that("warnings issued", {
  expect_warning(prec_rate(2,1))
  expect_warning(prec_rate(0,1, method = "vs"))
  expect_warning(prec_prop(.01, 20, method = "ac"))
  expect_warning(prec_prop(.01, 20, method = "wilson"), regexp = NA)
  expect_warning(prec_prop(.99, 20, method = "ac"))
  expect_warning(prec_prop(.99, 20, method = "wilson"), regexp = NA)
})


# Test: results compared to stata outputs
### mean

test_that("mean = stata", {

  #   . ciwidth onemean, probwidth(.5) width(.2)
  #   Performing iteration ...
  #   Estimated sample size for a one-mean CI
  #   Student's t two-sided CI
  # Study parameters:
  #         level =     95.00
  #      Pr_width =    0.5000
  #         width =    0.2000
  #            sd =    1.0000
  # Estimated sample size:
  #             N =       386

  ex <- prec_mean(1, 1, conf.width = .2)
  expect_equal(ex$n, 386, tolerance = 1, scale = 1)

  ex <- prec_mean(1, 1, n = 386)
  expect_equal(ex$conf.width, .2, tolerance = .001, scale = 1)

})

test_that("mean conf int = stata", {

  # . cii means 100 10 2
  #
  # Variable |        Obs        Mean    Std. Err.       [95% Conf. Interval]
  # ---------+---------------------------------------------------------------
  #          |        100          10          .2        9.603157    10.39684

  ex <- prec_mean(mu = 10 , sd = 2, n = 100, conf.level = 0.95)
  expect_equal(c(ex$lwr, ex$upr) , c(9.603157, 10.39684) , tolerance = .0001, scale = 1)

  ex <- prec_mean(mu = 10 , sd = 2, conf.width = 10.39684-9.603157, conf.level = 0.95)
  expect_equal(ex$n , 100 , tolerance = 1, scale = 1)
})

### proportion
test_that("proportions conf int wilson = stata", {

  # . cii proportions 100 50, wilson
  #
  # ------ Wilson ------
  #   Variable |        Obs  Proportion    Std. Err.       [95% Conf. Interval]
  # -----------+---------------------------------------------------------------
  #            |        100          .5         .05        .4038315    .5961685

  ex <- prec_prop(p = 0.5, n = 100, conf.level = 0.95, method = 'wilson')
  expect_equal(c(ex$lwr, ex$upr) , c(.4038315, .5961685) , tolerance = .0000001, scale = 1)

  ex <- prec_prop(p = 0.5, conf.width = .5961685-.4038315, conf.level = 0.95, method = 'wilson')
  expect_equal(ex$n , 100, tolerance = 1, scale = 1)
})

  test_that("proportions conf int agresti = stata", {

  # . cii proportions 100 50, agresti
  #
  # -- Agresti-Coull ---
  #   Variable |        Obs  Proportion    Std. Err.       [95% Conf. Interval]
  # -----------+---------------------------------------------------------------
  #            |        100          .5         .05        .4038315    .5961685

  ex <- prec_prop(p = 0.5, n = 100, conf.level = 0.95, method = 'agresti-coull')
  expect_equal(c(ex$lwr, ex$upr) , c(.4038315, .5961685) , tolerance = .0000001, scale = 1)

  ex <- prec_prop(p = 0.5, conf.width = .5961685-.4038315, conf.level = 0.95, method = 'agresti-coull')
  expect_equal(ex$n , 100 , tolerance = 1, scale = 1)
})

test_that("proportions conf int exact = stata", {

  # . cii proportions 100 50, exact
  #
  # -- Binomial Exact --
  #   Variable |        Obs  Proportion    Std. Err.       [95% Conf. Interval]
  # -----------+---------------------------------------------------------------
  #            |        100          .5         .05        .3983211    .6016789

  ex <- prec_prop(p = 0.5, n = 100, conf.level = 0.95, method = 'exact')
  expect_equal(c(ex$lwr, ex$upr) , c(.3983211, .6016789) , tolerance = .0000001, scale = 1)

  ex <- prec_prop(p = 0.5, conf.width = .6016789-.3983211, conf.level = 0.95, method = 'exact')
  expect_equal(ex$n, 100 , tolerance = 1, scale = 1)
})

test_that("proportions conf int wald = stata", {

  # . cii proportions 100 50, wald
  #
  # -- Binomial Wald ---
  #     Variable |        Obs  Proportion    Std. Err.       [95% Conf. Interval]
  # -------------+---------------------------------------------------------------
  #              |        100          .5         .05        .4020018    .5979982

  ex <- prec_prop(p = 0.5, n = 100, conf.level = 0.95, method = 'wald')
  expect_equal(c(ex$lwr, ex$upr) , c(.4020018, .5979982) , tolerance = .0000001, scale = 1)

  ex <- prec_prop(p = 0.5, conf.width = .5979982-.4020018, conf.level = 0.95, method = 'wald')
  expect_equal(ex$n , 100 , tolerance = 1, scale = 1)
})

### rate
test_that("rate conf int exact = stata", {

  # . cii means 50 10 , poisson
  #
  # -- Poisson  Exact --
  #     Variable |   Exposure        Mean    Std. Err.       [95% Conf. Interval]
  # -------------+---------------------------------------------------------------
  #              |         50          .2    .0632456        .0959078    .3678071
  #

  ex <- prec_rate(r = 0.2, x = 10, conf.level = 0.95, method = 'exact')
  expect_equal(c(ex$lwr, ex$upr) , c(.0959078, .3678071) , tolerance = .0000001, scale = 1)

  ex <- prec_rate(r = 0.2, conf.width = .3678071-.0959078, conf.level = 0.95, method = 'exact')
  expect_equal(ex$x , 10 , tolerance = 1, scale = 1)
})
