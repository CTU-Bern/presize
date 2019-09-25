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
  expect_error(prec_mean(2, 1, 1))
  expect_error(prec_rate(2, 1, 1))
})

test_that("warnings issued", {
  expect_warning(prec_rate(2,1))
  expect_warning(prec_rate(0,1, method = "vs"))
  expect_warning(prec_prop(.01, 20, method = "ac"))
  expect_warning(prec_prop(.01, 20, method = "wilson"), regexp = NA)
  expect_warning(prec_prop(.99, 20, method = "ac"))
  expect_warning(prec_prop(.99, 20, method = "wilson"), regexp = NA)
})


