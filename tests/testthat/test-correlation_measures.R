context("Correlation measures")

# ICC


test_that("icc", {
  rho <- .5
  k <- 3
  n <- 50
  p <- prec_icc(rho, k, n)

  expect_equal(class(p), "presize")
  expect_equal(p$rho, rho)
  expect_equal(p$k, k)
  expect_equal(p$n, n)
  expect_equal(p$conf.level, 0.95)
  expect_equivalent(p$conf.width, 0.3233102)
})

test_that("icc (Bonett example)", {
  rho <- .85
  k <- 4
  n <- 25
  p <- prec_icc(rho, k, n)

  expect_equal(class(p), "presize")
  expect_equal(p$rho, rho)
  expect_equal(p$k, k)
  expect_equal(p$n, n)
  expect_equal(p$conf.level, 0.95)
  expect_equivalent(round(p$conf.width, 1), 0.2)

  p <- prec_icc(rho, k, conf.width = .2)
  expect_equal(class(p), "presize")
  expect_equal(p$rho, rho)
  expect_equal(p$k, k)
  expect_equal(p$n, 20)
  expect_equal(p$conf.level, 0.95)
  expect_equivalent(p$conf.width, 0.2)

})




