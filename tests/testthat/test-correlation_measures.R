context("Correlation measures")


test_that("errors", {
  expect_error(prec_icc(k = "foo"))
  expect_error(prec_icc(k = .5))
  expect_error(prec_icc(k = 2), "exactly one")
  expect_error(prec_icc(k = 2, n = 15, conf.width = .3), "exactly one")

  expect_error(prec_cor())
  expect_error(prec_cor(n = 15, conf.width = .2))
  expect_error(prec_cor(n = 15, r = 3))
  expect_warning(prec_cor(r = .2, conf.width = .3))
  expect_error(prec_cor(r = .2, conf.width = .3, method = "pearson"), NA)
  expect_error(prec_cor(r = .2, conf.width = .3, method = "kendall"), NA)
  expect_error(prec_cor(r = .2, conf.width = .3, method = "spearman"), NA)

  expect_error(prec_lim_agree(), "exactly one")
  expect_error(prec_lim_agree(15), NA)
  expect_error(prec_lim_agree(conf.width = 15), NA)


})



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




