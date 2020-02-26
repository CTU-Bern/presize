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


test_that("kappa", {
  props <- c(.4, .6)
  expect_error(prec_kappa(.5))
  expect_error(prec_kappa(.5, 500, n_category = 9, props = c(rep(.1,8), .2)))
  expect_error(prec_kappa(.5, 500, raters = 9, props = props))
  expect_error(prec_kappa(.5, "500"))
  expect_error(prec_kappa(.5, 500, props = c(1,2)))
  expect_error(prec_kappa(.5, 500, props = props), regexp = NA)
  expect_error(prec_kappa(.5, conf.width = .2, props = props), regexp = NA)

  kappa <- .5
  N <- c(50,100)
  pk <- prec_kappa(kappa, N, props = props)
  expect_equal(pk$kappa, kappa)
  expect_equal(pk$n, N)

  N <- 50
  pk1 <- prec_kappa(kappa, N, props = props)
  pk2 <- prec_kappa(kappa, props = props, conf.width = pk1$conf.width)
  expect_equal(pk1$n, pk2$n)
  expect_equal(pk1$conf.width, pk2$conf.width)
})


