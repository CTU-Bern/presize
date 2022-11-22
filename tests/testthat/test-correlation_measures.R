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


test_that("Limit of agreement, Bland Altman - The Lancet 1986 - example", {
  id <- c(1:17)
  n  <- length(id)

  group1 <- large <- c(494, 395, 516, 434, 476, 557, 413, 442, 650, 433, 417, 656, 267, 478, 178, 423, 427)
  group2 <- mini <-  c(512, 430, 520, 428, 500, 600, 364, 380, 658, 445, 432, 626, 260, 477, 259, 350, 451)
  sd <- sqrt(var(group1-group2))
  # CI width from the papaer's example
  length.cw <- 114.3-45.1

  ex <- prec_lim_agree(n = 17)
  expect_equal(ex$conf.width , length.cw/sd , tolerance = .2, scale = 1)
  expect_equal(ex$conf.width , 2*1.96*sqrt(3/n) , tolerance = .01, scale = 1)

  # reverse
  ex <- prec_lim_agree(conf.width = 1.65)
  expect_equal(ceiling(ex$n), 17 , tolerance = 1, scale = 1)
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

# Cronbach’s alpha
expect_error(prec_cronb (k=5,calpha=1.1,n= 10,conf.level= 0.95, conf.width= NULL))
expect_error(prec_cronb (k=5,calpha=0.5,n= 10,conf.level= 0.95, conf.width= 0.2))
expect_error(prec_cronb (k=5,calpha=0.5,n= 10,conf.level= 0.95, conf.width= 0.2))
expect_error(prec_cronb (k=5,calpha=0.5,n= NULL,conf.level= 0.95, conf.width= NULL))

expect_equal(prec_cronb (k=7,calpha=0.8,n= NULL,conf.level= 0.95, conf.width= 0.1)[["n" ]],147)

