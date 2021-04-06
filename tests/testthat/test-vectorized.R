context("vectorized")



test_that("prec_auc", {
  expect_error(prec_auc(.65, .1, n = 500), NA)
  x1 <- as.data.frame(prec_auc(.65, .1, n = 500))
  expect_equal(nrow(x1), 1)

  expect_error(prec_auc(.65, c(.1, .2), n = c(500, 600)), NA)
  x2 <- as.data.frame(prec_auc(.65, c(.1, .2), n = c(500, 600)))
  expect_equal(nrow(x2), 2)
})


test_that("prec_cor", {
  expect_error(prec_cor(.4, 400, method = "pearson"), NA)

  x1 <- as.data.frame(prec_cor(.4, 400, method = "pearson"))
  expect_equal(nrow(x1), 1)

  expect_error(prec_cor(c(.4, .45), c(400, 500), method = "pearson"), NA)
  x2 <- as.data.frame(prec_cor(c(.4, .45), c(400, 500), method = "pearson"))
  expect_equal(nrow(x2), 2)


  x <- as.data.frame(prec_cor(c(.4, .45), c(400, 500), method = "pearson"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_cor(c(.4, .45), c(400, 500), method = "kendall"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_cor(c(.4, .45), c(400, 500), method = "spearman"))
  expect_equal(nrow(x), 2)

})


test_that("prec_icc", {

  x <- as.data.frame(prec_icc(.4, 4, 50))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_icc(.4, 4, c(50, 60)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_icc(.4, c(3, 4), c(50, 60)))
  expect_equal(nrow(x), 2)

})


test_that("prec_kappa", {

  x <- as.data.frame(prec_kappa(.5, 200, props = c(.3, .7)))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_kappa(.5, c(200, 250), props = c(.3, .7)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_kappa(c(.5, .6), c(200, 250), props = c(.3, .7)))
  expect_equal(nrow(x), 2)

})


test_that("prec_lim_agree", {

  x <- as.data.frame(prec_lim_agree(50))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_lim_agree(50:60))
  expect_equal(nrow(x), 11)

})


test_that("prec_lr", {

  x <- as.data.frame(prec_lr(.3, .8, .27, 74))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_lr(.3, .8, .27, c(74, 90)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_lr(.3, c(.7, .8), .27, c(74, 90)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_lr(c(.3, .4), c(.7, .8), .27, c(74, 90)))
  expect_equal(nrow(x), 2)

})


test_that("prec_mean", {

  x <- as.data.frame(prec_mean(5, 1, 50))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_mean(5, 1, c(50, 55)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_mean(c(5, 8), 1, c(50, 55)))
  expect_equal(nrow(x), 2)

})


test_that("prec_meandiff", {

  x <- as.data.frame(prec_meandiff(5, c(2.5), n1 = c(20)))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_meandiff(5, c(2.5, 2.5, 3), n1 = c(20, 25, 25)))
  #expect_equal(nrow(x), 3)

})


test_that("prec_neg_lr", {

  x <- as.data.frame(prec_neg_lr(.3, .67, .34, 30))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_neg_lr(.3, .67, .34, c(30, 40)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_neg_lr(c(.3, .4), .67, .34, c(30, 40)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_neg_lr(.3, c(.5, .67), .34, c(30, 40)))
  expect_equal(nrow(x), 2)

})


test_that("prec_or", {

  x <- as.data.frame(prec_or(.3, .4, 20, method = "indip_smooth"))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_or(.3, .4, c(20, 30), method = "indip_smooth"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_or(.3, c(.2, .4), c(20, 30), method = "gart"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_or(.3, c(.2, .4), c(20, 30), method = "woolf"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_or(.3, c(.2, .4), c(20, 30), method = "indip_smooth"))
  expect_equal(nrow(x), 2)

})


test_that("prec_pos_lr", {

  x <- as.data.frame(prec_pos_lr(.3, .67, .34, 30))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_pos_lr(.3, .67, .34, c(30, 40)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_pos_lr(c(.3, .4), .67, .34, c(30, 40)))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_pos_lr(.3, c(.5, .67), .34, c(30, 40)))
  expect_equal(nrow(x), 2)

})



test_that("prec_prop", {
  x <- as.data.frame(prec_prop(.3, 50, method = "wilson"))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_prop(.3, 50:55, method = "wilson"))
  expect_equal(nrow(x), 6)

  x <- as.data.frame(prec_prop(c(.3, .4), c(50, 60), method = "wilson"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_prop(c(.3, .4), c(50, 60), method = "agresti-coull"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_prop(c(.3, .4), c(50, 60), method = "exact"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_prop(c(.3, .4), c(50, 60), method = "wald"))
  expect_equal(nrow(x), 2)

})

test_that("prec_rate", {
  x <- as.data.frame(prec_rate(.3, 40, method = "score"))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_rate(.3, c(40, 50), method = "score"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_rate(c(.3, .4), c(40, 50), method = "score"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_rate(c(.3, .4), c(40, 50), method = "score"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_rate(c(.3, .4), c(40, 50), method = "vs"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_rate(c(.3, .4), c(40, 50), method = "exact"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_rate(c(.3, .4), c(40, 50), method = "wald"))
  expect_equal(nrow(x), 2)

})



test_that("prec_rateratio", {
  x <- as.data.frame(prec_rateratio(20, .5, 3))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_rateratio(20, .5, c(3, 4)))
  expect_equal(nrow(x), 2)

})


test_that("prec_riskdiff", {
  x <- as.data.frame(prec_riskdiff(.5, .6, 100, method = "newcombe"))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_riskdiff(.5, c(.6, .7), 100, method = "newcombe"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_riskdiff(c(.5, .6), c(.6, .7), 100, method = "newcombe"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_riskdiff(c(.5, .6), c(.6, .7), 100, method = "mn"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_riskdiff(c(.5, .6), c(.6, .7), 100, method = "ac"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_riskdiff(c(.5, .6), c(.6, .7), 100, method = "wald"))
  expect_equal(nrow(x), 2)

})



test_that("prec_riskratio", {
  x <- as.data.frame(prec_riskratio(.5, .6, 100, method = "koopman"))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_riskratio(.5, .6, c(100, 200), method = "koopman"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_riskratio(.5, c(.6, .7), c(100, 200), method = "koopman"))
  expect_equal(nrow(x), 2)

  x <- as.data.frame(prec_riskratio(.5, c(.6, .7), c(100, 200), method = "katz"))
  expect_equal(nrow(x), 2)

})


test_that("prec_sens", {
  x <- as.data.frame(prec_sens(.7, 100, method = "wilson"))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_sens(.7, c(100, 150), method = "wilson"))
  expect_equal(nrow(x), 2)

  expect_error(prec_sens(.7, ntot = 100, method = "wilson"))

  x <- as.data.frame(prec_sens(c(.7, .8), c(100, 150), method = "wilson"))
  expect_equal(nrow(x), 2)

})


test_that("prec_sens", {
  x <- as.data.frame(prec_spec(.7, 100, method = "wilson"))
  expect_equal(nrow(x), 1)

  x <- as.data.frame(prec_spec(.7, c(100, 150), method = "wilson"))
  expect_equal(nrow(x), 2)

  expect_error(prec_spec(.7, ntot = 100, method = "wilson"))

  x <- as.data.frame(prec_spec(c(.7, .8), c(100, 150), method = "wilson"))
  expect_equal(nrow(x), 2)

})

