context("helpers")

x <- prec_mean(1:12, 1, 100)




test_that("printing works", {
  expect_error(capture.output(print(x)), regexp = NA)
  cap <- capture.output(print(x))
  expect_true(grepl("Output truncated at 10 of 12 rows", cap[length(cap)]))
})


test_that("as.data.frame works", {
  y <- as.data.frame(x)
  expect_equal(nrow(y), 12)
  expect_equal(ncol(y), 7)
  expect_equal(class(y), "data.frame")
  expect_equal(attr(y, "method"), "precision for mean")
})


test_that("as.matrix works", {
  y <- as.matrix(x)
  expect_equal(dim(y), c(12, 7))
  expect_equal(class(y), "matrix")
  expect_equal(attr(y, "method"), "precision for mean")
})

