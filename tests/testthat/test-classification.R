context("classification")

test_that("RFE-SVM works", {
  data(hgsc)
  dat <- hgsc[1:15, 1:5]
  class <- attr(hgsc, "class.true")[1:15]
  expect_error(classification(dat, class, "svm", rfe = TRUE, sizes = 5), NA)
})
