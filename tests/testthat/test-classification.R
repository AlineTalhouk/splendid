context("classification")

test_that("RFE-SVM works", {
  data(hgsc)
  dat <- hgsc[1:50, 1:25]
  class <- attr(hgsc, "class.true")[1:50]
  expect_error(classification(dat, class, "svm", rfe = TRUE, sizes = 5), NA)
})
