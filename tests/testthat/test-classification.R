context("classification")

test_that("RFE-SVM works", {
  data(hgsc)
  dat <- hgsc[1:50, 1:25]
  class <- stringr::str_split_fixed(rownames(dat), "_", n = 2)[, 2]
  expect_error(classification(dat, class, "svm", rfe = TRUE, sizes = 5), NA)
})
