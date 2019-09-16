context("classification")

data(hgsc)
dat <- hgsc[1:15, 1:5]
class <- attr(hgsc, "class.true")[1:15]

test_that("RFE-SVM works", {
  expect_error(classification(dat, class, "svm", rfe = TRUE, sizes = 5), NA)
})

test_that("RF tuning works", {
  c1 <- classification(dat, class, "rf", tune = TRUE)
  expect_is(c1, "train")

  p <- prediction(c1, dat, class = class)
  expect_is(p, "prediction")
})

test_that("RF tuning can be reproducible", {
  c2 <- classification(dat, class, "rf", tune = TRUE, seed_alg = 2, trees = 100)
  set.seed(2)
  c3 <- randomForest::randomForest(x = dat, y = factor(class), ntree = 100,
                                   mtry = c2$finalModel$mtry)

  expect_equal(c2$finalModel$votes, c3$votes)
})
