data(hgsc)
dat <- hgsc[1:15, 1:5]
class <- attr(hgsc, "class.true")[1:15]

test_that("RFE-SVM works", {
  expect_error(classification(dat, class, "svm", rfe = TRUE, sizes = 5), NA)
})

test_that("RF with no tuning and NULL trees uses randomForest defaults", {
  rf_1 <- classification(dat, class, "rf", rfe = TRUE, tune = FALSE, trees = NULL, seed_alg = 1)
  expect_equal(rf_1$ntree, 500)
})

test_that("RF tuning and prediction works", {
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

test_that("LDA doesn't need tuning", {
  lda_1 <- classification(dat, class, "lda", tune = FALSE)
  lda_2 <- classification(dat, class, "lda", tune = TRUE)
  expect_equal(lda_1[1:7], lda_2$finalModel[1:7])
})

test_that("SVM tuning works", {
  svm_1 <- sink_output(classification(dat, class, "svm", tune = TRUE))
  expect_equal(nrow(svm_1$results), 25)
})

test_that("pam is reproducible", {
  pam_1 <- classification(dat, class, "pam", seed_alg = 1)
  pam_2a <- classification(dat, class, "pam", seed_alg = 2)
  pam_2b <- classification(dat, class, "pam", seed_alg = 2)

  expect_false(isTRUE(all.equal(pam_1, pam_2a)))
  expect_equal(pam_2a, pam_2b)
})

test_that("lasso and ridge are reproducible", {
  dat <- hgsc[1:150, 1:5]
  class <- attr(hgsc, "class.true")[1:150]

  lasso_1 <- classification(dat, class, "mlr_lasso", seed_alg = 1)
  lasso_2a <- classification(dat, class, "mlr_lasso", seed_alg = 2)
  lasso_2b <- classification(dat, class, "mlr_lasso", seed_alg = 2)

  expect_false(isTRUE(all.equal(lasso_1, lasso_2a)))
  expect_equal(lasso_2a, lasso_2b)

  ridge_1 <- classification(dat, class, "mlr_ridge", seed_alg = 1)
  ridge_2a <- classification(dat, class, "mlr_ridge", seed_alg = 2)
  ridge_2b <- classification(dat, class, "mlr_ridge", seed_alg = 2)

  expect_false(isTRUE(all.equal(ridge_1, ridge_2a)))
  expect_equal(ridge_2a, ridge_2b)
})
