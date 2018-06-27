context("prediction")

data(hgsc)
class <- attr(hgsc, "class.true")
dat <- data.frame(hgsc, class)
dat$class <- as.numeric(dat$class)

test_that("unsupported model object causes error", {
  set.seed(1)
  training.id <- sample(seq_along(class), replace = TRUE)
  test.id <- which(!seq_along(class) %in% training.id)
  mod <- lm(class ~ ., dat, subset = training.id)
  expect_error(prediction(mod, hgsc, test.id))
})

test_that("lda feature selection works", {
  mod <- classification(hgsc, class, "lda", rfe = TRUE, sizes = 5)
  expect_error(prediction(mod, hgsc, class = class, standardize = TRUE), NA)
})

test_that("svm feature selection works", {
  mod <- classification(hgsc, class, "svm", rfe = TRUE, sizes = 5)
  expect_error(prediction(mod, hgsc, class = class, standardize = TRUE), NA)
})

test_that("adaboost_m1 feature selection works", {
  mod <- classification(hgsc, class, "adaboost_m1", rfe = TRUE, sizes = 5)
  expect_error(prediction(mod, hgsc, class = class, standardize = TRUE), NA)
})
