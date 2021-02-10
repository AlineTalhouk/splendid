data(hgsc)
class <- attr(hgsc, "class.true")
dat <- data.frame(hgsc, class)
dat$class <- as.numeric(as.factor(dat$class))

test_that("unsupported model object causes error", {
  set.seed(1)
  training.id <- sample(seq_along(class), replace = TRUE)
  test.id <- which(!seq_along(class) %in% training.id)
  mod <- lm(class ~ ., dat, subset = training.id)
  expect_error(prediction(mod, hgsc, test.id))
})

test_that("lda feature selection works", {
  mod <- classification(hgsc, class, "lda", rfe = TRUE, sizes = 5)
  pred <- prediction(mod, hgsc, class = class, standardize = TRUE)
  expect_output(print(pred))
})

test_that("svm feature selection works", {
  mod <- classification(iris[, -5], iris$Species, "svm", rfe = TRUE, sizes = 3)
  pred <- prediction(mod, iris[, -5], class = iris$Species, standardize = TRUE)
  expect_output(print(pred))
})

test_that("adaboost_m1 feature selection works", {
  mod <- classification(hgsc, class, "adaboost_m1", rfe = TRUE, sizes = 5)
  pred <- prediction(mod, hgsc, class = class, standardize = TRUE)
  expect_output(print(pred))
})

test_that("different output printed when `class` is NULL", {
  mod <- classification(hgsc, class, "lda")
  pred <- prediction(mod, hgsc, class = NULL)
  expect_output(print(pred), "reference class not provided")
})
