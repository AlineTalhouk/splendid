context("prediction")

data(hgsc)
class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
dat <- data.frame(hgsc, class)
dat$class <- as.numeric(dat$class)

test_that("unsupported model object causes error", {
  set.seed(1)
  training.id <- sample(seq_along(class), replace = TRUE)
  test.id <- which(!seq_along(class) %in% training.id)
  mod <- lm(class ~ ., dat, subset = training.id)
  expect_error(prediction(mod, hgsc, test.id))
})
