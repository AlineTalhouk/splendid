context("evaluation")

test_that("evaluation plots can be outputted", {
  data(hgsc)
  class <- factor(stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2])
  set.seed(1)
  training.id <- sample(seq_along(class), replace = TRUE)
  test.id <- which(!seq_along(class) %in% training.id)
  mod <- classification(hgsc[training.id, ], class[training.id], "xgboost")
  pred <- prediction(mod, hgsc, test.id, class)
  expect_error(evaluation(class[test.id], pred, plot = TRUE), NA)
})
