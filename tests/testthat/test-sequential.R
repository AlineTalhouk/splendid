context("sequential")

data(hgsc)
class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
sm <- splendid_model(hgsc, class, n = 2, algorithms = c("xgboost", "slda"))

test_that("sequential alg fits one fewer model than number of classes", {
  st <- sequential_train(sm, hgsc, class)
  sp <- sequential_pred(st, sm, hgsc, class)

  expect_length(st, 3)
  expect_length(sp$prob, 3)
  expect_length(sp$cm, 3)
})

test_that("boxplot showing F1-score distribution can be plotted", {
  expect_error(sequential_train(sm, hgsc, class, boxplot = TRUE), NA)
})
