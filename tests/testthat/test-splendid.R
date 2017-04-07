context("splendid")

data(hgsc)
class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]

test_that("missing algorithm parameter means use all", {
  sl_result <- splendid(hgsc, class, n = 1)
  expect_length(sl_result$model, 10)
  expect_length(sl_result$pred, 10)
  expect_equal(nrow(sl_result$eval), 220)
})

test_that("unsupported algorithm call causes error", {
  expect_error(splendid(hgsc, class, n = 1, algorithms = "ridge"))
})

test_that("number of best algorithms equals number of bootstraps reps", {
  n.boot <- 2
  sl_boot <- splendid(hgsc, class, n = n.boot,
                      algorithms = c("lda", "knn", "rf"))
  expect_length(sl_boot$best.algs, n.boot)
})
