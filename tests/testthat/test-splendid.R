context("splendid")

data(hgsc)
class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]

test_that("missing algorithm parameter means use all", {
  sl_result <- splendid(hgsc, class, n = 1)
  expect_length(sl_result$models, length(ALG.NAME))
  expect_length(sl_result$preds, length(ALG.NAME))
  expect_equal(sum(purrr::map_int(sl_result$evals, nrow)),
               length(ALG.NAME) * 22)
})

test_that("unsupported algorithm call causes error", {
  expect_error(splendid(hgsc, class, n = 1, algorithms = "random"))
})

test_that("number of best algorithms equals number of bootstraps reps", {
  n.boot <- 2
  sl_boot <- splendid(hgsc, class, n = n.boot,
                      algorithms = c("lda", "knn", "rf"))
  expect_length(sl_boot$bests, n.boot)
})
