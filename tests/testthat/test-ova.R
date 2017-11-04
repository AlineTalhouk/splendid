context("ova")

data(hgsc)
dat <- hgsc[1:50, 1:25]
class <- attr(hgsc, "class.true")[1:50]

test_that("knn ova is same as original because it's nonparametric", {
  sl_result <- splendid_model(dat, class, n = 1, algorithms = "knn",
                              ova = TRUE)
  expect_identical(sl_result$evals$knn, sl_result$evals$ova_knn)
})

test_that("mlr_nnet and nnet ova outputs need matrix coercion", {
  sl_result <- splendid_model(dat, class, n = 1,
                              algorithms = c("mlr_nnet", "nnet"),
                              ova = TRUE)
  expect_is(sl_result$preds$ova_mlr_nnet[[1]] %@% "prob", "matrix")
  expect_is(sl_result$preds$ova_nnet[[1]] %@% "prob", "matrix")
})
