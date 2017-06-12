context("ova")

data(hgsc)
dat <- hgsc[1:50, 1:25]
class <- stringr::str_split_fixed(rownames(dat), "_", n = 2)[, 2]

test_that("knn ova is same as original because it's nonparametric", {
  sl_result <- splendid_model(dat, class, n = 1, algorithms = "knn",
                              ova = TRUE)
  expect_identical(sl_result$evals$knn, sl_result$evals$ova_knn)
})

test_that("multinom_nnet and nnet ova outputs need matrix coercion", {
  sl_result <- splendid_model(dat, class, n = 1,
                              algorithms = c("multinom_nnet", "nnet"),
                              ova = TRUE)
  expect_is(sl_result$preds$ova_multinom_nnet[[1]] %@% "prob", "matrix")
  expect_is(sl_result$preds$ova_nnet[[1]] %@% "prob", "matrix")
})
