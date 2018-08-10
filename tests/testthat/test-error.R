context("error")

test_that(".632 and .632+ estimators depend on plus argument", {
  data(hgsc)
  class <- attr(hgsc, "class.true")
  m1 <- splendid_model(hgsc, class, n = 1, seed_boot = 1, algorithms = "xgboost", plus = FALSE)
  m2 <- splendid_model(hgsc, class, n = 1, seed_boot = 1, algorithms = "xgboost", plus = TRUE)
  expect_lte(
    attr(m1$evals$xgboost, "err_632"),
    attr(m2$evals$xgboost, "err_632plus")
  )
})
