test_that("variable importance works for random forest models", {
  data(hgsc)
  class <- attr(hgsc, "class.true")
  mod <- classification(hgsc, class, "rf")
  vi_out <- var_imp(mod)
  expect_equal(nrow(vi_out), length(names(hgsc)))
})
