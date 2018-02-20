context("knnflex.R")

test_that("leave one out prediction works for knn", {
  data(hgsc)
  class <- attr(hgsc, "class.true")
  id <- seq_along(class)
  mod <- classification(hgsc[id[-1], ], class[id[-1]], "knn")
  pred <- prediction(mod, hgsc, class, id[1])
  expect_length(pred, 1)
})
