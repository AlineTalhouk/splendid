test_that("standardizing ignores dummy variables", {
  data <- iris
  class <- iris$Species
  expect_error(splendid_model(data, class, "xgboost", convert = FALSE))
  expect_error(splendid_model(data, class, "xgboost", convert = TRUE,
                              standardize = TRUE), NA)
  expect_error(splendid_model(data[, -5], class, "xgboost", convert = TRUE,
                              standardize = TRUE), NA)
})
