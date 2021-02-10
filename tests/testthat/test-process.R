test_that("dummify converts categorical variables into one column per level", {
  ncol <- sum(purrr::map_lgl(iris, is.numeric), purrr::map_int(iris, nlevels))
  expect_length(dummify(iris), ncol)
})

test_that("specifying convert is the same as dummifying", {
  expect_equivalent(
    dummify(iris),
    splendid_process(iris, iris$Species, algorithms = "algorithm_name",
                     convert = TRUE)$data
  )
})

test_that("error message is thrown for certain algorithms", {
  expect_identical(
    iris,
    splendid_process(iris, iris$Species, algorithms = "algorithm_name",
                     convert = FALSE)$data
  )
  expect_error(splendid_process(iris, algorithms = "lda", convert = FALSE))
})

test_that("subsampling methods create different number of cases", {
  iris2 <- iris[1:130, ]
  iris_orig <- subsample(iris2, iris2$Species, sampling = "none")
  iris_up <- subsample(iris2, iris2$Species, sampling = "up", seed_samp = 1)
  iris_down <- subsample(iris2, iris2$Species, sampling = "down")
  iris_smote <- subsample(iris2, iris2$Species, sampling = "smote")

  expect_equal(nrow(iris_orig), 130)
  expect_equal(nrow(iris_up), 150)
  expect_equal(nrow(iris_down), 90)
  expect_equal(nrow(iris_smote), 630)
})
