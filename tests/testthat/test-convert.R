context("convert")

test_that("dummify converts categorical variables into one column per level", {
  ncol <- sum(purrr::map_lgl(iris, is.numeric), purrr::map_int(iris, nlevels))
  expect_length(dummify(iris), ncol)
})

test_that("specifying convert is the same as dummifying", {
  expect_identical(
    dummify(iris),
    splendid_convert(iris, algorithms = "algorithm_name", convert = TRUE)
  )
})

test_that("error message is thrown for certain algorithms", {
  expect_identical(
    iris,
    splendid_convert(iris, algorithms = "algorithm_name", convert = FALSE)
  )
  expect_error(
    splendid_convert(iris, algorithms = "lda", convert = FALSE)
  )
})
