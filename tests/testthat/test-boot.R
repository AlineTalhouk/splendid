context("boot")

test_that("stratified bootstrap samples preserve class proportions", {
  iris2 <- iris[1:130, ]
  counts <- as.vector(table(iris2$Species))

  id1 <- boot_train(iris2, iris2$Species, n = 10, stratify = FALSE)
  counts1 <- purrr::map(id1, ~ as.vector(table(iris2[., "Species"])))
  expect_false(all(unlist(purrr::map(counts1, ~ . == counts))))

  id2 <- boot_train(iris2, iris2$Species, n = 10, stratify = TRUE)
  counts2 <- purrr::map(id2, ~ as.vector(table(iris2[., "Species"])))
  expect_true(all(unlist(purrr::map(counts2, ~ . == counts))))
})

test_that("bootstrap runs recursively when there's extreme imbalance", {
  set.seed(1)
  iris2 <- iris
  iris2$Species[101:147] <- "versicolor"

  id <- boot_train(iris2, iris2$Species, n = 10, stratify = FALSE)
  expect_length(id, 10)
})
