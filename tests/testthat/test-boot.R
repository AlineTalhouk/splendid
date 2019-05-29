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
