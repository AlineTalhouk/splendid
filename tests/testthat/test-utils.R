context("utils")

test_that("sum_to_one assigns random class when probs all equal zero", {
  set.seed(1)
  prob <- matrix(c(0.1, 0.2, 0.3, 0.39, 0, 0, 0, 0), nrow = 2, byrow = TRUE)
  runs <- purrr::rerun(10, sum_to_one(prob))
  expect_equal(
    purrr::map(runs, `[`, 2, 1:4) %>%
      purrr::map(`==`, 1) %>%
      purrr::map_int(which) %>%
      magrittr::is_in(1:4),
   rep(TRUE, 10)
  )
})
