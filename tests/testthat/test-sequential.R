context("sequential")

data(hgsc)
dat <- hgsc[1:100, 1:25]
class <- attr(hgsc, "class.true")[1:100]
set.seed(2)
sm <- splendid_model(dat, class, n = 2, algorithms = c("xgboost", "slda"))

test_that("sequential alg fits one fewer model than number of classes", {
  st <- sequential_train(sm, dat, class)
  sp <- sequential_pred(st, sm, dat, class)
  m <- dplyr::n_distinct(class) - 1
  expect_length(st, m)
  expect_length(sp$prob, m)
  expect_length(sp$cm, m)
})

test_that("boxplot showing F1-score distribution can be plotted", {
  expect_error(sequential_train(sm, dat, class, boxplot = TRUE), NA)
})

dev.off()
if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
