#' Functions from the orphaned CRAN package knnflex
#' @author Atina Dunlap Brooks
#'
#' @references https://github.com/cran/knnflex/blob/master/R/classprob.R
#' @noRd
classprob <- function(x) {
  x <- as.factor(x)
  n <- nlevels(x)
  votes <- rep(0, n)
  for (i in 1:length(x)) votes[as.integer(x[i])] <- votes[as.integer(x[i])] + 1
  votes / length(x)
}

#' @references https://github.com/cran/knnflex/blob/master/R/knn.dist.R
#' @noRd
knn.dist <- function(x, dist.meth = "euclidean", p = 2) {
  d <- as.matrix(stats::dist(x, dist.meth, p = p))
  round(d, digits = 15)
}

#' @references https://github.com/cran/knnflex/blob/master/R/knn.predict.R
#' @noRd
knn.predict <- function(train, test, y, dist.matrix, k = 1,
                        agg.meth = if (is.factor(y)) "majority" else "mean",
                        ties.meth = "min") {
  n <- length(test)
  if (is.unsorted(train))
    train <- sort(train)
  if (is.unsorted(test))
    test <- sort(test)
  d <- dist.matrix[test, train]
  if (length(y) > length(train))
    y <- y[train]
  if (n == 1) {
    d <- rank(d, ties.method = ties.meth)
    x <- apply(data.frame(y[d <= k]), 2, agg.meth)
    names(x) <- test
    return(x)
  }
  else {
    d <- t(apply(d, 1, function(x) rank(x, ties.method = ties.meth)))
    apply(d, 1, function(x) apply(data.frame(y[x <= k]),
                                  2, agg.meth))
  }
}

#' @references https://github.com/cran/knnflex/blob/master/R/knn.probability.R
#' @noRd
knn.probability <- function(train, test, y, dist.matrix, k = 1,
                            ties.meth = "min") {
  n <- length(test)
  if (is.unsorted(train))
    train <- sort(train)
  if (is.unsorted(test))
    test <- sort(test)
  d <- dist.matrix[test, train]
  y <- as.factor(y)
  if (length(y) > length(train))
    y <- y[train]
  if (n == 1) {
    d <- rank(d, ties.method = ties.meth)
    x <- classprob(y[d <= k])
    x <- data.frame(x)
    names(x) <- test
    row.names(x) <- levels(y)
    return(x)
  }
  else {
    d <- t(apply(d, 1, function(x) rank(x, ties.method = ties.meth)))
    x <- apply(d, 1, function(x) classprob(y[x <= k]))
    row.names(x) <- levels(y)
    return(x)
  }
}

#' @references https://github.com/cran/knnflex/blob/master/R/majority.R
#' @noRd
majority <- function(x) {
  x <- as.factor(x)
  n <- nlevels(x)
  votes <- rep(0, n)
  for (i in 1:length(x)) votes[as.integer(x[i])] <- votes[as.integer(x[i])] +
    1
  levels(x)[order(votes, decreasing = TRUE, sample(1:n, n))[1]]
}
