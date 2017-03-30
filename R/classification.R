#' Machine learning classification algorithms
#'
#' @param x data
#' @param y class
#' @param algs algorithms
#'
#' @note qda errors because there are too many columns; need to reduce so that 
#'   every class is greater than number of variables
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' data <- hgsc
#' class <-  stringr::str_split_fixed(rownames(data), "_", n = 2)[, 2]
classification <- function(x, y, algs) {
  algs <- match.arg(algs, ALG.FUN)
  switch(algs,
         lda = suppressWarnings(MASS::lda(x, grouping = y)),
         qda = MASS::qda(x, grouping = y),
         rf = randomForest::randomForest(x, y = y),
         multinom = nnet::multinom(y ~ ., x, MaxNWts = 2000, trace = FALSE),
         nnet = nnet::nnet(y ~ ., x, size = 3, MaxNWts = 2000, trace = FALSE),
         knn = structure(NULL, class = "knn"),
         svm = e1071::svm(y ~ ., x),
         pam = sink_output(
           pamr::pamr.train(list(x = t(x), y = y), n.threshold = 100,
                            prior = rep(1 / dplyr::n_distinct(y),
                                        dplyr::n_distinct(y)))),
         adaboost = sink_output(maboost::maboost(x, y, breg = "entrop")),
         nb = e1071::naiveBayes(x, y),
         glmnet = glmnet::cv.glmnet(as.matrix(x), y, family = "multinomial")
  )
}
