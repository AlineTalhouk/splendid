#' Machine learning classification algorithms
#' 
#' Runs a classification algorithm on a given dataset and reference class.
#' 
#' Some of the classification algorithms implemented use pre-defined values that
#' specify hyperparameters, settings, options, etc. \code{"multinom"} and 
#' \code{"nnet"} increase the maximum number of weights used to 2000, in case 
#' \code{data} is high dimensional and classification is time-consuming. 
#' \code{"nnet"} uses 3 nodes in its hidden layer, a choice that hopefully 
#' promotes sufficient complexity in many datasets. \code{"pamr"} considers 100 
#' thresholds when training, and uses a uniform prior. \code{"adaboost"} 
#' actually calls \code{\link[maboost]{maboost}} instead of 
#' \code{\link[adabag]{boosting}} because of faster performance. As a result, we
#' use the "entrop" option, which uses the KL-divergence method and mimics 
#' adaboost.
#' 
#' When \code{alg = "knn"}, the result is \code{NULL} because the
#' \code{\link[class]{knn}} does not have output an intermediate model object.
#' The modelling and prediction is done in one step. However, a class attribute
#' is still assigned to the result in order to enact the corresponding method in
#' \code{\link{prediction}}.
#' 
#' @inheritParams splendid
#' @param algs character string of classification algorithm to use. See Details 
#'   in \code{\link{splendid}} for a list of choices.
#' @return The model object from running the classification algorithm 
#'   \code{"alg"}
#'   
#' @note \code{"qda"} gives errors when using the \code{hgsc} dataset because 
#'   there are too many variables The algorithm requires the size of every class
#'   to be greater than the number of features. A feature selection framework 
#'   and certain assertion checks need to be built for this algorithm to work.
#'   
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' classification(hgsc, class, "rf")
classification <- function(data, class, algs) {
  algs <- match.arg(algs, ALG.NAME)
  class <- as.factor(class)  # ensure class is a factor
  switch(algs,
         lda = suppressWarnings(MASS::lda(data, grouping = class)),
         qda = MASS::qda(data, grouping = class),
         rf = randomForest::randomForest(data, y = class),
         multinom = nnet::multinom(class ~ ., data, MaxNWts = 2000,
                                   trace = FALSE),
         nnet = nnet::nnet(class ~ ., data, size = 3, MaxNWts = 2000,
                           trace = FALSE),
         knn = structure(NULL, class = "knn"),
         svm = e1071::svm(class ~ ., data),
         pam = sink_output(
           pamr::pamr.train(list(x = t(data), y = class), n.threshold = 100,
                            prior = rep(1 / dplyr::n_distinct(class),
                                        dplyr::n_distinct(class)))),
         adaboost = sink_output(maboost::maboost(data, class, breg = "entrop")),
         xgboost = xgboost::xgb.train(
           params = list("objective" = "multi:softprob",
                         "eval_metric" = "mlogloss",
                         "num_class" = dplyr::n_distinct(class)),
           data = xgboost::xgb.DMatrix(data = as.matrix(data),
                                       label = as.integer(class) - 1),
           nrounds = 2),
         nb = e1071::naiveBayes(data, class),
         glmnet = glmnet::cv.glmnet(as.matrix(data), class,
                                    family = "multinomial")
  )
}
