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
#' @param sizes the range of sizes of features to test RFE algorithm
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
classification <- function(data, class, algs, rfe = FALSE, sizes = NULL) {
  algs <- match.arg(algs, ALG.NAME)
  class <- as.factor(class)  # ensure class is a factor
  sizes <- sizes %||% seq_len(round(min(table(class)) / 2))
  switch(algs,
         lda = {
           if (!rfe)
             suppressWarnings(MASS::lda(data, grouping = class))
           else
             suppressPackageStartupMessages(suppressWarnings(
               caret::rfe(data, class, sizes = sizes,
                          rfeControl = caret::rfeControl(
                            functions = caret::ldaFuncs, method = "cv"))))
         },
         qda = {
           data <- data %>%  # Use the variables with the largest variance
             magrittr::extract(, apply(., 2, stats::var) %>% 
                                 unlist() %>% 
                                 sort() %>% 
                                 utils::tail(min(table(class)) - 1) %>% 
                                 names()) %>% 
             apply(2, jitter)
           if (!rfe)
             MASS::qda(data, grouping = class)
           else
             suppressPackageStartupMessages(suppressWarnings(
               caret::rfe(data, class, sizes = sizes,
                          rfeControl = caret::rfeControl(
                            functions = qdaFuncs, method = "cv"))))
         },
         rf = {
           if (!rfe)
             randomForest::randomForest(data, y = class)
           else
             suppressPackageStartupMessages(suppressWarnings(
               caret::rfe(data, class, sizes = sizes,
                          rfeControl = caret::rfeControl(
                            functions = caret::rfFuncs, method = "cv"))))
         },
         multinom = nnet::multinom(class ~ ., data, MaxNWts = 2000,
                                   trace = FALSE),
         nnet = e1071::best.nnet(class ~ ., data = cbind(data, class),
                                 size = seq_len(5),
                                 decay = seq(0, 0.5, length.out = 5),
                                 MaxNWts = 2000,
                                 tunecontrol = e1071::tune.control(
                                   sampling = "fix")),
         knn = structure(list(), class = "knn"),
         svm = {
           if (!rfe) {
             opt_var <- names(data)
           } else {
             mod <- suppressPackageStartupMessages(suppressWarnings(
               caret::rfe(data, class, sizes = sizes[sizes %% 5 == 0],
                          method = "svmRadial",
                          rfeControl = caret::rfeControl(
                            functions = caret::caretFuncs, method = "cv",
                            number = 2))))
             opt_var <- mod$optVariables
           }
           e1071::best.svm(x = data[, opt_var], y = class,
                           probability = TRUE,
                           gamma = 1 / ncol(data) * 2 ^ (0:4),
                           cost = 2 ^ (0:4),
                           tunecontrol = e1071::tune.control(
                             sampling = "fix"))
         },
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
         nbayes = e1071::naiveBayes(data, class),
         lasso = glmnet::cv.glmnet(as.matrix(data), class, alpha = 1,
                                   family = "multinomial"),
         ridge = glmnet::cv.glmnet(as.matrix(data), class, alpha = 0,
                                   family = "multinomial")
  )
}

#' QDA functions
#' @noRd
qdaFuncs <- caret::ldaFuncs
qdaFuncs$fit <- function(x, y, first, last, ...) {
  MASS::qda(x, y, ...)
}
