#' Predict using mod onto test set indicated by indices in test.id
#' If model is of class "lda", the predictions are accessed in the "class" name
#'
#' @param mod model
#' @param train.id training set ID
#' @param test.id test set ID
#' @param data data
#' @param class class
#'
#' @author Derek Chiu
#' @export
prediction <- function(mod, train.id, test.id, data, class) {
  # Special handling for pamr, knn, nnet, lda, glmnet; otherwise just predict
  if (inherits(mod, "pamrtrained")) {
    model.cv <- sink_output(
      pamr::pamr.cv(mod, list(x = t(data[train.id, ]), y = class[train.id]),
                    nfold = 5))
    delta <- model.cv$threshold[which.min(model.cv$error)]
    p <- list(pred = pamr::pamr.predict(mod, t(data[test.id, ]),
                                        threshold = delta),
              delta = delta)
  } else if (is.null(mod)) {
    p <- class::knn(train = data[train.id, ], test = data[test.id, ],
                    cl = class[train.id], k = 5, l = 3)
  } else if (inherits(mod, c("randomForest", "svm"))) {
    p <- unname(stats::predict(mod, data[test.id, ]))
  } else if (inherits(mod, "nnet.formula")) {
    p <- factor(stats::predict(mod, data[test.id, ], type = "class"))
  } else if (inherits(mod, "cv.glmnet")) {
    p <- factor(stats::predict(mod, as.matrix(data[test.id, ]), type = "class"))
  } else if (inherits(mod, "lda")) {
    p <- stats::predict(mod, data[test.id, ])$class
  } else {
    p <- stats::predict(mod, data[test.id, ])
  }
  return(p)
}




# $lda
# [1] "lda"
# 
# $rf
# [1] "randomForest"
# 
# $multinom
# [1] "multinom" "nnet"    
# 
# $nnet
# [1] "nnet.formula" "nnet"        
# 
# $knn
# [1] "NULL"
# 
# $svm
# [1] "svm.formula" "svm"        
# 
# $pam
# [1] "pamrtrained"
# 
# $adaboost
# [1] "maboost"
# 
# $nb
# [1] "naiveBayes"
# 
# $glmnet
# [1] "cv.glmnet"

