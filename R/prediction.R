#' Prediction on OOB set
#' 
#' Functions to predict class labels on the Out-Of-Bag set for different models.
#' 
#' Only the \code{knn} and \code{pamr} prediction methods require the additional
#' \code{train.id} and \code{class} arguments. The other prediction methods 
#' make use of the default method.
#' 
#' @param mod model
#' @param data data
#' @param test.id test set ID
#' @param ... additional arguments to be passed to or from methods
#' @param train.id training set ID
#' @param class class
#' @return A factor of predicted values with levels in the same order as true
#'   class
#'   
#' @author Derek Chiu
#' @export
prediction <- function(mod, data, test.id, ...) {
  if (!inherits(mod, ALG.CLASS)) {
    match.arg(class(mod), ALG.CLASS)
  } else {
    UseMethod("prediction")
  }
}

#' @rdname prediction
#' @export
prediction.default <- function(mod, data, test.id, ...) {
  stats::predict(mod, data[test.id, ], ...)
}

#' @export
prediction.lda <- function(mod, data, test.id, ...) {
  prediction.default(mod, data, test.id)$class
}

#' @export
prediction.randomForest <- function(mod, data, test.id, ...) {
  unname(prediction.default(mod, data, test.id))
}

#' @export
prediction.multinom <- function(mod, data, test.id, ...) {
  prediction.default(mod, data, test.id)
}

#' @export
prediction.nnet.formula <- function(mod, data, test.id, ...) {
  factor(prediction.default(mod, data, test.id, type = "class"))
}

#' @rdname prediction
#' @export
prediction.knn <- function(mod, data, test.id, train.id, class, ...) {
  class::knn(train = data[train.id, ], test = data[test.id, ],
             cl = class[train.id], k = 5, l = 3)
}

#' @export
prediction.svm <- function(mod, data, test.id, ...) {
  unname(prediction.default(mod, data, test.id))
}

#' @rdname prediction
#' @export
prediction.pamrtrained <- function(mod, data, test.id, train.id, class, ...) {
  model.cv <- sink_output(
    pamr::pamr.cv(mod, list(x = t(data[train.id, ]), y = class[train.id]),
                  nfold = 5))
  delta <- model.cv$threshold[which.min(model.cv$error)]
  list(pred = pamr::pamr.predict(mod, t(data[test.id, ]), threshold = delta),
       delta = delta)
}

#' @export
prediction.maboost <- function(mod, data, test.id, ...) {
  names(data) <- make.names(names(data))
  prediction.default(mod, data, test.id)
}

#' @export
prediction.naiveBayes <- function(mod, data, test.id, ...) {
  prediction.default(mod, data, test.id)
}

#' @export
prediction.cv.glmnet <- function(mod, data, test.id, ...) {
  factor(prediction.default(mod, as.matrix(data), test.id, type = "class"))
}