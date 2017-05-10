#' Class prediction on OOB set
#'
#' Functions to predict class labels on the Out-Of-Bag (test) set for different
#' classifiers.
#'
#' Only the \code{knn} and \code{pamr} prediction methods require the additional
#' \code{train.id} and \code{class} arguments. For \code{knn}, the modelling and
#' prediction are performed in one step, so the function takes in both training
#' and test set identifiers. For \code{pamr}, the classifier needs to be
#' cross-validated on the training set in order to find a shrinkage threshold
#' with the minimum CV error to use in prediction on the test set. The other
#' prediction methods make use of the default method.
#'
#' @inheritParams splendid
#' @param mod model object from \code{\link{classification}}
#' @param test.id integer vector of indices for test set
#' @param ... additional arguments to be passed to or from methods
#' @param train.id integer vector of indices for training set ID. Only used for
#' \code{knn} and \code{pamr} prediction methods.
#' @return A factor of predicted classes with labels in the same order as true
#'   class. If \code{mod} is a \code{"pamr"} classifier, the return value is a
#'   list of length 2: the predicted class, and the threshold value.
#'
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' set.seed(1)
#' training.id <- sample(seq_along(class), replace = TRUE)
#' test.id <- which(!seq_along(class) %in% training.id)
#' mod <- classification(hgsc[training.id, ], class[training.id], "lda")
#' pred <- prediction(mod, hgsc, test.id)
#' table(true = class[test.id], pred)
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
  p <- prediction.default(mod, data, test.id)
  attr(p$class, "prob") <- p$posterior
  p$class
}

#' @export
prediction.qda <- function(mod, data, test.id, ...) {
  p <- prediction.default(mod, data[, colnames(mod$means)], test.id)
  attr(p$class, "prob") <- p$posterior
  p$class
}

#' @export
prediction.rfe <- function(mod, data, test.id, ...) {
  p <- prediction.default(mod, data[, mod$optVariables], test.id)
  attr(p$pred, "prob") <- dplyr::select(p, -dplyr::contains("pred"))
  p$pred
}

#' @export
prediction.randomForest <- function(mod, data, test.id, ...) {
  pred <- unname(prediction.default(mod, data, test.id, type = "response"))
  prob <- prediction.default(mod, data, test.id, type = "prob")
  attr(pred, "prob") <- prob
  pred
}

#' @export
prediction.multinom <- function(mod, data, test.id, ...) {
  pred <- prediction.default(mod, data, test.id, type = "class")
  prob <- prediction.default(mod, data, test.id, type = "probs")
  attr(pred, "prob") <- prob
  pred
}

#' @export
prediction.nnet.formula <- function(mod, data, test.id, ...) {
  pred <- factor(prediction.default(mod, data, test.id, type = "class"))
  prob <- prediction.default(mod, data, test.id, type = "raw")
  attr(pred, "prob") <- prob
  pred
}

#' @rdname prediction
#' @export
prediction.knn <- function(mod, data, test.id, train.id, class, ...) {
  kdist <- knnflex::knn.dist(data[c(train.id, test.id), ])
  kparams <- list(train = seq_along(train.id),
                  test = length(train.id) + seq_along(test.id),
                  y = class[train.id], dist.matrix = kdist, k = 5)
  pred <- unname(factor(purrr::invoke(knnflex::knn.predict, kparams)))
  prob <- t(purrr::invoke(knnflex::knn.probability, kparams))
  attr(pred, "prob") <- prob
  pred
}

#' @export
prediction.svm <- function(mod, data, test.id, ...) {
  unname(prediction.default(mod, data, test.id, probability = TRUE))
}

#' @rdname prediction
#' @export
prediction.pamrtrained <- function(mod, data, test.id, train.id, class, ...) {
  model.cv <- sink_output(
    pamr::pamr.cv(mod, list(x = t(data[train.id, ]), y = class[train.id]),
                  nfold = 5))
  delta <- model.cv$threshold[which.min(model.cv$error)]
  pred <- pamr::pamr.predict(mod, t(data[test.id, ]), threshold = delta,
                             type = "class")
  prob <- pamr::pamr.predict(mod, t(data[test.id, ]), threshold = delta,
                             type = "posterior")
  attr(pred, "prob") <- prob
  dplyr::lst(pred, delta)
}

#' @export
prediction.maboost <- function(mod, data, test.id, ...) {
  names(data) <- make.names(names(data))
  both <- prediction.default(mod, data, test.id, type = "both")
  attr(both$class, "prob") <- both$probs
  both$class
}

#' @export
prediction.xgb.Booster <- function(mod, data, test.id, class, ...) {
  prob <-  prediction.default(mod, as.matrix(data), test.id, reshape = TRUE) %>%
    magrittr::set_colnames(levels(class)) %>%
    round(6)
  eps <- rowSums(prob) - 1
  prob[, 1] <- prob[, 1] - eps  # make sure every row sums to 1
  pred <- factor(levels(class)[max.col(data.frame(prob))])
  attr(pred, "prob") <- prob
  pred
}

#' @export
prediction.naiveBayes <- function(mod, data, test.id, ...) {
  pred <- prediction.default(mod, data, test.id, "class")
  prob <- prediction.default(mod, data, test.id, "raw")
  attr(pred, "prob") <- prob
  pred
}

#' @export
prediction.cv.glmnet <- function(mod, data, test.id, ...) {
  pred <- factor(prediction.default(mod, as.matrix(data), test.id,
                                    type = "class"))
  prob <- prediction.default(mod, as.matrix(data), test.id,
                             type = "response")[, , 1]
  attr(pred, "prob") <- prob
  pred
}

#' @export
prediction.glmnet <- function(mod, data, test.id, ...) {
  prediction.cv.glmnet(mod, data, test.id, ...)
}
