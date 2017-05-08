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
#' @param probability \code{svm} parameter; logical; if \code{TRUE}, returns
#'   posterior probabilities in addition to class labels.
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
  prediction.default(mod, data, test.id)$class
}

#' @export
prediction.qda <- function(mod, data, test.id, ...) {
  prediction.default(mod, data[, colnames(mod$means)], test.id)$class
}

#' @export
prediction.rfe <- function(mod, data, test.id, ...) {
  prediction.default(mod, data[, mod$optVariables], test.id)$pred
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
prediction.knn <- function(mod, data, test.id, train.id, class,
                           probability = FALSE, ...) {
  kdist <- knnflex::knn.dist(data[c(train.id, test.id), ])
  kparams <- list(train = seq_along(train.id),
                  test = length(train.id) + seq_along(test.id),
                  y = class[train.id], dist.matrix = kdist, k = 5)
  cl <- purrr::invoke(knnflex::knn.predict, kparams)
  if (probability) {
    attr(cl, "prob") <- purrr::invoke(knnflex::knn.probability, kparams) %>% t()
  }
  cl
}

#' @export
prediction.svm <- function(mod, data, test.id, probability = FALSE, ...) {
  unname(prediction.default(mod, data, test.id, probability = probability))
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
prediction.xgb.Booster <- function(mod, data, test.id, ...) {
  prediction.default(mod, as.matrix(data), test.id) %>% 
    matrix(ncol = mod$params$num_class, byrow = TRUE) %>%
    data.frame() %>%
    max.col()
}

#' @export
prediction.naiveBayes <- function(mod, data, test.id, ...) {
  prediction.default(mod, data, test.id)
}

#' @export
prediction.cv.glmnet <- function(mod, data, test.id, ...) {
  factor(prediction.default(mod, as.matrix(data), test.id, type = "class"))
}