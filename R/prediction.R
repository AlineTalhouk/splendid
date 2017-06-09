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
#'   \code{knn} and \code{pamr} prediction methods.
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
#' pred <- prediction(mod, hgsc, test.id, class = class)
#' table(true = class[test.id], pred)
prediction <- function(mod, data, test.id, threshold = 0.5, class, ...) {
  if (!inherits(mod, ALG.CLASS)) {
    match.arg(class(mod), ALG.CLASS)
  } else {
    UseMethod("prediction")
  }
}

#' @rdname prediction
#' @export
prediction.default <- function(mod, data, test.id, threshold = 0.5, class,
                               ...) {
  stats::predict(mod, data[test.id, ], ...)
}

#' @export
prediction.lda <- function(mod, data, test.id, threshold = 0.5, class, ...) {
  p <- prediction.default(mod, data, test.id, ...)
  prob <- p$posterior %>% sum_to_one()
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(p$class, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.sda <- function(mod, data, test.id, threshold = 0.5, class, ...) {
  prediction.lda(mod, as.matrix(data), test.id, threshold, class,
                 verbose = FALSE, ...)
}

#' @export
prediction.rfe <- function(mod, data, test.id, threshold = 0.5, class, ...) {
  p <- prediction.default(mod, data[, mod$optVariables], test.id)
  prob <- p[, names(p) != "pred"]
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(p$pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.randomForest <- function(mod, data, test.id, threshold = 0.5,
                                    class, ...) {
  pred <- unname(prediction.default(mod, data, test.id, type = "response"))
  prob <- prediction.default(mod, data, test.id, type = "prob")
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.multinom <- function(mod, data, test.id, threshold = 0.5, class,
                                ...) {
  pred <- prediction.default(mod, data, test.id, type = "class")
  prob <- prediction.default(mod, data, test.id, type = "probs")
  if (!is.matrix(prob)) {  # for OVA case
    prob <- matrix(c(1 - prob, prob), ncol = 2,
                   dimnames = list(NULL, mod$lev))
  }
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.nnet.formula <- function(mod, data, test.id, threshold = 0.5, class,
                                    ...) {
  pred <- factor(prediction.default(mod, data, test.id, type = "class"))
  prob <- prediction.default(mod, data, test.id, type = "raw")
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @rdname prediction
#' @export
prediction.knn <- function(mod, data, test.id, threshold = 0.5, class, train.id,
                           ...) {
  kdist <- knnflex::knn.dist(data[c(train.id, test.id), ])
  kparams <- list(train = seq_along(train.id),
                  test = length(train.id) + seq_along(test.id),
                  y = class[train.id], dist.matrix = kdist, k = 5)
  pred <- unname(factor(purrr::invoke(knnflex::knn.predict, kparams)))
  prob <- t(purrr::invoke(knnflex::knn.probability, kparams))
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.svm <- function(mod, data, test.id, threshold = 0.5, class, ...) {
  pred <- unname(prediction.default(mod, data, test.id, probability = TRUE))
  prob <- attr(pred, "probabilities")
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp, probabilities = NULL)
}

#' @rdname prediction
#' @export
prediction.pamrtrained <- function(mod, data, test.id, threshold = 0.5,
                                   class, train.id, ...) {
  model.cv <- sink_output(
    pamr::pamr.cv(mod, list(x = t(data[train.id, ]), y = class[train.id]),
                  nfold = 5))
  delta <- model.cv$threshold[which.min(model.cv$error)]
  pred <- pamr::pamr.predict(mod, t(data[test.id, ]), threshold = delta,
                             type = "class")
  prob <- pamr::pamr.predict(mod, t(data[test.id, ]), threshold = delta,
                             type = "posterior")
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp, delta = delta)
}

#' @export
prediction.maboost <- function(mod, data, test.id, threshold = 0.5, class,
                               ...) {
  names(data) <- make.names(names(data))
  both <- prediction.default(mod, data, test.id, type = "both")
  prob <- both$probs %>% magrittr::set_rownames(rownames(data[test.id, ]))
  ct <- class_threshold(both$probs, threshold = threshold)
  cp <- class_proportion(ct)
  structure(both$class, prob = prob, class.true = class[test.id],
            class.thres = ct, class.prop = cp)
}

#' @export
prediction.xgb.Booster <- function(mod, data, test.id, threshold = 0.5, class,
                                   ...) {
  class <- factor(class)
  prob <- prediction.default(mod, as.matrix(data), test.id, reshape = TRUE) %>%
    magrittr::set_rownames(rownames(data[test.id, ])) %>%
    sum_to_one()
  if (ncol(prob) == nlevels(class)) {
    colnames(prob) <- levels(class)
  }
  pred <- factor(levels(class)[max.col(data.frame(prob))])
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.naiveBayes <- function(mod, data, test.id, threshold = 0.5, class,
                                  ...) {
  pred <- prediction.default(mod, data, test.id, type = "class")
  prob <- prediction.default(mod, data, test.id, type = "raw") %>%
    magrittr::set_rownames(rownames(data[test.id, ]))
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.cv.glmnet <- function(mod, data, test.id, threshold = 0.5, class,
                                 ...) {
  pred <- factor(prediction.default(mod, as.matrix(data), test.id,
                                    type = "class"))
  prob <- prediction.default(mod, as.matrix(data), test.id,
                             type = "response")[, , 1]
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}

#' @export
prediction.glmnet <- function(mod, data, test.id, threshold = 0.5, class, ...) {
  prediction.cv.glmnet(mod, data, test.id, threshold, class, ...)
}

#' Predicted class labels above a max class probability threshold
#' @noRd
class_threshold <- function(prob, threshold = 0.5) {
  prob %>%
    as.data.frame() %>%
    dplyr::mutate_(.dots = stats::setNames(
      list(~purrr::pmap_dbl(., max),
           ~purrr::pmap(., list) %>%
             purrr::map_chr(~ names(which.max(.x))) %>%
             ifelse(max_prop >= threshold, ., "unclassified")),
      c("max_prop", "max_class"))) %>%
    magrittr::extract2("max_class") %>%
    factor() %>%
    forcats::fct_expand(colnames(prob))
}

#' Proportion of classified predictions
#' @noRd
class_proportion <- function(pred) {
  good_ind <- pred != "unclassified"
  sum(good_ind) / length(good_ind)
}
