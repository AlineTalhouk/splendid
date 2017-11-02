#' Class prediction on OOB set
#'
#' Functions to predict class labels on the Out-Of-Bag (test) set for different
#' classifiers.
#'
#' The \code{knn} and \code{pamr} prediction methods use the \code{train.id} and
#' \code{class} arguments for additional modelling steps before prediction. For
#' \code{knn}, the modelling and prediction are performed in one step, so the
#' function takes in both training and test set identifiers. For \code{pamr},
#' the classifier needs to be cross-validated on the training set in order to
#' find a shrinkage threshold with the minimum CV error to use in prediction on
#' the test set. The other prediction methods make use of the default method.
#'
#' @inheritParams splendid
#' @param mod model object from \code{\link{classification}}
#' @param test.id integer vector of indices for test set ID. By default,
#'   prediction occurs on the full data set.
#' @param train.id integer vector of indices for training set ID. Only used for
#'   \code{knn} and \code{pamr} prediction methods.
#' @param ... additional arguments to be passed to or from methods
#' @return A factor of predicted classes with labels in the same order as true
#'   class. If \code{mod} is a \code{"pamr"} classifier, the return value is a
#'   list of length 2: the predicted class, and the threshold value.
#'
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' set.seed(1)
#' training.id <- sample(seq_along(class), replace = TRUE)
#' test.id <- which(!seq_along(class) %in% training.id)
#' mod <- classification(hgsc[training.id, ], class[training.id], "lda")
#' pred <- prediction(mod, hgsc, class, test.id)
#' table(true = class[test.id], pred)
prediction <- function(mod, data, class, test.id = NULL, train.id = NULL,
                       threshold = 0.5, standardize = FALSE, ...) {
  if (!inherits(mod, ALG.CLASS)) {
    match.arg(class(mod), ALG.CLASS)
  } else {
    UseMethod("prediction")
  }
}

#' @rdname prediction
#' @export
prediction.default <- function(mod, data, class, test.id = NULL,
                               train.id = NULL, threshold = 0.5,
                               standardize = FALSE, ...) {
  dat <- split_data(data, test.id, train.id, standardize)
  stats::predict(mod, dat$test, ...)
}

#' @rdname prediction
#' @export
prediction.pamrtrained <- function(mod, data, class, test.id = NULL,
                                   train.id = NULL, threshold = 0.5,
                                   standardize = FALSE, ...) {
  dat <- split_data(data, test.id, train.id, standardize)
  model.cv <- sink_output(
    pamr::pamr.cv(mod, list(x = t(dat$train), y = class[train.id]), nfold = 5))
  delta <- with(model.cv, threshold[which.min(error)])
  pred <- pamr::pamr.predict(mod, t(dat$test), threshold = delta,
                             type = "class")
  prob <- pamr::pamr.predict(mod, t(dat$test), threshold = delta,
                             type = "posterior")
  prediction_output(pred, prob, class, test.id, threshold) %>%
    structure(delta = delta)
}

#' @export
prediction.rfe <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  p <- prediction.default(mod, data[, mod$optVariables], test.id = test.id,
                          train.id = train.id, standardize = standardize, ...)
  pred <- p$pred
  prob <- p[, names(p) != "pred"]
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.svm <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  pred <- unname(prediction.default(mod, data, test.id = test.id,
                                    train.id = train.id,
                                    standardize = standardize,
                                    probability = TRUE, ...))
  prob <- attr(pred, "probabilities")
  if (!("0" %in% mod$levels))
    prob <- prob[, order(colnames(prob), names(table(class)))]
  prediction_output(pred, prob, class, test.id, threshold) %>%
    structure(probabilities = NULL)
}

#' @export
prediction.randomForest <- function(mod, data, class, test.id = NULL,
                                    train.id = NULL, threshold = 0.5,
                                    standardize = FALSE, ...) {
  pred <- unname(prediction.default(mod, data, test.id = test.id,
                                    train.id = train.id,
                                    standardize = standardize,
                                    type = "response", ...))
  prob <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                             standardize = standardize,
                             type = "prob", ...)
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.lda <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  p <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                          standardize = standardize, ...)
  pred <- p$class
  prob <- p$posterior %>% sum_to_one()
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.sda <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  prediction.lda(mod = mod, data = as.matrix(data), class = class,
                 test.id = test.id, train.id = train.id, threshold = threshold,
                 standardize = standardize, verbose = FALSE, ...)
}

#' @export
prediction.cv.glmnet <- function(mod, data, class, test.id = NULL,
                                 train.id = NULL, threshold = 0.5,
                                 standardize = FALSE, ...) {
  pred <- factor(prediction.default(mod, as.matrix(data), test.id = test.id,
                                    train.id = train.id,
                                    standardize = standardize, type = "class",
                                    ...))
  prob <- prediction.default(mod, as.matrix(data), test.id = test.id,
                             train.id = train.id, standardize = standardize,
                             type = "response", ...)[, , 1]
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.glmnet <- function(mod, data, class, test.id = NULL, train.id = NULL,
                              threshold = 0.5, standardize = FALSE, ...) {
  prediction.cv.glmnet(mod = mod, data = data, class = class, test.id = test.id,
                       train.id = train.id, threshold = threshold,
                       standardize = standardize, ...)
}

#' @export
prediction.multinom <- function(mod, data, class, test.id = NULL,
                                train.id = NULL, threshold = 0.5,
                                standardize = FALSE, ...) {
  pred <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                             standardize = standardize, type = "class", ...)
  prob <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                             standardize = standardize, type = "probs", ...)
  if (!is.matrix(prob)) {  # for ova case
    prob <- matrix(c(1 - prob, prob), ncol = 2, dimnames = list(NULL, mod$lev))
  }
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.nnet.formula <- function(mod, data, class, test.id = NULL,
                                    train.id = NULL, threshold = 0.5,
                                    standardize = FALSE, ...) {
  pred <- factor(prediction.default(mod, data, test.id = test.id,
                                    train.id = train.id,
                                    standardize = standardize, type = "class",
                                    ...))
  prob <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                             standardize = standardize, type = "raw", ...)
  if (ncol(prob) == 1) {  # for ova case
    prob <- matrix(c(1 - prob, prob), ncol = 2, dimnames = list(NULL, mod$lev))
  }
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.naiveBayes <- function(mod, data, class, test.id = NULL,
                                  train.id = NULL, threshold = 0.5,
                                  standardize = FALSE, ...) {
  pred <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                             standardize = standardize, type = "class", ...)
  prob <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                             standardize = standardize, type = "raw", ...) %>%
    magrittr::set_rownames(rownames(data[test.id, ]))
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.maboost <- function(mod, data, class, test.id = NULL,
                               train.id = NULL, threshold = 0.5,
                               standardize = FALSE, ...) {
  names(data) <- make.names(names(data))
  both <- prediction.default(mod, data, test.id = test.id, train.id = train.id,
                             standardize = standardize, type = "both", ...)
  pred <- both$class
  prob <- both$probs %>% magrittr::set_rownames(rownames(data[test.id, ]))
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.xgb.Booster <- function(mod, data, class, test.id = NULL,
                                   train.id = NULL, threshold = 0.5,
                                   standardize = FALSE, ...) {
  class <- factor(class)
  prob <- prediction.default(mod, as.matrix(data), test.id = test.id,
                             train.id = train.id, standardize = standardize,
                             reshape = TRUE, ...) %>%
    magrittr::set_rownames(rownames(data[test.id, ])) %>%
    sum_to_one()
  if (ncol(prob) == nlevels(class)) {
    colnames(prob) <- levels(class)
  }
  pred <- factor(levels(class)[max.col(data.frame(prob))])
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @rdname prediction
#' @export
prediction.knn <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  dat <- split_data(data, test.id, train.id, standardize) %>% {
    rbind(.$train, .$test)
  }
  if (inherits(mod, "ova")) {  # for ova case
    lev <- as.character(unlist(mod))
    if (length(lev) == 1) class <- ifelse(class == lev, lev, 0)
  }

  kdist <- knnflex::knn.dist(dat)
  kparams <- list(train = seq_along(train.id),
                  test = length(train.id) + seq_along(test.id),
                  y = factor(class[train.id]), dist.matrix = kdist, k = 5)
  pred <- unname(factor(purrr::invoke(knnflex::knn.predict, kparams)))
  prob <- t(purrr::invoke(knnflex::knn.probability, kparams))
  prediction_output(pred, prob, class, test.id, threshold)
}

#' Prediction output with attributes
#' @noRd
prediction_output <- function(pred, prob, class, test.id, threshold) {
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
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

#' Split data into training and test sets
#'
#' Split data into training and test sets, optionally standardizing by training
#' set centers and standard deviations
#'
#' @inheritParams prediction
#' @export
split_data <- function(data, test.id = NULL, train.id = NULL,
                       standardize = FALSE) {
  test.id <- test.id %||% seq_len(nrow(data))
  train.id <- train.id %||% seq_len(nrow(data))
  if (standardize) {
    train <- scale(data[train.id, ])
    test <- scale(data[test.id, ],
                  attr(train, "scaled:center"),
                  attr(train, "scaled:scale"))
    if (inherits(data, "data.frame")) {
      train <- as.data.frame(train)
      test <- as.data.frame(test)
    }
  } else {
    train <- data[train.id, ]
    test <- data[test.id, ]
  }
  dplyr::lst(train, test)
}
