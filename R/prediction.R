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
#' @param test.id integer vector of indices for test set. If \code{NULL}
#'   (default), all samples are used.
#' @param train.id integer vector of indices for training set. If \code{NULL}
#'   (default), all samples are used.
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
  dat <- split_data(data, test.id, train.id, standardize) %>% purrr::map(t)
  cvdat <- list(x = dat$train, y = class[train.id])
  model.cv <- sink_output(pamr::pamr.cv(mod, cvdat, nfold = 5))
  delta <- with(model.cv, threshold[which.min(error)])
  p_args <- dplyr::lst(fit = mod, newx = dat$test, threshold = delta)
  pred <- purrr::invoke(pamr::pamr.predict, p_args, type = "class")
  prob <- purrr::invoke(pamr::pamr.predict, p_args, type = "posterior")
  prediction_output(pred, prob, class, test.id, threshold) %>%
    structure(delta = delta)
}

#' @export
prediction.rfe <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, test.id, train.id, standardize)
  p <- purrr::invoke(prediction.default, p_args,
                     data = data[, mod$optVariables], ...)
  pred <- p$pred
  prob <- p[, names(p) != "pred"]
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.svm <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, data, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, probability = TRUE, ...) %>%
    unname()
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
  p_args <- dplyr::lst(mod, data, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, type = "response", ...) %>%
    unname()
  prob <- purrr::invoke(prediction.default, p_args, type = "prob", ...)
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.lda <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, data, test.id, train.id, standardize)
  p <- purrr::invoke(prediction.default, p_args, ...)
  pred <- p$class
  prob <- p$posterior %>% sum_to_one()
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.sda <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0.5, standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, class, test.id, train.id, threshold, standardize)
  purrr::invoke(prediction.lda, p_args, data = as.matrix(data), verbose = FALSE,
                ...)
}

#' @export
prediction.cv.glmnet <- function(mod, data, class, test.id = NULL,
                                 train.id = NULL, threshold = 0.5,
                                 standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, data = as.matrix(data),
                        type = "class", ...)
  prob <- purrr::invoke(prediction.default, p_args, data = as.matrix(data),
                        type = "response", ...)[, , 1]
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.glmnet <- function(mod, data, class, test.id = NULL, train.id = NULL,
                              threshold = 0.5, standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, data, class, test.id, train.id, threshold,
                       standardize)
  purrr::invoke(prediction.cv.glmnet, p_args, ...)
}

#' @export
prediction.multinom <- function(mod, data, class, test.id = NULL,
                                train.id = NULL, threshold = 0.5,
                                standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, data, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, type = "class", ...)
  prob <- purrr::invoke(prediction.default, p_args, type = "probs", ...)
  if (!is.matrix(prob)) {  # for ova case
    prob <- matrix(c(1 - prob, prob), ncol = 2, dimnames = list(NULL, mod$lev))
  }
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.nnet.formula <- function(mod, data, class, test.id = NULL,
                                    train.id = NULL, threshold = 0.5,
                                    standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, data, test.id, train.id, standardize)
  pred <- factor(purrr::invoke(prediction.default, p_args, type = "class", ...))
  prob <- purrr::invoke(prediction.default, p_args, type = "raw", ...)
  if (ncol(prob) == 1) {  # for ova case
    prob <- matrix(c(1 - prob, prob), ncol = 2, dimnames = list(NULL, mod$lev))
  }
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.naiveBayes <- function(mod, data, class, test.id = NULL,
                                  train.id = NULL, threshold = 0.5,
                                  standardize = FALSE, ...) {
  p_args <- dplyr::lst(mod, data, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, type = "class", ...)
  prob <- purrr::invoke(prediction.default, p_args, type = "raw", ...) %>%
    magrittr::set_rownames(rownames(data[test.id, ]))
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.maboost <- function(mod, data, class, test.id = NULL,
                               train.id = NULL, threshold = 0.5,
                               standardize = FALSE, ...) {
  names(data) <- make.names(names(data))
  p_args <- dplyr::lst(mod, data, test.id, train.id, standardize)
  both <- purrr::invoke(prediction.default, p_args, type = "both", ...)
  pred <- both$class
  prob <- both$probs %>% magrittr::set_rownames(rownames(data[test.id, ]))
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.xgb.Booster <- function(mod, data, class, test.id = NULL,
                                   train.id = NULL, threshold = 0.5,
                                   standardize = FALSE, ...) {
  class <- factor(class)
  p_args <- dplyr::lst(mod, test.id, train.id, standardize)
  prob <- purrr::invoke(prediction.default, p_args, data = as.matrix(data),
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
  train.id <- train.id %||% seq_len(nrow(data))
  test.id <- test.id %||% seq_len(nrow(data))
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
  train <- data[train.id %||% seq_len(nrow(data)), ]
  test <- data[test.id %||% seq_len(nrow(data)), ]
  if (standardize) {
    train <- scale(train)
    test <- scale(test,
                  attr(train, "scaled:center"),
                  attr(train, "scaled:scale"))
    if (inherits(data, "data.frame")) {
      train <- as.data.frame(train)
      test <- as.data.frame(test)
    }
  }
  dplyr::lst(train, test)
}
