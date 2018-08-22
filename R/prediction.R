#' Class prediction on OOB set
#'
#' Functions to predict class labels on the Out-Of-Bag (test) set for different
#' classifiers.
#'
#' The `knn` and `pamr` prediction methods use the `train.id` and `class`
#' arguments for additional modelling steps before prediction. For `knn`, the
#' modelling and prediction are performed in one step, so the function takes in
#' both training and test set identifiers. For `pamr`, the classifier needs to
#' be cross-validated on the training set in order to find a shrinkage threshold
#' with the minimum CV error to use in prediction on the test set. The other
#' prediction methods make use of the default method.
#'
#' @inheritParams splendid
#' @param mod model object from [classification()]
#' @param test.id integer vector of indices for test set. If `NULL` (default),
#'   all samples are used.
#' @param train.id integer vector of indices for training set. If `NULL`
#'   (default), all samples are used.
#' @param ... additional arguments to be passed to or from methods
#' @return A factor of predicted classes with labels in the same order as true
#'   class. If `mod` is a `"pamr"` classifier, the return value is a list of
#'   length 2: the predicted class, and the threshold value.
#'
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' set.seed(1)
#' training.id <- sample(seq_along(class), replace = TRUE)
#' test.id <- which(!seq_along(class) %in% training.id)
#' mod <- classification(hgsc[training.id, ], class[training.id], "slda")
#' pred <- prediction(mod, hgsc, class, test.id)
#' table(true = class[test.id], pred)
prediction <- function(mod, data, class, test.id = NULL, train.id = NULL,
                       threshold = 0, standardize = FALSE, ...) {
  if (!inherits(mod, ALG.CLASS)) {
    match.arg(class(mod), ALG.CLASS)
  } else {
    UseMethod("prediction")
  }
}

#' @rdname prediction
#' @export
prediction.default <- function(mod, data, class, test.id = NULL,
                               train.id = NULL, threshold = 0,
                               standardize = FALSE, ...) {
  dat <- split_data(data, test.id, train.id, standardize)
  stats::predict(mod, dat$test, ...)
}

#' @rdname prediction
#' @export
prediction.pamrtrained <- function(mod, data, class, test.id = NULL,
                                   train.id = NULL, threshold = 0,
                                   standardize = FALSE, ...) {
  loadNamespace("pamr")
  dat <- split_data(data, test.id, train.id, standardize) %>% purrr::map(t)
  cvdat <- list(x = dat$train, y = class[train.id])
  model.cv <- sink_output(pamr::pamr.cv(mod, cvdat, nfold = 5))
  delta <- model.cv$threshold[which.min(model.cv$error)]
  p_args <- tibble::lst(fit = mod, newx = dat$test, threshold = delta)
  pred <- purrr::invoke(pamr::pamr.predict, p_args, type = "class")
  prob <- purrr::invoke(pamr::pamr.predict, p_args, type = "posterior")
  prediction_output(pred, prob, class, test.id, threshold) %>%
    structure(delta = delta)
}

#' @export
prediction.train <- function(mod, data, class, test.id = NULL, train.id = NULL,
                             threshold = 0, standardize = FALSE, ...) {
  loadNamespace("caret")
  opt_vars <- head(names(mod[["trainingData"]]), -1)
  if (mod[["method"]] == "AdaBoost.M1") names(data) <- make.names(names(data))
  p_args <- tibble::lst(mod, test.id, train.id, standardize,
                        data = data[opt_vars])
  pred <- purrr::invoke(prediction.default, p_args, type = "raw")
  prob <- purrr::invoke(prediction.default, p_args, type = "prob")
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.svm <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0, standardize = FALSE, ...) {
  loadNamespace("e1071")
  names(data) <- make.names(names(data))
  p_args <- tibble::lst(mod, test.id, train.id, standardize,
                        data = data[colnames(mod$SV)])
  pred <- purrr::invoke(prediction.default, p_args, probability = TRUE, ...) %>%
    unname()
  prob <- attr(pred, "probabilities")
  if (!("class_0" %in% mod$levels))
    prob <- prob[, order(colnames(prob), mod$levels)]
  prediction_output(pred, prob, class, test.id, threshold) %>%
    structure(probabilities = NULL)
}

#' @export
prediction.randomForest <- function(mod, data, class, test.id = NULL,
                                    train.id = NULL, threshold = 0,
                                    standardize = FALSE, ...) {
  loadNamespace("randomForest")
  p_args <- tibble::lst(mod, data, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, type = "response") %>%
    unname()
  prob <- purrr::invoke(prediction.default, p_args, type = "prob")
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.lda <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0, standardize = FALSE, ...) {
  loadNamespace("MASS")
  p_args <- tibble::lst(mod, test.id, train.id, standardize,
                        data = data[colnames(mod$means)])
  p <- purrr::invoke(prediction.default, p_args, ...)
  pred <- p$class
  prob <- p$posterior %>% sum_to_one()
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.sda <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0, standardize = FALSE, ...) {
  loadNamespace("sda")
  p_args <- tibble::lst(mod, test.id, train.id, standardize)
  p <- purrr::invoke(prediction.default, p_args, data = as.matrix(data),
                     verbose = FALSE, ...)
  pred <- p$class
  prob <- p$posterior %>% sum_to_one()
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.cv.glmnet <- function(mod, data, class, test.id = NULL,
                                 train.id = NULL, threshold = 0,
                                 standardize = FALSE, ...) {
  loadNamespace("glmnet")
  p_args <- tibble::lst(mod, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, data = as.matrix(data),
                        type = "class", ...) %>% factor()
  prob <- purrr::invoke(prediction.default, p_args, data = as.matrix(data),
                        type = "response", ...)[, , 1]
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.glmnet <- function(mod, data, class, test.id = NULL, train.id = NULL,
                              threshold = 0, standardize = FALSE, ...) {
  p_args <- tibble::lst(mod, data, class, test.id, train.id, threshold,
                        standardize)
  purrr::invoke(prediction.cv.glmnet, p_args, ...)
}

#' @export
prediction.multinom <- function(mod, data, class, test.id = NULL,
                                train.id = NULL, threshold = 0,
                                standardize = FALSE, ...) {
  loadNamespace("nnet")
  p_args <- tibble::lst(mod, data, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, type = "class", ...)
  prob <- purrr::invoke(prediction.default, p_args, type = "probs", ...)
  if (!is.matrix(prob)) {
    prob <- matrix(c(1 - prob, prob), ncol = 2, dimnames = list(NULL, mod$lev))
  }
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.nnet.formula <- function(mod, data, class, test.id = NULL,
                                    train.id = NULL, threshold = 0,
                                    standardize = FALSE, ...) {
  loadNamespace("e1071")
  p_args <- tibble::lst(mod, data, test.id, train.id, standardize)
  pred <- factor(purrr::invoke(prediction.default, p_args, type = "class", ...))
  prob <- purrr::invoke(prediction.default, p_args, type = "raw", ...)
  if (ncol(prob) == 1) {
    prob <- matrix(c(1 - prob, prob), ncol = 2, dimnames = list(NULL, mod$lev))
  }
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.naiveBayes <- function(mod, data, class, test.id = NULL,
                                  train.id = NULL, threshold = 0,
                                  standardize = FALSE, ...) {
  loadNamespace("e1071")
  p_args <- tibble::lst(mod, data, test.id, train.id, standardize)
  pred <- purrr::invoke(prediction.default, p_args, type = "class", ...)
  prob <- purrr::invoke(prediction.default, p_args, type = "raw", ...) %>%
    magrittr::set_rownames(rownames(data[test.id, ]))
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.boosting <- function(mod, data, class, test.id = NULL,
                                train.id = NULL, threshold = 0,
                                standardize = FALSE, ...) {
  loadNamespace("adabag")
  names(data) <- make.names(names(data))
  p_args <- tibble::lst(mod, data, test.id, train.id, standardize)
  p <- purrr::invoke(prediction.default, p_args, ...)
  pred <- factor(p$class)
  prob <- p$prob %>% magrittr::set_rownames(rownames(data[test.id, ]))
  if (ncol(prob) == nlevels(class)) {
    colnames(prob) <- levels(class)
  }
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.maboost <- function(mod, data, class, test.id = NULL,
                               train.id = NULL, threshold = 0,
                               standardize = FALSE, ...) {
  loadNamespace("maboost")
  names(data) <- make.names(names(data))
  p_args <- tibble::lst(mod, data, test.id, train.id, standardize)
  both <- purrr::invoke(prediction.default, p_args, type = "both", ...)
  pred <- both$class
  if (is.null(test.id)) {
    rn <- rownames(data)
  } else {
    rn <- rownames(data[test.id, ])
  }
  prob <- both$probs %>% magrittr::set_rownames(rn)
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @export
prediction.xgb.Booster <- function(mod, data, class, test.id = NULL,
                                   train.id = NULL, threshold = 0,
                                   standardize = FALSE, ...) {
  loadNamespace("xgboost")
  p_args <- tibble::lst(mod, test.id, train.id, standardize)
  prob <- purrr::invoke(prediction.default, p_args, data = as.matrix(data),
                        reshape = TRUE, ...) %>%
    magrittr::set_rownames(rownames(data[test.id, ])) %>%
    sum_to_one()
  class <- factor(class)
  if (ncol(prob) == nlevels(class)) {
    colnames(prob) <- levels(class)
  }
  pred <- factor(levels(class)[max.col(data.frame(prob))])
  prediction_output(pred, prob, class, test.id, threshold)
}

#' @rdname prediction
#' @export
prediction.knn <- function(mod, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0, standardize = FALSE, ...) {
  train.id <- train.id %||% seq_len(nrow(data))
  test.id <- test.id %||% seq_len(nrow(data))
  dat <- split_data(data, test.id, train.id, standardize) %>% {
    rbind(.$train, .$test)
  }
  if (inherits(mod, "ova")) {
    lev <- as.character(unlist(mod))
    if (length(lev) == 1) class <- ifelse(class == lev, lev, 0)
  }

  kdist <- knn.dist(dat)
  kparams <- list(train = seq_along(train.id),
                  test = length(train.id) + seq_along(test.id),
                  y = factor(class[train.id]), dist.matrix = kdist, k = 5)
  pred <- unname(factor(purrr::invoke(knn.predict, kparams)))
  prob <- t(purrr::invoke(knn.probability, kparams))
  prediction_output(pred, prob, class, test.id, threshold)
}

#' Prediction output with attributes
#' @noRd
prediction_output <- function(pred, prob, class, test.id, threshold) {
  if (is.null(class)) {
    ctr <- NULL
  } else {
    if (is.null(test.id)) {
      ctr <- class
    } else {
      ctr <- class[test.id]
    }
  }
  cth <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(cth)

  structure(
    pred,
    prob = prob,
    class.true = ctr,
    class.thres = cth,
    class.prop = cp
  )
}

#' Predicted class labels above a max class probability threshold
#' @noRd
class_threshold <- function(prob, threshold = 0) {
  prob %>%
    as.data.frame() %>%
    dplyr::mutate(
      !!"max_prop" := purrr::pmap_dbl(., max),
      !!"max_class" := purrr::pmap(., list) %>%
        purrr::map_chr(~ names(which.max(.))) %>%
        ifelse(.data$max_prop >= threshold, ., "unclassified") %>%
        factor() %>%
        forcats::fct_expand(colnames(prob))
    ) %>%
    dplyr::pull()
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
  tibble::lst(train, test)
}
