#' .632(+) Estimator for log loss error rate
#'
#' The .632 estimator for the log loss error rate is calculated for a given
#' classifer. The .632+ estimator is an extension that reduces overfitting and
#' is run by default.
#'
#' This function is intended to be used internally by \code{splendid_model}.
#'
#' @inheritParams splendid
#' @param algorithm character string for classifier. See \code{splendid} for
#'   possible options.
#' @param pred vector of OOB predictions using the same classifier as
#'   \code{algorithm}.
#' @param test.id vector of test set indices for each bootstrap replicate
#' @param train.id vector of training set indices for each bootstrap replicate
#' @return the .632(+) log loss error rate
#' @author Derek Chiu
#' @references Friedman, Jerome, Trevor Hastie, and Robert Tibshirani. The
#'   elements of statistical learning. Vol. 1. New York: Springer series in
#'   statistics, 2001.
#' @references Efron, Bradley and Tibshirani, Robert (1997), "Improvements on
#'   Cross-Validation: The .632+ Bootstrap Method," Journal of American
#'   Statistical Association, 92, 438, 548-560.
#'
#' @export
#' @examples
#' data(hgsc)
#' class <- as.factor(attr(hgsc, "class.true"))
#' set.seed(1)
#' train.id <- boot_train(data = hgsc, class = class, n = 10)
#' test.id <- boot_test(train.id = train.id)
#' mod <- purrr::map(train.id, ~ classification(hgsc[., ], class[.], "xgboost"))
#' pred <- purrr::pmap(list(mod = mod, test.id = test.id, train.id = train.id),
#' prediction, data = data, class = class)
#' error_632(hgsc, class, "xgboost", pred, test.id, train.id, plus = FALSE)
#' error_632(hgsc, class, "xgboost", pred, test.id, train.id, plus = TRUE)
error_632 <- function(data, class, algorithm, pred, test.id, train.id,
                      plus = TRUE) {
  err_train <- error_training(data, class, algorithm, plus)
  err_looboot <- error_looboot(pred, class, test.id, train.id)
  if (plus) {
    gamma_hat <- nier(class, attr(err_train, "pred_full"))
    R_hat <- (err_looboot - err_train) / (gamma_hat - err_train)
  } else {
    R_hat <- 0
  }
  w_hat <- .632 / (1 - .368 * R_hat)
  ((1 - w_hat) * err_train + w_hat * err_looboot) %>% `attributes<-`(NULL)
}

#' Training error rate
#' @inheritParams error_632
#' @noRd
error_training <- function(data, class, algorithm, plus) {
  pred_out <- classification(data, class, algorithm) %>%
    prediction(data, class)  # prediction on same data as was used to classify
  prob <- adjust_prob(attr(pred_out, "prob"))  # predicted probability matrix
  col <- factor(class) %>% match(levels(.))  # prob column for true class

  # Training error rate
  seq_along(class) %>%
    purrr::map_dbl(~ -1 / length(col) * log(prob[., col[.]])) %>%
    mean() %>%
    purrr::when(plus ~ magrittr::set_attr(., "pred_full", factor(pred_out)),
                TRUE ~ .)
}

#' Leave-one-out bootstrap error rate
#' @inheritParams error_632
#' @noRd
error_looboot <- function(pred, class, test.id, train.id) {
  # Error for each observation across bootstrap samples
  looboot <- seq_along(class) %>%
    purrr::map(function(obs) {
      C_i <- which(purrr::map_lgl(train.id, ~ !(obs %in% .))) # boot samples without obs
      purrr::map_dbl(C_i, function(i) {
        prob <- adjust_prob(attr(pred[[i]], "prob")) # predicted prob matrix
        col <- class[test.id[[i]]] %>% match(levels(.)) # prob column for true class
        idx <- match(obs, test.id[[i]]) # index for obs in prob and test set
        - 1 / length(col) * log(prob[idx, col[idx]]) # logloss
      })
    })

  # Leave-one-out bootstrap error rate
  looboot %>%
    purrr::compact() %>% # remove obs with |C_i| = 0
    purrr::map_dbl(mean) %>% # average within bootstrap samples for one obs
    mean() # average over all obs
}

#' Adjust predicted probability matrix so that extreme values do not cause
#' numerical instability when taking logarithms
#' @param prob predicted probability matrix
#' @param eps small epsilon value used for thresholding extremely small/large
#'   probabilities
#' @noRd
adjust_prob <- function(prob, eps = 1e-15) {
  pmax(pmin(prob, 1 - eps), eps)
}

#' No-information error rate
#' @param p vector of observed responses for each class
#' @param q vector of observed predictions for each class
#' @noRd
nier <- function(p, q) {
  p_hat <- table(p) / length(p)  # proportion of responses for each class
  q_hat <- table(q) / length(q)  # proportion of predictions for each class
  q_hat <- q_hat[match(names(p_hat), names(q_hat))] # ensure classes match
  drop(p_hat %*% (1 - q_hat))
}
