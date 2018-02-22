#' .632(+) Estimator for log loss error rate
#'
#' The .632 estimator for the log loss error rate is calculated for a given
#' classifier. The .632+ estimator is an extension that reduces overfitting and
#' is run by default.
#'
#' This function is intended to be used internally by `splendid_model`.
#'
#' @inheritParams splendid
#' @param algorithm character string for classifier. See `splendid` for possible
#'   options.
#' @param pred vector of OOB predictions using the same classifier as
#'   `algorithm`.
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
#' \dontrun{
#' data(hgsc)
#' class <- as.factor(attr(hgsc, "class.true"))
#' set.seed(1)
#' train.id <- boot_train(data = hgsc, class = class, n = 5)
#' test.id <- boot_test(train.id = train.id)
#' mod <- purrr::map(train.id, ~ classification(hgsc[., ], class[.], "xgboost"))
#' pred <- purrr::pmap(list(mod = mod, test.id = test.id, train.id = train.id),
#' prediction, data = hgsc, class = class)
#' error_632(hgsc, class, "xgboost", pred, test.id, train.id, plus = FALSE)
#' error_632(hgsc, class, "xgboost", pred, test.id, train.id, plus = TRUE)
#' }
error_632 <- function(data, class, algorithm, pred, test.id, train.id,
                      plus = TRUE) {
  err <- error_training(data, class, algorithm, plus)
  err_1 <- error_looboot(pred, class, test.id, train.id)
  err_632 <- .368 * err + .632 * err_1
  if (plus) {
    gamma <- nier(class, attr(err, "pred_full"))
    err_1p <- min(err_1, gamma)
    R <- ifelse(err_1p > err & gamma > err, (err_1p - err) / (gamma - err), 0)
    err_632plus <- err_632 + (err_1p - err) * (.368 * .632 * R) / (1 - .368 * R)
    `attributes<-`(err_632plus, NULL)
  } else {
    `attributes<-`(err_632, NULL)
  }
}

#' Training error rate
#' @inheritParams error_632
#' @noRd
error_training <- function(data, class, algorithm, plus) {
  pred_out <- classification(data, class, algorithm) %>%
    prediction(data, class)  # prediction on same data as was used to classify
  prob <- adjust_prob(attr(pred_out, "prob"))  # predicted probability matrix

  # Training error rate
  logloss(class, prob) %>%
    purrr::when(plus ~ `attr<-`(., "pred_full", factor(pred_out)), ~ .)
}

#' Leave-one-out bootstrap error rate
#' @inheritParams error_632
#' @noRd
error_looboot <- function(pred, class, test.id, train.id) {
  # Error for each obs in boot samples where it wasn't selected to train
  looboot <- seq_along(class) %>%
    purrr::map(function(obs) {
      C_i <- which(purrr::map_lgl(train.id, ~ !(obs %in% .))) # boot id w/o obs
      purrr::map_dbl(C_i, function(i) {
        prob <- adjust_prob(attr(pred[[i]], "prob")) # predicted prob matrix
        col <- class[test.id[[i]]] %>% match(levels(.)) # column index for class
        idx <- match(obs, test.id[[i]]) # index for obs in prob and col
        - log(prob[idx, col[idx]]) # logloss for single obs
      })
    })

  # Average errors after removing obs that were selected in all training sets
  looboot %>%
    purrr::compact() %>% # remove obs with |C_i| = 0
    purrr::map_dbl(mean) %>% # average within bootstrap samples for each obs
    mean() # average across all obs
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
