#' @noRd
error_632 <- function(data, class, algorithm, pred, test.id, train.id) {
  err_train <- error_training(data, class, algorithm)
  err_looboot <- error_looboot(pred, class, test.id, train.id)
  0.368 * err_train + 0.632 * err_looboot
}

#' @noRd
error_training <- function(data, class, algorithm) {
  # Naive prediction on same data as was used to classify
  prob <- classification(data, class, algorithm) %>%
    prediction(data, class) %>%
    attr("prob") %>%
    adjust_prob()
  col <- class %>%
    factor() %>%
    match(levels(.))

  # Training error rate
  seq_along(class) %>%
    purrr::map_dbl(~ -1 / length(col) * log(prob[., col[.]])) %>%
    mean()
}

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
#' @noRd
adjust_prob <- function(prob, eps = 1e-15) {
  pmax(pmin(prob, 1 - eps), eps)
}
