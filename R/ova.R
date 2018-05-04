#' One-Vs-All training approach
#'
#' @inheritParams classification
#' @return list of binary classifier fits on each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_classification <- function(data, class, algorithms, rfe = FALSE,
                               ova = FALSE, standardize = FALSE, trees = 500,
                               tune = FALSE) {
  class %>%
    binarize() %>%
    purrr::map(~ purrr::invoke(
      .f = classification,
      .x = tibble::lst(data, algorithms, rfe, ova, standardize, trees, tune),
      class = .
    ))
}

#' One-Vs-All prediction approach
#'
#' @param fits list of ova fits from `ova_classification`
#' @inheritParams prediction
#' @return (tibble) predicted probabilities for each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_prediction <- function(fits, data, class, test.id = NULL, train.id = NULL,
                           threshold = 0, standardize = FALSE, ...) {
  prob <- fits %>%
    purrr::map(~ purrr::invoke(
      .f = prediction,
      .x = tibble::lst(data, class, test.id, train.id, threshold, standardize),
      mod = .
    )) %>%
    purrr::map(`%@%`, "prob") %>%
    purrr::imap(~ {
      colnames(.x) %>%
        purrr::when(
          is.null(.) ~ magrittr::set_colnames(.x, c("class_0", .y)), # xgboost
          ~ .x
        ) %>%
        `[`(, .y)
    }) %>%
    as.data.frame() %>%
    sum_to_one()
  pred <- factor(colnames(prob)[max.col(prob)])
  prediction_output(pred, prob, class, test.id, threshold)
}
