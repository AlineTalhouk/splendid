#' One-Vs-All training approach
#'
#' @inheritParams classification
#' @return list of binary classifier fits on each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_classification <- function(data, class, algorithms, rfe = FALSE,
                               ova = FALSE, standardize = FALSE, trees = 500,
                               tune = FALSE) {
  ova_args <- tibble::lst(data, algorithms, rfe, ova, standardize, trees, tune)
  class %>%
    binarize() %>%
    purrr::map(~ purrr::invoke(classification, ova_args, class = .))
}

#' One-Vs-All prediction approach
#'
#' @param fits list of ova fits from `ova_classification`
#' @inheritParams prediction
#' @return (tibble) predicted probabilities for each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_prediction <- function(fits, data, test.id = NULL, class, train.id,
                           threshold = 0, standardize = FALSE, ...) {
  prob <- fits %>%
    purrr::map(prediction, data = data, test.id = test.id, class = class,
               train.id = train.id, standardize = standardize) %>%
    purrr::map(`%@%`, "prob") %>%
    purrr::map2(.x = ., .y = names(.), ~ {
      colnames(.x) %>%
        purrr::when(
          is.null(.) ~ magrittr::set_colnames(.x, c("class_0", .y)),# for xgboost
          ~ .x
        )
    }) %>%
    purrr::map2(.x = ., .y = names(.), ~ .x[, .y]) %>%
    as.data.frame() %>%
    sum_to_one()
  pred <- factor(colnames(prob)[max.col(prob)])
  prediction_output(pred, prob, class, test.id, threshold)
}
