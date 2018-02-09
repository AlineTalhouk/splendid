#' One-Vs-All training approach
#'
#' @inheritParams classification
#' @return list of binary classifier fits on each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_classification <- function(data, class, algorithms, rfe = FALSE,
                               ova = FALSE, standardize = FALSE) {
  fits <- class %>%
    binarize() %>%
    purrr::map(classification, data = data, algorithms = algorithms, rfe = rfe,
               ova = ova, standardize = standardize)
  fits
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
          is.null(.) ~ magrittr::set_colnames(.x, c("0", .y)),  # for xgboost
          ~ .x
        )
    }) %>%
    purrr::map(~ .x[, colnames(.x) != "0"]) %>%
    as.data.frame() %>%
    sum_to_one()
  pred <- colnames(prob)[max.col(prob)]
  prediction_output(pred, prob, class, test.id, threshold)
}
