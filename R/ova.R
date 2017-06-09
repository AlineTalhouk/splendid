#' One-Vs-All training approach
#'
#' @inheritParams splendid
#' @return list of  binary classifier fits on each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_train <- function(data, class, algs, rfe) {
  fits <- data.frame(class) %>%
    binarize() %>%
    dplyr::select(-class) %>%
    purrr::map(classification, data = data, algs = algs, rfe = rfe)
  fits
}

#' One-Vs-All prediction approach
#'
#' @param fits list of ova fits from \code{ova_train}
#' @inheritParams splendid
#' @return (tibble) predicted probabilities for each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_predict <- function(fits, data, test.id, class, threshold = 0.5, ...) {
  prob <- fits %>%
    purrr::map(prediction, data = data, test.id = test.id, class = class) %>%
    purrr::map(`%@%`, "prob") %>%
    purrr::map_df(~ .x[, 2]) %>%
    as.matrix() %>%
    sum_to_one() %>%
    magrittr::set_rownames(rownames(data[test.id, ]))
  pred <- factor(unique(class)[max.col(prob)])
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}
