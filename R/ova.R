#' One-Vs-All training approach
#'
#' @inheritParams classification
#' @return list of  binary classifier fits on each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_train <- function(data, class, algs, rfe, ova) {
  fits <- data.frame(class) %>%
    binarize() %>%
    dplyr::select(-class) %>%
    purrr::map(classification, data = data, algs = algs, rfe = rfe, ova = ova)
  fits
}

#' One-Vs-All prediction approach
#'
#' @param fits list of ova fits from \code{ova_train}
#' @inheritParams prediction
#' @return (tibble) predicted probabilities for each class
#' @author Dustin Johnson, Derek Chiu
#' @export
ova_predict <- function(fits, data, test.id, class, train.id,
                        threshold = 0.5, ...) {
  prob <- fits %>%
    purrr::map(prediction, data = data, test.id = test.id, class = class,
               train.id = train.id) %>%
    purrr::map(`%@%`, "prob") %>%
    purrr::imap(~ {  # for xgboost
      if (is.null(colnames(.x)))
        .x %>% magrittr::set_colnames(c("0", .y))
      else
        .x
      }) %>%
    purrr::map_df(~ .x[, colnames(.x) != "0"]) %>%
    as.matrix() %>%
    sum_to_one() %>%
    magrittr::set_rownames(rownames(data[test.id, ]))
  pred <- colnames(prob)[max.col(prob)]
  ct <- class_threshold(prob, threshold = threshold)
  cp <- class_proportion(ct)
  structure(pred, prob = prob, class.true = class[test.id], class.thres = ct,
            class.prop = cp)
}
