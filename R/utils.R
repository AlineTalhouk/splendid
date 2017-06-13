# magrittr placeholder
globalVariables(".")

# Algorithm functions and classes
ALG.NAME <- c("lda", "slda", "sdda", "rf", "multinom_nnet", "nnet",
              "knn", "svm", "pam", "adaboost", "xgboost", "nbayes", "lasso",
              "ridge", "multinom_glm")
ALG.CLASS <- c("lda", "sda", "randomForest", "multinom", "nnet.formula",
               "knn", "svm", "pamrtrained", "maboost", "xgb.Booster",
               "naiveBayes", "cv.glmnet", "glmnet", "rfe")

#' Redirect any console printouts from print() or cat() to null device
#' @references
#'   http://stackoverflow.com/questions/5310393/disabling-the-cat-command
#' @noRd
sink_output <- function(expr) {
  tmp <- tempfile()
  sink(tmp)
  on.exit(sink())
  on.exit(file.remove(tmp), add = TRUE)
  invisible(force(expr))
}

#' Ensure all row sums of probability matrix equal 1
#' If all probabilities are 0 from ova_model, randomly assign a class
#'
#' @noRd
sum_to_one <- function(prob) {
  apply(prob, 1, function(x) {
    if (sum(x) == 0) x[sample(seq_along(x), size = 1)] <- 1
    x / sum(x)
  }) %>% t()
}

#' Add binary One-Vs-All matrix to class vector
#'
#' @param x data frame with class labels in a column vector
#' @return data frame of \code{x} and one column for each binarized class
#'   membership
#' @noRd
binarize <- function(x) {
  cl <- factor(x$class)
  cl %>%
    levels() %>%
    purrr::set_names() %>%
    purrr::map(~ ifelse(cl == .x, .x, 0)) %>%
    purrr::prepend(list(class = as.character(cl))) %>%
    data.frame(stringsAsFactors = FALSE)
}

#' Confusion matrix
#' @noRd
conf_mat <- function(class.true, class.pred)
  as.matrix(table(class.true, class.pred))

#' Class error
#' @noRd
class_error <- function(confmat) {
  1 - (sum(diag(confmat)) / sum(confmat))
}
