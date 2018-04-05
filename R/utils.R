# magrittr placeholder
globalVariables(".")

# Algorithm functions and classes
ALG.NAME <- c("pam", "svm", "rf", "lda", "slda", "sdda", "mlr_glm", "mlr_lasso",
              "mlr_ridge", "mlr_nnet", "nnet", "nbayes", "adaboost",
              "adaboost_m1", "xgboost", "knn")
ALG.CLASS <- c("pamrtrained", "train", "sda", "cv.glmnet", "glmnet", "multinom",
               "nnet.formula", "naiveBayes", "maboost", "boosting",
               "xgb.Booster", "knn")

# Algorithms that need all continuous predictors
ALG.CONT <- c("lda", "mlr_glm", "mlr_lasso", "mlr_ridge")

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

#' Ensure all row sums of probability matrix equal 1 If all probabilities are 0
#' from ova_model, randomly assign a class
#'
#' @noRd
sum_to_one <- function(prob) {
  apply(prob, 1,
        function(x) {
          if (sum(x) == 0) x[sample(seq_along(x), size = 1)] <- 1
          x / sum(x)
        }
  ) %>% t()
}

#' Add binary One-Vs-All matrix to class vector
#'
#' @param x class label vector
#' @return tibble of `x` and one column for each binarized class membership
#' @noRd
binarize <- function(x) {
  x %>%
    unique() %>%
    sort() %>%
    as.character() %>%
    purrr::set_names() %>%
    purrr::map_df(~ ifelse(x == ., ., "class_0"))
}

#' Confusion matrix
#' @noRd
conf_mat <- function(reference, prediction)
  as.matrix(table(reference = reference, prediction = prediction))

#' Class error
#' @noRd
class_error <- function(confmat) {
  1 - (sum(diag(confmat)) / sum(confmat))
}
