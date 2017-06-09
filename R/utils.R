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
#' @noRd
sum_to_one <- function(prob) {
  apply(prob, 1, function(x) {
    x <- x / sum(x)
    if (sum(x) > 1) {
      x %>% magrittr::inset(which.max(.), max(.) - (sum(.) - 1))
    } else if (sum(x) < 1) {
      x %>% magrittr::inset(which.min(.), min(.) - (sum(.) - 1))
    } else {
      x
    }
  }) %>% t()
}
