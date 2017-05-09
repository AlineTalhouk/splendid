# magrittr placeholder
globalVariables(".")

# Algorithm functions and classes
ALG.NAME <- c("lda", "qda", "rf", "multinom", "nnet", "knn", "svm", "pam",
              "adaboost", "xgboost", "nbayes", "lasso", "ridge")
ALG.CLASS <- c("lda", "qda", "randomForest", "multinom", "nnet.formula", "knn",
               "svm", "pamrtrained", "maboost", "xgb.Booster", "naiveBayes",
               "cv.glmnet", "rfe")

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
