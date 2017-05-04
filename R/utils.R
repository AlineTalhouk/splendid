# magrittr placeholder
globalVariables(".")

# Algorithm functions and classes
ALG.NAME <- c("lda", "rf", "multinom", "nnet", "knn", "svm", "pam", "adaboost",
              "xgboost", "nb", "glmnet")
ALG.CLASS <- c("lda", "randomForest", "multinom", "nnet.formula", "knn", "svm",
               "pamrtrained", "maboost", "xgb.Booster", "naiveBayes",
               "cv.glmnet", "rfe")

#' Redirect any console printouts from print() or cat() to null device
#' @noRd
sink_output <- function(expr) {
  sink("/dev/null")
  mod <- eval(expr)
  sink()
  return(mod)
}