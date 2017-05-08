# magrittr placeholder
globalVariables(".")

# Algorithm functions and classes
ALG.NAME <- c("lda", "qda", "rf", "multinom", "nnet", "knn", "svm", "pam", "adaboost",
              "xgboost", "nb", "lasso", "ridge")
ALG.CLASS <- c("lda", "qda", "randomForest", "multinom", "nnet.formula", "knn", "svm",
               "pamrtrained", "maboost", "xgb.Booster", "naiveBayes",
               "cv.glmnet", "rfe")

#' Redirect any console printouts from print() or cat() to null device
#' @noRd
sink_output <- function(expr) {
  sink("NUL")
  mod <- eval(expr)
  sink()
  if (file.exists("NUL")) file.remove("NUL")
  return(mod)
}
