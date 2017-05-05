# magrittr placeholder
globalVariables(".")

# Algorithm functions and classes
ALG.NAME <- c("lda", "rf", "multinom", "nnet", "knn", "svm", "pam", "adaboost",
              "xgboost", "nb", "lasso", "ridge")
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

# Temp code for #17
# Load data and run splendid on all algorithms
data(hgsc)
class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
sl_result <- splendid(hgsc, class, n = 1)
mods <- purrr::map(sl_result$models, 1)

# Testing on training set here, but use test set in real application
predict(mods$lda, hgsc)$posterior
predict(mods$rf, hgsc, "prob")
predict(mods$multinom, hgsc, "probs")
predict(mods$nnet, hgsc)
# no knn since it uses non-parametric classification
predict(mods$svm, hgsc, probability = TRUE) %>% attr("prob")
pamr::pamr.predict(mods$pam, t(hgsc), threshold = 1, type = "posterior")
predict(mods$adaboost, hgsc %>% set_names(make.names(names(.))), "prob")
predict(mods$xgboost, as.matrix(hgsc)) %>% 
  matrix(ncol = mods$xgboost$params$num_class, byrow = TRUE)
predict(mods$nb, hgsc, "raw")
predict(mods$lasso, as.matrix(hgsc), type = "response")
predict(mods$ridge, as.matrix(hgsc), type = "response")