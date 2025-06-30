#' Variable Importance
#'
#' Methods to calculate variable importance for different classifiers.
#'
#' Currently, variable importance methods are implemented for these classifiers:
#' * "rf"
#' * "xgboost",
#' * "mlr_ridge", "mlr_lasso"
#' * "adaboost"
#' * "svm"
#' * "nnet"
#'
#' @inheritParams prediction
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' mod <- classification(hgsc, class, "xgboost")
#' var_imp(mod)
var_imp <- function(mod, data, ...) {
  UseMethod("var_imp")
}

#' @rdname var_imp
#' @export
var_imp.default <- function(mod, data, ...) {
  tryCatch(
    vip::vi(mod, ...),
    error = function(e) NULL
  )
}

#' @rdname var_imp
#' @export
var_imp.randomForest <- function(mod, data, ...) {
  var_imp.default(mod, ...)
}

#' @rdname var_imp
#' @export
var_imp.cv.glmnet <- function(mod, data, ...) {
  var_imp.default(mod, ...)
}

#' @rdname var_imp
#' @export
var_imp.xgb.Booster <- function(mod, data, ...) {
  var_imp.default(mod, ...)
}

#' @rdname var_imp
#' @export
var_imp.nnet <- function(mod, data, ...) {
  if (!requireNamespace("NeuralNetTools", quietly = TRUE)) {
    stop("Package \"NeuralNetTools\" is needed. Please install it.",
         call. = FALSE)
  } else {
    var_imp.default(mod, type = "garson", ...)
  }
}

#' @rdname var_imp
#' @export
var_imp.maboost <- function(mod, data, ...) {
  loadNamespace("maboost")
  mod %>%
    maboost::varplot.maboost(plot.it = FALSE,
                             type = "scores",
                             max.var.show = Inf) %>%
    tibble::enframe(name = "Variable", value = "Importance")
}

#' @rdname var_imp
#' @export
var_imp.train <- function(mod, data, ...) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package \"caret\" is needed. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("fastshap", quietly = TRUE)) {
    stop("Package \"fastshap\" is needed. Please install it.",
         call. = FALSE)
  } else {
    pfun <- function(object, newdata) {
      caret::predict.train(object, newdata = newdata, type = "prob")[, 1]
    }
    mod %>%
      vip::vi_shap(pred_wrapper = pfun) %>%
      dplyr::arrange(dplyr::desc(.data$Importance))
  }
}

#' @rdname var_imp
#' @export
var_imp.svm <- function(mod, data, ...) {
  pfun <- function(object, newdata) {
    stats::predict(object, newdata = newdata, probability = TRUE) %>%
      attr("probabilities") %>%
      `[`(, 1)
  }
  mod %>%
    vip::vi_shap(
      feature_names = names(data),
      train = data,
      pred_wrapper = pfun
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Importance))
}
