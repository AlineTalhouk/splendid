#' Variable Importance
#'
#' Methods to calculate variable importance for different classifiers.
#' @inheritParams prediction
#'
#' @author Derek Chiu
#' @export
var_imp <- function(mod, ...) {
  UseMethod("var_imp")
}

#' @rdname var_imp
#' @export
var_imp.default <- function(mod, ...) {
  vip::vi(mod)
}

#' @rdname var_imp
#' @export
var_imp.randomForest <- function(mod, ...) {
  var_imp.default(mod, ...)
}

#' @rdname var_imp
#' @export
var_imp.maboost <- function(mod, ...) {
  loadNamespace("maboost")
  mod %>%
    maboost::varplot.maboost(plot.it = FALSE,
                             type = "scores",
                             max.var.show = Inf) %>%
    tibble::enframe(name = "Variable", value = "Importance")
}

#' @rdname var_imp
#' @export
var_imp.train <- function(mod, ...) {
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
