#' Multiclass classification
#'
#' Run a multiclass classification algorithm on a given dataset and reference
#' class.
#'
#' Some of the classification algorithms implemented use pre-defined values that
#' specify settings and options while others need to tune hyperparameters.
#' \code{"multinom"} and \code{"nnet"} use a maximum number of weights of 2000,
#' in case \code{data} is high dimensional and classification is time-consuming.
#' \code{"nnet"} also tunes the number of nodes (1-5) in the hidden layer.
#' \code{"pam"} considers 100 thresholds when training, and uses a uniform
#' prior. \code{"adaboost"} calls \code{\link[maboost]{maboost}} instead of
#' \code{\link[adabag]{boosting}} for faster performance. As a result, we use
#' the \code{"entrop"} option, which uses the KL-divergence method and mimics
#' adaboost.
#'
#' When \code{alg = "knn"}, the return value is \code{NULL} because
#' \code{\link[class]{knn}} does not output an intermediate model object. The
#' modelling and prediction is performed in one step. However, the class
#' attribute "knn" is still assigned to the result in order to call the
#' respective \code{\link{prediction}} method. An additional class "ova" is
#' added if \code{ova = TRUE}.
#'
#' @inheritParams splendid
#' @inheritSection splendid Algorithms
#' @param ova logical; if \code{TRUE}, use the One-Vs-All approach for the
#'   \code{knn} algorithm.
#' @param sizes the range of sizes of features to test RFE algorithm
#' @return The model object from running the classification algorithm
#'   \code{"alg"}
#'
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' classification(hgsc, class, "rf")
classification <- function(data, class, algorithms, rfe = FALSE, ova = FALSE,
                           sizes = NULL) {
  algorithms <- match.arg(algorithms, ALG.NAME)
  class <- as.factor(class)  # ensure class is a factor
  sizes <- sizes %||% seq_len(round(min(table(class)) / 2)) %>%
    magrittr::extract(. %% 25 == 0)
  switch(algorithms,
         lda = {
           if (!rfe)
             suppressWarnings(MASS::lda(data, grouping = class))
           else
             rfe_model(data, class, algorithms, sizes)
         },
         slda = sda::sda(as.matrix(data), class, diagonal = FALSE,
                         verbose = FALSE),
         sdda = sda::sda(as.matrix(data), class, diagonal = TRUE,
                         verbose = FALSE),
         rf = {
           if (!rfe)
             randomForest::randomForest(data, y = class)
           else
             rfe_model(data, class, algorithms, sizes)
         },
         multinom_nnet = nnet::multinom(class ~ ., data, MaxNWts = 2000,
                                        trace = FALSE),
         nnet = {
           if (!"package:nnet" %in% search()) attachNamespace("nnet")
           e1071::best.nnet(class ~ ., data = cbind(data, class),
                            size = seq_len(5),
                            decay = seq(0, 0.5, length.out = 5),
                            MaxNWts = 2000,
                            tunecontrol = e1071::tune.control(
                              sampling = "fix"))
         },
         knn = {
           if (ova) {
             structure(list(unique(class[class != "0"])),
                       class = c("knn", "ova"))
           } else {
             structure(list(), class = "knn")
           }
         },
         svm = {
           if (!rfe) {
             opt_var <- names(data)
           } else {
             mod <- rfe_model(data, class, algorithms, sizes)
             opt_var <- mod$optVariables
           }
           e1071::best.svm(x = data[, opt_var], y = class,
                           probability = TRUE,
                           gamma = 1 / ncol(data) * 2 ^ (0:4),
                           cost = 2 ^ (0:4),
                           tunecontrol = e1071::tune.control(
                             sampling = "fix"))
         },
         pam = sink_output(
           pamr::pamr.train(list(x = t(data), y = class), n.threshold = 100,
                            prior = rep(1 / dplyr::n_distinct(class),
                                        dplyr::n_distinct(class)))),
         adaboost = sink_output(maboost::maboost(data, class, breg = "entrop")),
         xgboost = xgboost::xgb.train(
           params = list("objective" = "multi:softprob",
                         "eval_metric" = "mlogloss",
                         "num_class" = dplyr::n_distinct(class)),
           data = xgboost::xgb.DMatrix(data = as.matrix(data),
                                       label = as.integer(class) - 1),
           nrounds = 2),
         nbayes = e1071::naiveBayes(data, class),
         lasso = glmnet::cv.glmnet(as.matrix(data), class, alpha = 1,
                                   family = "multinomial"),
         ridge = glmnet::cv.glmnet(as.matrix(data), class, alpha = 0,
                                   family = "multinomial"),
         multinom_glm = glmnet::glmnet(as.matrix(data), class, lambda = 0,
                                       family = "multinomial")
  )
}

#' RFE model
#' @noRd
rfe_model <- function(data, class, algorithms, sizes) {
  funcs <- switch(algorithms,
                  lda = caret::ldaFuncs,
                  rf = caret::rfFuncs,
                  svm = caret::caretFuncs)
  method <- if (algorithms == "svm") "svmRadial" else NULL
  suppressPackageStartupMessages(suppressWarnings(
    caret::rfe(data, class, sizes = sizes, method = method,
               rfeControl = caret::rfeControl(functions = funcs, method = "cv",
                                              number = 2))))
}
