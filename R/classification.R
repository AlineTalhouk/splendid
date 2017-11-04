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
#' @param algorithms character string of algorithm to use for supervised
#'   learning. See \strong{Algorithms} section for possible options.
#' @param ova logical; if \code{TRUE}, use the One-Vs-All approach for the
#'   \code{knn} algorithm.
#' @param sizes the range of sizes of features to test RFE algorithm
#' @return The model object from running the classification \code{algorithm}
#'
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' classification(hgsc, class, "rf")
classification <- function(data, class, algorithms, rfe = FALSE, ova = FALSE,
                           standardize = FALSE, sizes = NULL) {
  algorithms <- match.arg(algorithms, ALG.NAME)
  class <- as.factor(class)  # ensure class is a factor
  sizes <- rfe_sizes(sizes, class)
  if (standardize) {
    if (!is.null(attr(data, "dummy_vars"))) {
      data <- dplyr::mutate_at(data,
                               vars(-dplyr::one_of(attr(data, "dummy_vars"))),
                               scale)
    } else {
      data <- dplyr::mutate_all(data, scale)
    }
  }
  switch(algorithms,
         pam = pam_model(data, class),
         svm = rfe_model(data, class, "svm", rfe, sizes),
         rf = rfe_model(data, class, "rf", rfe, sizes),
         lda = rfe_model(data, class, "lda", rfe, sizes),
         slda = sda_model(data, class, "slda"),
         sdda = sda_model(data, class, "sdda"),
         mlr_glm = mlr_model(data, class, "mlr_glm"),
         mlr_lasso = mlr_model(data, class, "mlr_lasso"),
         mlr_ridge = mlr_model(data, class, "mlr_ridge"),
         mlr_nnet = mlr_model(data, class, "mlr_nnet"),
         nnet = nnet_model(data, class),
         nbayes = nbayes_model(data, class),
         adaboost = boost_model(data, class, "adaboost"),
         xgboost = boost_model(data, class, "xgboost"),
         knn = knn_model(class, "knn", ova)
  )
}

#' pam model
#' @noRd
pam_model <- function(data, class) {
  sink_output(pamr::pamr.train(
    list(x = t(data),
         y = class),
    n.threshold = 100,
    prior = pam_prior(class)
  ))
}

#' Uniform prior probabilities for class representation
#' @noRd
pam_prior <- function(class) {
  nc <- dplyr::n_distinct(class)
  rep(1 / nc, nc)
}

#' RFE model
#' @noRd
rfe_model <- function(data, class, algorithms, rfe, sizes) {
  if (rfe) {
    funcs <- switch(algorithms,
                    lda = caret::ldaFuncs,
                    rf = caret::rfFuncs,
                    svm = caret::caretFuncs)
    method <- if (algorithms == "svm") "svmRadial" else NULL
    mod <- suppressPackageStartupMessages(suppressWarnings(
      caret::rfe(data, class, sizes = sizes, method = method,
                 rfeControl = caret::rfeControl(functions = funcs,
                                                method = "cv",
                                                number = 2))))
    if (algorithms != "svm") {
      mod
    } else {
      svm_model(data, class, mod$optVariables)
    }
  } else {
    switch(algorithms,
           lda = suppressWarnings(MASS::lda(data, grouping = class)),
           rf =  randomForest::randomForest(data, y = class),
           svm = svm_model(data, class, names(data))
    )
  }
}

#' RFE sizes by default are equal to every 25th integer up to one-half of the
#' smallest class size
#' @noRd
rfe_sizes <- function(sizes, class) {
  sizes <- sizes %||% class %>%
    table() %>%
    min() %>%
    magrittr::divide_by_int(2) %>%
    seq_len() %>%
    magrittr::extract(. %% 25 == 0)
  sizes
}

#' support vector machine model with tuning
#' @noRd
svm_model <- function(data, class, vars) {
  e1071::best.svm(x = data[, vars], y = class, probability = TRUE,
                  gamma = 1 / ncol(data) * 2 ^ (0:4), cost = 2 ^ (0:4),
                  tunecontrol = e1071::tune.control(sampling = "fix"))
}

#' sda model
#' @noRd
sda_model <- function(data, class, algorithms) {
  diagonal <- switch(algorithms, slda = FALSE, sdda = TRUE)
  sda::sda(as.matrix(data), class, diagonal = diagonal, verbose = FALSE)
}

#' mlr model
#' @noRd
mlr_model <- function(data, class, algorithms) {
  if (algorithms == "mlr_nnet") {
    nnet::multinom(class ~ ., data, MaxNWts = 2000, trace = FALSE)
  } else if (algorithms == "mlr_glm") {
    glmnet::glmnet(as.matrix(data), class, lambda = 0, family = "multinomial")
  } else {
    alpha <- switch(algorithms, mlr_lasso = 1, mlr_ridge = 0)
    glmnet::cv.glmnet(as.matrix(data), class, alpha = alpha,
                      family = "multinomial")
  }
}

#' neural network model
#' @noRd
nnet_model <- function(data, class) {
  if (!"package:nnet" %in% search()) attachNamespace("nnet")
  e1071::best.nnet(
    class ~ .,
    data = cbind(data, class),
    size = seq_len(5),
    decay = seq(0, 0.5, length.out = 5),
    MaxNWts = 2000,
    tunecontrol = e1071::tune.control(sampling = "fix")
  )
}

#' naive bayes model
#' @noRd
nbayes_model <- function(data, class) {
  e1071::naiveBayes(data, class)
}

#' boosting models
#' @noRd
boost_model <- function(data, class, algorithms) {
  switch(algorithms,
         adaboost = sink_output(maboost::maboost(data, class, breg = "entrop")),
         xgboost = xgboost::xgb.train(
           params = list("objective" = "multi:softprob",
                         "eval_metric" = "mlogloss",
                         "num_class" = dplyr::n_distinct(class)),
           data = xgboost::xgb.DMatrix(data = as.matrix(data),
                                       label = as.integer(class) - 1),
           nrounds = 2))
}

#' knn dummy model
#' @noRd
knn_model <- function(class, algorithms, ova) {
  if (ova) {
    structure(list(unique(class[class != "0"])),
              class = c(algorithms, "ova"))
  } else {
    structure(list(), class = algorithms)
  }
}
