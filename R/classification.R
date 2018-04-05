#' Multiclass classification
#'
#' Run a multiclass classification algorithm on a given dataset and reference
#' class.
#'
#' Some of the classification algorithms implemented use pre-defined values that
#' specify settings and options while others need to tune hyperparameters.
#' `"multinom"` and `"nnet"` use a maximum number of weights of 2000, in case
#' `data` is high dimensional and classification is time-consuming. `"nnet"`
#' also tunes the number of nodes (1-5) in the hidden layer. `"pam"` considers
#' 100 thresholds when training, and uses a uniform prior. `"adaboost"` calls
#' [maboost::maboost()] instead of [adabag::boosting()] for faster performance.
#' As a result, we use the `"entrop"` option, which uses the KL-divergence
#' method and mimics adaboost.
#'
#' When `alg = "knn"`, the return value is `NULL` because [class::knn()] does
#' not output an intermediate model object. The modelling and prediction is
#' performed in one step. However, the class attribute "knn" is still assigned
#' to the result in order to call the respective [prediction()] method. An
#' additional class "ova" is added if `ova = TRUE`.
#'
#' @inheritParams splendid
#' @inheritSection splendid Algorithms
#' @param algorithms character string of algorithm to use for supervised
#'   learning. See \strong{Algorithms} section for possible options.
#' @param ova logical; if `TRUE`, use the One-Vs-All approach for the `knn`
#'   algorithm.
#' @param sizes the range of sizes of features to test RFE algorithm
#' @return The model object from running the classification `algorithm`
#'
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' classification(hgsc, class, "rf")
classification <- function(data, class, algorithms, rfe = FALSE, ova = FALSE,
                           standardize = FALSE, sizes = NULL, trees = 500,
                           tune = FALSE) {
  algorithms <- match.arg(algorithms, ALG.NAME)
  class <- as.factor(class)  # ensure class is a factor
  sizes <- rfe_sizes(sizes, class)
  if (standardize) {
    if (!is.null(attr(data, "dummy_vars"))) {
      data <- dplyr::mutate_at(
        data,
        dplyr::vars(-dplyr::one_of(attr(data, "dummy_vars"))),
        scale
      )
    } else {
      data <- dplyr::mutate_all(data, scale)
    }
  }
  switch(algorithms,
         pam = pam_model(data, class),
         svm = rfe_model(data, class, "svm", rfe, sizes, trees, tune),
         rf = rfe_model(data, class, "rf", rfe, sizes, trees, tune),
         lda = rfe_model(data, class, "lda", rfe, sizes, trees, tune),
         adaboost_m1 = rfe_model(data, class, "adaboost_m1", rfe, sizes, trees, tune),
         slda = sda_model(data, class, "slda"),
         sdda = sda_model(data, class, "sdda"),
         mlr_glm = mlr_model(data, class, "mlr_glm"),
         mlr_lasso = mlr_model(data, class, "mlr_lasso"),
         mlr_ridge = mlr_model(data, class, "mlr_ridge"),
         mlr_nnet = mlr_model(data, class, "mlr_nnet"),
         nnet = nnet_model(data, class),
         nbayes = nbayes_model(data, class),
         adaboost = boost_model(data, class, "adaboost", trees),
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
rfe_model <- function(data, class, algorithms, rfe, sizes, trees, tune) {
  method <- rfe_method(algorithms)
  type <- if (tune) "range" else "default"
  tune_args <- dplyr::lst(class, method, trees, type)
  if (method == "AdaBoost.M1") names(data) <- make.names(names(data))
  if (rfe) {
    mod <- suppressWarnings(
      caret::rfe(
        x = data,
        y = class,
        sizes = sizes,
        metric = "Accuracy",
        rfeControl = caret::rfeControl(method = "cv", number = 2),
        trControl = caret::trainControl(method = "none"),
        method = method,
        tuneGrid = param_grids(data, method, type = "default")
      )
    )
    data_ov <- data[mod[["optVariables"]]]
    suppressWarnings(purrr::invoke(tune_model, tune_args, data = data_ov))
  } else {
    suppressWarnings(purrr::invoke(tune_model, tune_args, data = data))
  }
}

#' RFE sizes by default are equal to every 25th integer up to one-half of the
#' smallest class size. If class sizes are too small, use size = 1
#' @noRd
rfe_sizes <- function(sizes, class) {
  sizes <- sizes %||% {
    class %>%
      table() %>%
      min() %>%
      magrittr::divide_by_int(2) %>%
      seq_len() %>%
      magrittr::extract(. %% 25 == 0)
  }
  ifelse(length(sizes) == 0, 1, sizes)
}

#' RFE methods
#' @noRd
rfe_method <- function(algorithms) {
  switch(
    algorithms,
    lda = "lda",
    rf = "rf",
    svm = "svmRadial",
    adaboost_m1 = "AdaBoost.M1"
  )
}

#' Hyperparameter search grids. A single set is used for type "default" whereas
#' combinations of values are used for type "range"
#' @noRd
param_grids <- function(data, method, type = c("default", "range")) {
  type <- match.arg(type)
  switch(
    type,
    default = switch(
      method,
      lda = NULL,
      rf = data.frame(mtry = floor(sqrt(ncol(data)))),
      svmRadial = data.frame(sigma = mean(kernlab::sigest(as.matrix(data))[-2]),
                             C = 1),
      AdaBoost.M1 = data.frame(mfinal = 3, maxdepth = 5, coeflearn = "Breiman")
    ),
    range = switch(
      method,
      lda = NULL,
      rf = data.frame(mtry = (1:5) ^ 2),
      svmRadial = expand.grid(sigma = 1 / ncol(data) * 2 ^ (0:4),
                              C = 2 ^ (0:4)),
      AdaBoost.M1 = expand.grid(
        mfinal = 1:5,
        maxdepth = 1:5,
        coeflearn = c("Breiman", "Freund", "Zhu")
      )
    )
  )
}

#' Tune models with pre-specified search grids for hyperparameters
#' @noRd
tune_model <- function(data, class, method, trees, type) {
  caret::train(
    x = data,
    y = class,
    method = method,
    metric = "Accuracy",
    trControl = caret::trainControl(method = "cv",
                                    number = 5,
                                    classProbs = TRUE),
    tuneGrid = param_grids(data, method, type = type),
    ntree = trees
  )
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
boost_model <- function(data, class, algorithms, trees) {
  switch(algorithms,
         adaboost = sink_output(maboost::maboost(
           x = data, y = class, breg = "entrop", iter = trees)),
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
    structure(list(unique(class[class != "class_0"])),
              class = c(algorithms, "ova"))
  } else {
    structure(list(), class = algorithms)
  }
}
