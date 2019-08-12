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
#' method and mimics adaboost. However, `"adaboost_m1"` calls
#' [adabag::boosting()] which supports hyperparameter tuning.
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
#' classification(hgsc, class, "xgboost")
classification <- function(data, class, algorithms, rfe = FALSE, ova = FALSE,
                           standardize = FALSE,
                           sampling = c("none", "up", "down", "smote"),
                           seed_samp = NULL, sizes = NULL, trees = 100,
                           tune = FALSE, seed_alg = NULL, convert = FALSE) {
  algorithms <- match.arg(algorithms, ALG.NAME)
  # Process the data, allow subsampling on the training data
  sp <- splendid_process(data, class, algorithms, convert, standardize, sampling, seed_samp)
  data <- sp[["data"]]
  class <- sp[["class"]]
  switch(
    algorithms,
    pam = pam_model(data, class, seed_alg),
    svm = rfe_model(data, class, "svm", rfe, sizes, tune, seed_alg = seed_alg),
    rf = rfe_model(data, class, "rf", rfe, sizes, tune, trees, seed_alg),
    lda = rfe_model(data, class, "lda", rfe, sizes, tune),
    slda = sda_model(data, class, "slda"),
    sdda = sda_model(data, class, "sdda"),
    mlr_glm = mlr_model(data, class, "mlr_glm"),
    mlr_lasso = cv_mlr_model(data, class, "mlr_lasso", seed_alg),
    mlr_ridge = cv_mlr_model(data, class, "mlr_ridge", seed_alg),
    mlr_nnet = mlr_model(data, class, "mlr_nnet"),
    nnet = nnet_model(data, class),
    nbayes = nbayes_model(data, class),
    adaboost = boost_model(data, class, "adaboost", trees, seed_alg),
    adaboost_m1 = rfe_model(data, class, "adaboost_m1", rfe, sizes,
                            tune, trees),
    xgboost = boost_model(data, class, "xgboost", seed_alg = seed_alg),
    knn = knn_model(class, "knn", ova)
  )
}

#' pam model using uniform prior probabilities for class representation
#' optimal threshold delta is selected as largest threshold among those with the
#' smallest cross-validated error
#' @noRd
pam_model <- function(data, class, seed_alg = NULL) {
  if (!is.null(seed_alg)) set.seed(seed_alg)
  nc <- dplyr::n_distinct(class)
  pamr_data <- list(x = t(data), y = class)

  mod <- sink_output(pamr::pamr.train(
    data = pamr_data,
    n.threshold = 100,
    prior = rep(1 / nc, nc)
  ))
  mod_cv <- sink_output(pamr::pamr.cv(
    fit = mod,
    data = pamr_data,
    nfold = 5
  ))
  delta <-
    mod_cv$threshold[max(which(mod_cv$error == min(mod_cv$error)))]
  mod <- c(mod, delta = delta)
  class(mod) <- "pamrtrained"
  mod
}

#' RFE model
#' @noRd
rfe_model <- function(data, class, algorithms, rfe, sizes, tune, trees = NULL,
                      seed_alg = NULL) {
  method <- rfe_method(algorithms)
  sizes <- rfe_sizes(sizes, class)
  tune_args <- tibble::lst(class, method, trees, seed_alg)
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
    data <- data[mod[["optVariables"]]]
  }
  if (!is.null(seed_alg)) set.seed(seed_alg)
  if (tune) {
    suppressWarnings(purrr::invoke(tune_model, tune_args, data = data))
  } else {
    switch(
      algorithms,
      rf = randomForest::randomForest(x = data, y = class),
      lda = suppressWarnings(MASS::lda(x = data, grouping = class)),
      svm = e1071::svm(x = data, y = class, probability = TRUE),
      adaboost_m1 = adabag::boosting(formula = class ~ .,
                                     data = cbind(data, class),
                                     mfinal = 3)
    )
  }
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

#' RFE sizes by default are equal to every 25th integer up to one-half of the
#' smallest class size. If class sizes are too small, use size = 1
#' @noRd
rfe_sizes <- function(sizes, class) {
  sizes <- sizes %||% {
    class %>%
      table() %>%
      min() %>%
      magrittr::divide_by_int(2) %>%
      seq_len(.) %>%
      magrittr::extract(. %% 25 == 0)
  }
  if (length(sizes) == 0) 1 else sizes
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
tune_model <- function(data, class, method, trees, seed_alg) {
  tune_grid <- param_grids(data, method, type = "range")
  if (is.null(seed_alg)) {
    seeds <- NA
  } else {
    seeds <- c(purrr::map(1:5, ~ sample(10 ^ 6, nrow(tune_grid))), seed_alg)
  }
  tune_args <- list(
    x = data,
    y = class,
    method = method,
    metric = "Accuracy",
    trControl = caret::trainControl(
      method = "cv",
      number = 5,
      classProbs = TRUE,
      seeds = seeds
    ),
    tuneGrid = tune_grid
  )
  if (is.null(trees)) {
    purrr::invoke(caret::train, tune_args)
  } else {
    purrr::invoke(caret::train, tune_args, ntree = trees)
  }
}

#' sda model
#' @noRd
sda_model <- function(data, class, algorithms) {
  diagonal <- switch(algorithms, slda = FALSE, sdda = TRUE)
  sda::sda(as.matrix(data), class, diagonal = diagonal, verbose = FALSE)
}

#' Multinomial Logistic Regression
#' @noRd
mlr_model <- function(data, class, algorithms) {
  switch(
    algorithms,
    mlr_nnet = nnet::multinom(class ~ ., data, MaxNWts = 2000, trace = FALSE),
    mlr_glm = glmnet::glmnet(as.matrix(data), class, lambda = 0,
                             family = "multinomial")
  )
}

#' Cross-validated regularized MLR
#' @noRd
cv_mlr_model <- function(data, class, algorithms, seed_alg = NULL) {
  if (!is.null(seed_alg)) set.seed(seed_alg)
  alpha <- switch(algorithms, mlr_lasso = 1, mlr_ridge = 0)
  glmnet::cv.glmnet(as.matrix(data), class, alpha = alpha,
                    family = "multinomial")
}

#' neural network model
#' @noRd
nnet_model <- function(data, class) {
  if (!"package:nnet" %in% search()) attachNamespace("nnet")
  suppressWarnings(e1071::best.nnet(
    class ~ .,
    data = cbind(data, class),
    size = seq_len(5),
    decay = seq(0, 0.5, length.out = 5),
    MaxNWts = 2000,
    tunecontrol = e1071::tune.control(sampling = "fix")
  ))
}

#' naive bayes model
#' @noRd
nbayes_model <- function(data, class) {
  e1071::naiveBayes(data, class)
}

#' boosting models
#' @noRd
boost_model <- function(data, class, algorithms, trees, seed_alg = NULL) {
  if (!is.null(seed_alg)) set.seed(seed_alg)
  switch(algorithms,
         adaboost = sink_output(maboost::maboost(
           x = data, y = class, breg = "entrop", iter = trees, minsplit = 2)),
         xgboost = xgboost::xgb.train(
           params = list("objective" = "multi:softprob",
                         "eval_metric" = "mlogloss",
                         "num_class" = nlevels(class)),
           data = xgboost::xgb.DMatrix(data = as.matrix(data),
                                       label = as.integer(class) - 1),
           nrounds = 2))
}

#' knn "dummy model" returns training class
#' @noRd
knn_model <- function(class, algorithms, ova) {
  if (ova) {
    structure(list(unique(class[class != "class_0"])),
              class = c(algorithms, "ova"))
  } else {
    structure(class, class = c(algorithms, "factor"))
  }
}
