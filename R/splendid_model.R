#' Train, predict, and evaluate classification models
#'
#' @inheritParams splendid
#' @inheritSection splendid Algorithms
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' sl_result <- splendid_model(hgsc, class, n = 1, algorithms = "xgboost")
splendid_model <- function(data, class, algorithms = NULL, n = 1, seed = 1,
                           convert = FALSE, rfe = FALSE, ova = FALSE,
                           standardize = FALSE, plus = TRUE, threshold = 0,
                           ...) {

  # Classification algorithms to use and their model function calls
  algorithms <- algorithms %||% ALG.NAME %>% purrr::set_names()

  # Determine whether to convert data in the presence of categorical predictors
  data <- splendid_convert(data, algorithms, convert)

  # Generate bootstrap resamples; test samples are those not chosen in training
  set.seed(seed)
  class <- as.factor(class)  # ensure class is a factor
  train.id <- boot_train(data = data, class = class, n = n)
  test.id <- boot_test(train.id = train.id)

  # Store lists of common arguments in model and pred operations
  m_args <- dplyr::lst(train.id, data, class, algorithms, rfe, standardize)
  p_args <- dplyr::lst(data, class, test.id, train.id, threshold, standardize)

  # Apply training sets to models and predict on the test sets
  models <- sp_mod %>%
    purrr::invoke(c(m_args, f = classification, ova = FALSE))
  preds <- sp_pred %>%
    purrr::invoke(c(p_args, f = prediction, model = list(models)))

  # Train and predict One-Vs-All models if specified
  if (ova) {
    ova_models <- sp_mod %>%
      purrr::invoke(c(m_args, f = ova_classification, ova = TRUE)) %>%
      purrr::set_names(paste("ova", algorithms, sep = "_"))
    ova_preds <- sp_pred %>%
      purrr::invoke(c(p_args, f = ova_prediction, model = list(ova_models)))

    # Combine results with multiclass approach
    algorithms <- rep(algorithms, 2)
    models <- c(models, ova_models)
    preds <- c(preds, ova_preds)
  }

  # Add .632 estimator error rates to evaluation object as attributes
  err_632 <- purrr::map2(
    algorithms, preds, error_632,
    data = data, class = class, test.id = test.id, train.id = train.id,
    plus = plus
  )
  # Evaluation object
  evals <- preds %>% purrr::map(
    ~ purrr::map2(test.id, .x, ~ evaluation(class[.x], .y) %>%
                    purrr::flatten() %>%
                    unlist() %>%
                    data.frame())) %>%
    purrr::map(~ data.frame(.x) %>% magrittr::set_colnames(seq_len(n))) %>%
    purrr::map2(err_632, ~ `attr<-`(.x, ifelse(plus, "err_632plus", "err_632"),
                                    .y))

  dplyr::lst(models, preds, evals)
}

#' Recursively create training set indices ensuring class representation
#' in every bootstrap resample
#' @inheritParams splendid
#' @export
boot_train <- function(data, class, n) {
  boot <- modelr::bootstrap(data, n)
  train.id <- purrr::map(boot$strap, "idx")
  nc <- purrr::map_int(train.id, ~ dplyr::n_distinct(class[.x]))
  all.cl <- nc == nlevels(class)
  if (any(!all.cl))
    c(train.id[all.cl], boot_train(data, class, sum(!all.cl)))
  else
    train.id[all.cl]
}

#' Obtain OOB sample to use as test set
#' @param train.id list of training set indices
#' @export
boot_test <- function(train.id) {
  purrr::map(train.id, ~ which(!seq_along(.x) %in% .x))
}

#' Train models based on function f
#' @noRd
sp_mod <- function(f, train.id, data, class, algorithms, rfe, ova,
                   standardize) {
  mod <- algorithms %>% purrr::map(
    ~ purrr::map(train.id, function(id)
      f(data[id, ], class[id], .x, rfe, ova, standardize)))
  mod
}

#' Make prediction based on function f
#' @noRd
sp_pred <- function(f, model, data, class, test.id, train.id, threshold,
                    standardize, ...) {
  pred <- model %>% purrr::map(
    ~ purrr::pmap(list(.x, test.id = test.id, train.id = train.id),
                  f, data = data, class = class, threshold = threshold,
                  standardize = standardize, ...))
  pred
}
