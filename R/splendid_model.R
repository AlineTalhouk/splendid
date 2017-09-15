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
                           rfe = FALSE, ova = FALSE, threshold = 0.5, ...) {

  # Classification algorithms to use and their model function calls
  algorithms <- algorithms %||% ALG.NAME %>% purrr::set_names()

  # Generate bootstrap resamples; test samples are those not chosen in training
  set.seed(seed)
  class <- as.factor(class)  # ensure class is a factor
  train.id <- boot_train(data = data, class = class, n = n)
  test.id <- boot_test(train.id = train.id)

  # Store lists of common arguments in model and pred operations
  m_args <- dplyr::lst(train.id, data, class, algorithms, rfe)
  p_args <- dplyr::lst(data, class, test.id, train.id, threshold)

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
    models <- c(models, ova_models)
    preds <- c(preds, ova_preds)
  }

  # Evaluate predictions
  evals <- preds %>% purrr::map(
    ~ purrr::map2(test.id, .x, ~ evaluation(class[.x], .y) %>%
                    purrr::flatten() %>%
                    unlist() %>%
                    data.frame())) %>%
    purrr::map(~ data.frame(.x) %>% magrittr::set_colnames(seq_len(n)))

  dplyr::lst(models, preds, evals)
}

#' Train models based on function f
#' @noRd
sp_mod <- function(f, train.id, data, class, algorithms, rfe, ova) {
  mod <- algorithms %>% purrr::map(
    ~ purrr::map(train.id, function(id)
      f(data[id, ], class[id], .x, rfe, ova)))
  mod
}

#' Make prediction based on function f
#' @noRd
sp_pred <- function(f, model, data, class, test.id, train.id, threshold, ...) {
  pred <- model %>% purrr::map(
    ~ purrr::pmap(list(.x, test.id = test.id, train.id = train.id),
                  f, data = data, class = class, threshold = threshold, ...))
  pred
}

#' Recurrsively create training set indices ensuring class representation
#' in every bootstrap resample
#' @noRd
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
#' @noRd
boot_test <- function(train.id) {
  purrr::map(train.id, ~ which(!seq_along(.x) %in% .x))
}
