#' Train, predict, and evaluate classification models
#'
#' @inheritParams splendid
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sl_result <- splendid_model(hgsc, class, n = 1, algorithms = "xgboost")
splendid_model <- function(data, class, n, seed = 1, algorithms = NULL,
                           rfe = FALSE, ova = FALSE, threshold = 0.5, ...) {

  # Generate bootstrap resamples; test samples are those not chosen in training
  set.seed(seed)
  class <- as.factor(class)  # ensure class is a factor
  train.idx <- training_id(data = data, class = class, n = n)
  test.idx <- purrr::map(train.idx, ~ which(!seq_len(nrow(data)) %in% .x))

  # Classification algorithms to use and their model function calls
  algs <- algorithms %||% ALG.NAME %>% purrr::set_names()  # if null, use all

  # Apply training sets to models and predict on the test sets
  models <- sp_mod(classification, train.idx, data, class, algs, rfe,
                   ova = FALSE)
  preds <- sp_pred(prediction, models, test.idx, train.idx, data, class,
                   threshold)

  # Train and predict One-Vs-All models if specified
  if (ova) {
    ova_models <- sp_mod(ova_classification, train.idx, data, class, algs, rfe,
                         ova = TRUE) %>%
      purrr::set_names(paste("ova", algs, sep = "_"))
    ova_preds <- sp_pred(ova_prediction, ova_models, test.idx, train.idx, data,
                         class, threshold)

    # Combine results with conventional approach
    models <- c(models, ova_models)
    preds <- c(preds, ova_preds)
  }

  # Evaluate predictions
  evals <- preds %>% purrr::map(
    ~ purrr::map2(test.idx, .x, ~ evaluation(class[.x], .y))) %>%
    purrr::modify_depth(2, ~ purrr::flatten(.x) %>%
                          unlist() %>%
                          data.frame()) %>%
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
sp_pred <- function(f, model, test.id, train.id, data, class, threshold, ...) {
  pred <- model %>% purrr::map(
    ~ purrr::pmap(list(.x, test.id, train.id),
                  f, data = data, class = class, threshold = threshold, ...))
  pred
}

#' Recurrsively create training set indices ensuring class representation
#' in every bootstrap resample
#' @noRd
training_id <- function(data, class, n) {
  boot <- modelr::bootstrap(data, n)
  train.idx <- purrr::map(boot$strap, "idx")
  nc <- purrr::map_int(train.idx, ~ dplyr::n_distinct(class[.x]))
  all.cl <- nc == nlevels(class)
  if (any(!all.cl))
    return(c(train.idx[all.cl], training_id(data, class, sum(!all.cl))))
  else
    return(train.idx[all.cl])
}
