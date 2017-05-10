#' Train, predict, and evaluate classification models
#'
#' @inheritParams splendid
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sl_result <- splendid_model(hgsc, class, n = 1, algorithms = "svm")
splendid_model <- function(data, class, n, seed = 1, algorithms = NULL,
                           conf.level = 0.95, rfe = FALSE, ...) {

  # Generate bootstrap resamples; test samples are those not chosen in training
  set.seed(seed)
  class <- as.factor(class)  # ensure class is a factor
  train.idx <- training_id(data = data, class = class, n = n)
  test.idx <- purrr::map(train.idx, ~ which(!seq_len(nrow(data)) %in% .x))

  # Classification algorithms to use and their model function calls
  algs <- algorithms %||% ALG.NAME %>%
    stats::setNames(., .)  # if null, use all

  # Apply training sets to models and predict on the test sets
  models <- purrr::map(algs,
                       ~ purrr::map(train.idx, function(id)
                         classification(data[id, ], class[id], .x, rfe)))
  preds <- purrr::map(models,
                      ~ purrr::pmap(list(.x, test.idx, train.idx),
                                    prediction, data = data, class = class,
                                    ...))
  evals <- purrr::map_at(preds, "pam", purrr::map, 1) %>%
    purrr::map(~ purrr::map2(test.idx, .x, ~ evaluation(class[.x], .y))) %>%
    purrr::at_depth(2, ~ purrr::flatten(.x) %>%
                      unlist() %>%
                      data.frame()) %>%
    purrr::map(~ data.frame(.x) %>% magrittr::set_colnames(seq_len(n)))
  dplyr::lst(models, preds, evals)
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
