#' Train, predict, and evaluate classification models
#'
#' @inheritParams splendid
#' @inheritSection splendid Algorithms
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' sl_result <- splendid_model(hgsc, class, n = 1, algorithms = "xgboost")
splendid_model <- function(data, class, algorithms = NULL, n = 1,
                           seed_boot = NULL, seed_samp = NULL, seed_alg = NULL,
                           convert = FALSE, rfe = FALSE, ova = FALSE,
                           standardize = FALSE,
                           sampling = c("none", "up", "down", "smote"),
                           stratify = FALSE, plus = TRUE, threshold = 0,
                           trees = 100, tune = FALSE, vi = FALSE) {

  # Classification algorithms to use and their model function calls
  algorithms <- algorithms %||% ALG.NAME %>% purrr::set_names()

  # Process the data, but don't allow subsampling yet
  sp <- splendid_process(data, class, algorithms, convert, standardize, "none")
  data <- sp[["data"]]
  class <- sp[["class"]]

  # Generate bootstrap resamples; test samples are those not chosen in training
  if (!is.null(seed_boot)) set.seed(seed_boot)
  train.id <- boot_train(data = data, class = class, n = n, stratify = stratify)
  test.id <- boot_test(train.id = train.id)

  # Store lists of common arguments in model and pred operations
  m_args <- tibble::lst(train.id, data, class, algorithms, rfe, standardize,
                        sampling, trees, tune, seed_samp, seed_alg)
  p_args <- tibble::lst(data, class, test.id, train.id, threshold, standardize)

  # Apply training sets to models and predict on the test sets
  models <- sp_mod %>%
    rlang::exec(!!!c(m_args, f = classification, ova = FALSE))
  preds <- sp_pred %>%
    rlang::exec(!!!c(p_args, f = prediction, model = list(models)))

  # Train and predict One-Vs-All models if specified
  if (ova) {
    ova_models <- sp_mod %>%
      rlang::exec(!!!c(m_args, f = ova_classification, ova = TRUE)) %>%
      rlang::set_names(paste("ova", algorithms, sep = "_"))
    ova_preds <- sp_pred %>%
      rlang::exec(!!!c(p_args, f = ova_prediction, model = list(ova_models)))

    # Combine results with multiclass approach
    algorithms <- rep(algorithms, 2)
    models <- c(models, ova_models)
    preds <- c(preds, ova_preds)
  }

  # Add .632 estimator error rates to evaluation object as attributes
  err_632 <- purrr::map2(
    algorithms,
    preds,
    error_632,
    data = data,
    class = class,
    test.id = test.id,
    train.id = train.id,
    plus = plus
  )

  # Evaluation object
  evals <- preds %>% purrr::map(
    ~ purrr::map2(test.id, ., ~ evaluation(class[.x], .y) %>%
                    purrr::flatten() %>%
                    unlist() %>%
                    data.frame())) %>%
    purrr::map(~ magrittr::set_colnames(data.frame(.), seq_len(n))) %>%
    purrr::map2(err_632, ~ `attr<-`(.x, ifelse(plus, "err_632plus", "err_632"),
                                    .y))

  # Variable importance object optionally returned
  if (vi) {
    vis <- purrr::map_depth(models, 2, var_imp, data = data)
    tibble::lst(models, preds, evals, vis)
  } else {
    tibble::lst(models, preds, evals)
  }
}

#' Recursively create training set indices ensuring class representation
#' in every bootstrap resample
#' @inheritParams splendid
#' @export
boot_train <- function(data, class, n, stratify = FALSE) {
  if (stratify) {
    data <- data.frame(data, class)
    boot <- rsample::bootstraps(data, n, "class")
  } else {
    boot <- rsample::bootstraps(data, n)
  }
  train.id <- purrr::map(boot$splits, "in_id")
  nc <- purrr::map_int(train.id, ~ dplyr::n_distinct(class[.]))
  all.cl <- nc == nlevels(class)
  if (any(!all.cl))
    c(train.id[all.cl], boot_train(data, class, sum(!all.cl), stratify))
  else
    train.id[all.cl]
}

#' Obtain OOB sample to use as test set
#' @param train.id list of training set indices
#' @export
boot_test <- function(train.id) {
  purrr::map(train.id, ~ which(!seq_along(.) %in% .))
}

#' Train models based on function f
#' @noRd
sp_mod <- function(f, train.id, data, class, algorithms, rfe, ova,
                   standardize, sampling, trees, tune, seed_samp, seed_alg) {
  mod <- algorithms %>% purrr::map(
    ~ purrr::map(train.id, function(id)
      f(data = data[id, ], class = class[id], algorithms = ., rfe = rfe,
        ova = ova, standardize = standardize, sampling = sampling,
        trees = trees, tune = tune, seed_samp = seed_samp,
        seed_alg = seed_alg)))
  mod
}

#' Make prediction based on function f
#' @noRd
sp_pred <- function(f, model, data, class, test.id, train.id, threshold,
                    standardize, ...) {
  pred <- model %>% purrr::map(
    ~ purrr::pmap(list(., test.id = test.id, train.id = train.id),
                  f, data = data, class = class, threshold = threshold,
                  standardize = standardize, ...))
  pred
}
