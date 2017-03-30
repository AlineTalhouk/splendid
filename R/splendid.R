#' Bootstrap framework for Supervised Learning
#' 
#' Supervised learning classification algorithms performed on bootstrap 
#' replicates.
#' 
#' Training sets are bootstrap replicates, and test sets comprise of remaining 
#' samples not chosen for each training set. This framework uses the 0.632 
#' bootstrap rule for large n.
#' 
#' The classification algorithms currently supported are: Linear Discriminant
#' Analysis ("lda"), Random Forests ("rf"), Multinomial Classification
#' ("multinom"), Neural Networks ("nnet"), K-Nearest Neighbours, ("knn"),
#' Support Vector Machines ("svm"), Prediction Analysis for Microarrays ("pam"),
#' Adaptive Boosting ("adaboost"), Naive Bayes ("nb"), and Generalized Linear
#' Models using Elastic Net model paths ("glmnet").
#' 
#' @param data data object with rows as samples, columns as features
#' @param class reference class used for supervised learning
#' @param n number of bootstrap replicates to generate
#' @param seed random seed used for reproducibility in bootstrapping results
#' @param algorithms character vector of algorithm names to use for supervised 
#'   learning. See Details for possible options. This argument is \code{NULL} by
#'   default, meaning all implemented algorithms will be used.
#' @return A nested list with five elements: "model", "pred", "eval", 
#'   "best.algs", and "ensemble". Elements "model" and "pred" show the models 
#'   and predictions respectively across the algorithms used and each bootstrap 
#'   replicate. Element "eval" is a tibble indicating the median of the 
#'   aggregate evaluation measures for each algorithm and measure used. 
#'   Evaluation measures include accuracy, average accuracy, average precision,
#'   average recall, and average F1-score. The average is taken across the
#'   number of classes in \code{class}, and there is one average for ebery
#'   bootstrap replicate. We arrive at the final tibble by calculating the
#'   median of these measures across replicates, omitting any missing entries.
#' @author Derek Chiu
#' @export
#' @examples 
#' data(hgsc)
#' data <- hgsc
#' class <- stringr::str_split_fixed(rownames(data), "_", n = 2)[, 2]
#' sl_result <- splendid(data, class, n = 3, algorithms = c("lda", "knn",
#' "svm"))
splendid <- function(data, class, n, seed = 1, algorithms = NULL) {
  
  # Generate bootstrap resamples; test samples are those not chosen in training
  set.seed(seed)
  class <- as.factor(class)  # ensure class is a factor
  train.idx <- training_id(data = data, class = class, n = n)
  test.idx <- purrr::map(train.idx, ~ which(!seq_len(nrow(data)) %in% .x))
  
  # Classification algorithms to use and their model function calls
  algs <- algorithms %||% ALG.NAME %>%
    stats::setNames(., .)  # if null, use all
  
  # Apply training sets to models and predict on the test sets
  name <- NULL
  models <- purrr::map(algs,
                       ~ purrr::map(train.idx, function(id) 
                         classification(data[id, ], class[id], .x)))
  preds <- purrr::map(models,
                      ~ purrr::pmap(list(.x, test.idx, train.idx),
                                    prediction, data = data, class = class))
  evals <- purrr::map_at(preds, "pam", purrr::map, 1) %>% 
    purrr::map(~ purrr::map2_df(.x, test.idx, ~ evaluation(.x, class[.y]))) %>% 
    tibble::enframe() %>% 
    tidyr::unnest() %>% 
    dplyr::group_by(name) %>% 
    dplyr::summarise_all(stats::median, na.rm = TRUE) %>% 
    dplyr::arrange(match(name, ALG.NAME))
  best.algs <- purrr::map_at(preds, "pam", purrr::map, 1) %>% 
    purrr::transpose() %>% 
    purrr::map2(test.idx, ~ purrr::map_df(.x, function(d)
      evaluation(d, class[.y]))) %>% 
    purrr::map(~ as.data.frame(t(tidyr::unnest(.x)))) %>% 
    purrr::map(~ purrr::map(.x, ~ algs[order(
      rank(-.x, ties.method = "random"))])) %>% 
    purrr::map(~ purrr::invoke(rbind, .x)) %>% 
    purrr::map_chr(~ RankAggreg::RankAggreg(.x, ncol(.x), seed = seed,
                                            verbose = FALSE) %>% 
                     magrittr::use_series("top.list") %>% 
                     head(1))
  ensemble <- purrr::map(best.algs, ~ classification(data, class, .x)) %>% 
    purrr::map(~ prediction(.x, data, seq_along(class), seq_along(class),
                            class)) %>% 
    purrr::map(as.character) %>% 
    purrr::invoke(cbind, .) %>% 
    apply(1, function(x) names(which.max(table(x))))
  return(list(model = models, pred = preds, eval = evals,
              best.algs = best.algs, ensemble = ensemble))
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