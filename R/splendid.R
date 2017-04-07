#' Ensemble framework for Supervised Learning classification problems
#' 
#' Supervised learning classification algorithms are performed on bootstrap 
#' replicates and an ensemble classifier is built and evaluated across these 
#' variants.
#' 
#' Training sets are bootstrap replicates of the original data sampled with 
#' replacement. Test sets comprise of all remaining samples left out from each 
#' training set, also called Out-Of-Bag samples. This framework uses the 0.632
#' bootstrap rule for large n.
#' 
#' The classification algorithms currently supported are: Linear Discriminant 
#' Analysis ("lda"), Random Forests ("rf"), Multinomial Classification 
#' ("multinom"), Neural Networks ("nnet"), K-Nearest Neighbours, ("knn"), 
#' Support Vector Machines ("svm"), Prediction Analysis for Microarrays ("pam"),
#' Adaptive Boosting ("adaboost"), Naive Bayes ("nb"), and Generalized Linear 
#' Models using Elastic Net model paths ("glmnet").
#' 
#' An ensemble classifier is constructed using Rank Aggregation across multiple
#' evaluation measures such as precision, recall, F1-score, and Matthew's 
#' Correlation Coefficient (MCC).
#' 
#' @param data data object with rows as samples, columns as features
#' @param class true/reference class vector used for supervised learning
#' @param n number of bootstrap replicates to generate
#' @param seed random seed used for reproducibility in bootstrapping results
#' @param algorithms character vector of algorithm names to use for supervised 
#'   learning. See Details for possible options. This argument is \code{NULL} by
#'   default, in which case uses all implemented algorithms.
#' @param conf.level confidence level for bootstrapped estimates of evaluation
#'   measures
#' @return A nested list with five elements
#' \item{model}{A list with an element for each algorithm, each of which is a 
#' list with length \code{n}. Shows the model object for each algorithm and
#' bootstrap replicate on the training set.}
#' \item{pred}{A list with an element for each algorithm, each of which is a 
#' list with length \code{n}. Shows the predicted classes for each algorithm and
#' bootstrap replicate on the test set.}
#' \item{eval}{For each bootstrap sample, we can calculate various evaluation 
#' measures for the predicted classes from each algorithm. Evaluation measures 
#' include macro-averaged precision/recall/F1-score, micro-averaged precision, 
#' and (micro-averaged MCC) The return value of \code{eval} is a tibble that 
#' shows some summary statistics (e.g. mean, median) of the evaluation measures
#' across bootstrap samples, for each classification algorithm.}
#' \item{best.alg}{A length \code{n} vector of the top performing algorithms in
#' each bootstrap sample as defined by the evaluation measures and using Rank
#' Aggregation.}
#' \item{ensemble}{A predicted class vector of the entire dataset using the 
#' ensemble classifier: majority voting is used for class representation of each
#' sample across different algortithms used.}
#' @author Derek Chiu
#' @export
#' @examples 
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sl_result <- splendid(hgsc, class, n = 2, algorithms = c("lda", "knn",
#' "svm"))
splendid <- function(data, class, n, seed = 1, algorithms = NULL,
                     conf.level = 0.95) {
  
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
    purrr::map(~ purrr::map2(test.idx, .x, ~ evaluation(class[.x], .y)) %>%
                 purrr::map_df(purrr::flatten)) %>% 
    tibble::enframe() %>% 
    tidyr::unnest() %>% 
    tidyr::gather(measure, value, -name) %>% 
    dplyr::mutate(measure = factor(measure, levels = unique(measure))) %>% 
    dplyr::group_by(name, measure) %>% 
    dplyr::summarise_all(funs(min = min,
                              lower = quantile(., probs = (1 - conf.level) / 2),
                              mean = mean,
                              median = median,
                              upper = quantile(., prob = (1 - (1 - conf.level) / 2)),
                              max = max),
                         na.rm = TRUE) %>% 
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