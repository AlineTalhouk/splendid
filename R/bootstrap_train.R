#' Bootstrap training (0.632) and internal validation
#' 
#' Training sets are bootstrap replicates of the original data sampled with 
#' replacement. Test sets comprise of all remaining samples left out from each 
#' training set, also called Out-Of-Bag samples. This framework uses the 0.632
#' bootstrap rule for large n.

#' Supervised learning classification algorithms are trained on bootstrap 
#' replicates and are evaluated on the out of bag data (not used for training).
#' 
#' The classification algorithms currently supported are: Linear Discriminant 
#' Analysis ("lda"), Random Forests ("rf"), Multinomial Classification 
#' ("multinom"), Neural Networks ("nnet"), K-Nearest Neighbours, ("knn"), 
#' Support Vector Machines ("svm"), Prediction Analysis for Microarrays ("pam"),
#' Adaptive Boosting ("adaboost"), Extreme Gradient Boosting ("xgboost"), Naive 
#' Bayes ("nb"), and Generalized Linear Models using Elastic Net model paths 
#' ("glmnet").
#' 
#' @param data data object with rows as samples, columns as features
#' @param class true/reference class vector used for supervised learning
#' @param n number of bootstrap replicates to generate (currently set to 1)
#' @param seed random seed used for reproducibility in bootstrapping results
#' @param algorithms character vector of algorithm names to use for supervised 
#'   learning. See Details for possible options. This argument is \code{NULL} by
#'   default, in which case uses all implemented algorithms.
#' @return A nested list with 2 elements
#' \item{model}{A list with an element for each algorithm, each of which is a 
#' list with length \code{n}}
#' \item{eval}{For each bootstrap sample, we can calculate various evaluation 
#' measures for the predicted classes from each algorithm. Evaluation measures 
#' include macro-averaged precision/recall/F1-score, micro-averaged precision, 
#' and (micro-averaged MCC) }
#' @export
#' @examples 
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sl_result <- bootstrap_train(hgsc, class, n = 1, algorithms = c("lda", "knn",
#' "svm"))

bootstrap_train <- function(data, class, n, seed = 1, algorithms=NULL) {
  
  # Generate bootstrap resamples; test samples are those not chosen in training
  set.seed(seed)
  class <- as.factor(class)  # ensure class is a factor
  train.idx <- training_id(data = data, class = class, n = n)
  test.idx <- purrr::map(train.idx, ~ which(!seq_len(nrow(data)) %in% .x))
  
  # Classification algorithms to use and their model function calls
  algs <- algorithms %||% ALG.NAME %>%
    stats::setNames(., .)  # if null, use all
  
  # Apply training sets to models and predict on the test sets
  name <- measure <- value <- NULL
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
    tidyr::unnest()  %>% 
    as.data.frame() %>% 
    `rownames<-`(.$name) %>% 
    extract(.,-1)
  return(list(model = models, pred = preds,evals = evals))
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