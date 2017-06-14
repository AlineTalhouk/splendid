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
#' Analysis ("lda"), Shrinkage Linear Discriminant Analysis ("slda"), Shrinkage
#' Diagonal Discriminant Analysis ("sdda"), Random Forests ("rf"), Multinomial
#' Classification using Neural Networks ("multinom_nnet"), Neural Networks
#' ("nnet"), K-Nearest Neighbours, ("knn"), Support Vector Machines ("svm"),
#' Prediction Analysis for Microarrays ("pam"), Adaptive Boosting ("adaboost"),
#' Extreme Gradient Boosting ("xgboost"), Naive Bayes ("nbayes"), and
#' Generalized Linear Models using the LASSO penalty ("lasso"), ridge ("ridge")
#' penalty, or no regularization ("multinom_glm").
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
#' @param rfe logical; if \code{TRUE}, run Recursive Feature Elimination as a
#'   feature selection method for "lda", "rf", and "svm" algorithms.
#' @param ova logical; if \code{TRUE}, a One-Vs-All classification approach is
#'   performed for every algorithm in \code{algorithms}. The relevant results
#'   are prefixed with the string \code{ova_}.
#' @param threshold a numeric indicating the lowest maximum class probability
#'   below which a sample will be unclassified.
#' @param top the number of highest-performing algorithms to retain for ensemble
#' @param sequential logical; if \code{TRUE}, a sequential model is fit on the
#'   algorithms that had the best performance with one-vs-all classification.
#' @param ... additional arguments to \code{splendid_model}
#' @return A nested list with five elements
#' \item{models}{A list with an element for each algorithm, each of which is a
#' list with length \code{n}. Shows the model object for each algorithm and
#' bootstrap replicate on the training set.}
#' \item{preds}{A list with an element for each algorithm, each of which is a
#' list with length \code{n}. Shows the predicted classes for each algorithm and
#' bootstrap replicate on the test set.}
#' \item{evals}{For each bootstrap sample, we can calculate various evaluation
#' measures for the predicted classes from each algorithm. Evaluation measures
#' include macro-averaged precision/recall/F1-score, micro-averaged precision,
#' and (micro-averaged MCC) The return value of \code{eval} is a tibble that
#' shows some summary statistics (e.g. mean, median) of the evaluation measures
#' across bootstrap samples, for each classification algorithm.}
#' \item{bests}{best-performing algorithm for each bootstrapped replicate of the
#' data, chosen by rank aggregation.}
#' \item{ensemble_algs}{tallies the frequencies in \code{bests}, returning the
#' top algorithms chosen.}
#' \item{ensemble}{list of model fits for each of the algorithms in
#' \code{ensemble_algs}, fit on the full data.}
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sl_result <- splendid(hgsc, class, n = 2, algorithms = c("lda", "knn",
#' "svm"))
splendid <- function(data, class, n, seed = 1, algorithms = NULL, rfe = FALSE,
                     ova = FALSE, threshold = 0.5, top = 3, sequential = FALSE,
                     ...) {
  sm <- splendid_model(data = data, class = class, n = n,
                       algorithms = algorithms, rfe = rfe, ova = ova, ...)
  se <- splendid_ensemble(sm = sm, data = data, class = class, top = top,
                          sequential = sequential)
  c(sm, se)
}
