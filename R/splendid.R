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
#' An ensemble classifier is constructed using Rank Aggregation across multiple
#' evaluation measures such as precision, recall, F1-score, and Matthew's
#' Correlation Coefficient (MCC).
#'
#' @section Algorithms: The classification algorithms currently supported are:
#' * Prediction Analysis for Microarrays ("pam")
#' * Support Vector Machines ("svm")
#' * Random Forests ("rf")
#' * Linear Discriminant Analysis ("lda")
#' * Shrinkage Linear Discriminant Analysis ("slda")
#' * Shrinkage Diagonal Discriminant Analysis ("sdda")
#' * Multinomial Logistic Regression using
#'     * Generalized Linear Model with no penalization ("mlr_glm")
#'     * GLM with LASSO penalty ("mlr_lasso")
#'     * GLM with ridge penalty ("mlr_ridge")
#'     * Neural Networks ("mlr_nnet")
#' * Neural Networks ("nnet")
#' * Naive Bayes ("nbayes")
#' * Adaptive Boosting ("adaboost")
#' * Extreme Gradient Boosting ("xgboost")
#' * K-Nearest Neighbours ("knn")
#'
#' @param data data frame with rows as samples, columns as features
#' @param class true/reference class vector used for supervised learning
#' @param algorithms character vector of algorithms to use for supervised
#'   learning. See \strong{Algorithms} section for possible options. By default,
#'   this argument is `NULL`, in which case all algorithms are used.
#' @param n number of bootstrap replicates to generate
#' @param seed random seed used for reproducibility in bootstrapping results
#' @param convert logical; if `TRUE`, converts all categorical variables in
#'   `data` to dummy variables. Certain algorithms only work with such
#'   limitations (e.g. LDA).
#' @param rfe logical; if `TRUE`, run Recursive Feature Elimination as a feature
#'   selection method for "lda", "rf", and "svm" algorithms.
#' @param ova logical; if `TRUE`, a One-Vs-All classification approach is
#'   performed for every algorithm in `algorithms`. The relevant results are
#'   prefixed with the string `ova_`.
#' @param standardize logical; if `TRUE`, the training sets are standardized on
#'   features to have mean zero and unit variance. The test sets are
#'   standardized using the vectors of centers and standard deviations used in
#'   corresponding training sets.
#' @param plus logical; if `TRUE` (default), the .632+ estimator is calculated.
#'   Otherwise, the .632 estimator is calculated.
#' @param threshold a number between 0 and 1 indicating the lowest maximum class
#'   probability below which a sample will be unclassified.
#' @param top the number of highest-performing algorithms to retain for ensemble
#' @param sequential logical; if `TRUE`, a sequential model is fit on the
#'   algorithms that had the best performance with one-vs-all classification.
#' @param ... additional arguments to `splendid_model`
#' @return A nested list with five elements
#' * `models`: A list with an element for each algorithm, each of which is a
#' list with length `n`. Shows the model object for each algorithm and bootstrap
#' replicate on the training set.
#' * `preds`: A list with an element for each algorithm, each of which is a list
#' with length `n`. Shows the predicted classes for each algorithm and bootstrap
#' replicate on the test set.
#' * `evals`: For each bootstrap sample, we can calculate various evaluation
#' measures for the predicted classes from each algorithm. Evaluation measures
#' include macro-averaged precision/recall/F1-score, micro-averaged precision,
#' and (micro-averaged MCC) The return value of `eval` is a tibble that shows
#' some summary statistics (e.g. mean, median) of the evaluation measures across
#' bootstrap samples, for each classification algorithm.
#' * `bests`: best-performing algorithm for each bootstrapped replicate of the
#' data, chosen by rank aggregation.
#' * `ensemble_algs`: tallies the frequencies in `bests`, returning the top
#' algorithms chosen.
#' * `ensemble`: list of model fits for each of the algorithms in
#' `ensemble_algs`, fit on the full data.
#' @author Derek Chiu
#' @export
#' @examples
#' \dontrun{
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' sl_result <- splendid(hgsc, class, n = 2, algorithms = c("lda", "xgboost"))
#' }
splendid <- function(data, class, algorithms = NULL, n = 1, seed = 1,
                     convert = FALSE, rfe = FALSE, ova = FALSE,
                     standardize = FALSE, plus = TRUE, threshold = 0, top = 3,
                     sequential = FALSE, ...) {

  algorithms <- algorithms %||% ALG.NAME %>% purrr::set_names()
  data <- splendid_convert(data, algorithms, convert)

  sm <- splendid_model(data = data, class = class, algorithms = algorithms,
                       n = n, convert = convert, rfe = rfe, ova = ova,
                       standardize = standardize, plus = plus, ...)
  se <- splendid_ensemble(sm = sm, data = data, class = class, top = top,
                          sequential = sequential)
  c(sm, se)
}
