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
#'     * GLM with elastic net penalty ("mlr_enet")
#'     * Neural Networks ("mlr_nnet")
#' * Neural Networks ("nnet")
#' * Naive Bayes ("nbayes")
#' * Adaptive Boosting ("adaboost")
#' * AdaBoost.M1 ("adaboost_m1")
#' * Extreme Gradient Boosting ("xgboost")
#' * K-Nearest Neighbours ("knn")
#'
#' @param data data frame with rows as samples, columns as features
#' @param class true/reference class vector used for supervised learning
#' @param algorithms character vector of algorithms to use for supervised
#'   learning. See \strong{Algorithms} section for possible options. By default,
#'   this argument is `NULL`, in which case all algorithms are used.
#' @param n number of bootstrap replicates to generate
#' @param seed_boot random seed used for reproducibility in bootstrapping
#'   training sets for model generation
#' @param seed_samp random seed used for reproducibility in subsampling
#'   training sets for model generation
#' @param seed_alg random seed used for reproducibility when running algorithms
#'   with an intrinsic random element (random forests)
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
#' @param sampling the default is "none", in which no subsampling is performed.
#'   Other options include "up" (Up-sampling the minority class), "down"
#'   (Down-sampling the majority class), and "smote" (synthetic points for the
#'   minority class and down-sampling the majority class). Subsampling is only
#'   applicable to the training set.
#' @param stratify logical; if `TRUE`, the bootstrap resampling is performed
#'   within each strata of `class` to ensure the bootstrap sample contains the
#'   same proportions of each strata as the original data.
#' @param plus logical; if `TRUE` (default), the .632+ estimator is calculated.
#'   Otherwise, the .632 estimator is calculated.
#' @param threshold a number between 0 and 1 indicating the lowest maximum class
#'   probability below which a sample will be unclassified.
#' @param trees number of trees to use in "rf" or boosting iterations (trees) in
#'   "adaboost"
#' @param tune logical; if `TRUE`, algorithms with hyperparameters are tuned
#' @param top the number of highest-performing algorithms to retain for ensemble
#' @param seed_rank random seed used for reproducibility in rank aggregation of
#'   ensemble algorithms
#' @param sequential logical; if `TRUE`, a sequential model is fit on the
#'   algorithms that had the best performance with one-vs-all classification.
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
splendid <- function(data, class, algorithms = NULL, n = 1,
                     seed_boot = NULL, seed_samp = NULL, seed_alg = NULL,
                     convert = FALSE, rfe = FALSE, ova = FALSE,
                     standardize = FALSE,
                     sampling = c("none", "up", "down", "smote"),
                     stratify = FALSE, plus = TRUE,
                     threshold = 0, trees = 100, tune = FALSE, top = 3,
                     seed_rank = 1, sequential = FALSE) {

  algorithms <- algorithms %||% ALG.NAME %>% purrr::set_names()
  sp <- splendid_process(data, class, algorithms, convert, standardize, "none")
  data <- sp[["data"]]
  class <- sp[["class"]]

  sm_args <- tibble::lst(data, class, algorithms, n, seed_boot, seed_samp,
                         seed_alg, convert, rfe, ova, standardize, sampling,
                         stratify, plus, threshold, trees, tune)
  sm <- purrr::invoke(splendid_model, sm_args)

  se_args <- tibble::lst(sm, data, class, top, seed_rank, rfe, sequential)
  se <- purrr::invoke(splendid_ensemble, se_args)

  c(sm, se)
}
