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
#' \itemize{
#'   \item Prediction Analysis for Microarrays ("pam")
#'   \item Support Vector Machines ("svm")
#'   \item Random Forests ("rf")
#'   \item Linear Discriminant Analysis ("lda")
#'   \item Shrinkage Linear Discriminant Analysis ("slda")
#'   \item Shrinkage Diagonal Discriminant Analysis ("sdda")
#'   \item Multinomial Logistic Regression using
#'   \itemize{
#'     \item Generalized Linear Model with no penalization ("mlr_glm")
#'     \item GLM with LASSO penalty ("mlr_lasso")
#'     \item GLM with ridge penalty ("mlr_ridge")
#'     \item Neural Networks ("mlr_nnet")
#'   }
#'   \item Neural Networks ("nnet")
#'   \item Naive Bayes ("nbayes")
#'   \item Adaptive Boosting ("adaboost")
#'   \item Extreme Gradient Boosting ("xgboost")
#'   \item K-Nearest Neighbours ("knn")
#' }
#'
#' @param data data frame with rows as samples, columns as features
#' @param class true/reference class vector used for supervised learning
#' @param algorithms character vector of algorithms to use for supervised
#'   learning. See \strong{Algorithms} section for possible options. By default,
#'   this argument is \code{NULL}, in which case all algorithms are used.
#' @param n number of bootstrap replicates to generate
#' @param seed random seed used for reproducibility in bootstrapping results
#' @param convert logical; if \code{TRUE}, converts all categorical variables in
#'   \code{data} to dummy variables. Certain algorithms only work with such
#'   limitations (e.g. LDA).
#' @param rfe logical; if \code{TRUE}, run Recursive Feature Elimination as a
#'   feature selection method for "lda", "rf", and "svm" algorithms.
#' @param ova logical; if \code{TRUE}, a One-Vs-All classification approach is
#'   performed for every algorithm in \code{algorithms}. The relevant results
#'   are prefixed with the string \code{ova_}.
#' @param standardize logical; if \code{TRUE}, the training sets are
#'   standardized by per feature to have mean zero and unit variance. The test
#'   sets are standardized using the vectors of centers and standard deviations
#'   used in corresponding training sets.
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
#' class <- attr(hgsc, "class.true")
#' sl_result <- splendid(hgsc, class, n = 2, algorithms = c("lda", "knn",
#' "xgboost"))
splendid <- function(data, class, algorithms = NULL, n = 1, seed = 1,
                     convert = FALSE, rfe = FALSE, ova = FALSE,
                     standardize = FALSE, threshold = 0.5, top = 3,
                     sequential = FALSE, ...) {

  algorithms <- algorithms %||% ALG.NAME %>% purrr::set_names()
  data <- splendid_convert(data, algorithms, convert)

  sm <- splendid_model(data = data, class = class, algorithms = algorithms,
                       n = n, convert = convert, rfe = rfe, ova = ova,
                       standardize = standardize, ...)
  se <- splendid_ensemble(sm = sm, data = data, class = class, top = top,
                          sequential = sequential)
  c(sm, se)
}
