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
#' Adaptive Boosting ("adaboost"), Extreme Gradient Boosting ("xgboost"), Naive 
#' Bayes ("nb"), and Generalized Linear Models using Elastic Net model paths 
#' ("glmnet").
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
  sm <- splendid_model(data = data, class = class, n = n,
                       algorithms = algorithms, conf.level = conf.level)
  se <- splendid_ensemble(sm = sm, data = data, class = class)
  c(sm, se)
}