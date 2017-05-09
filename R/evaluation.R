#' Evaluation of prediction performance
#' 
#' Evaluation of prediction performance on the OOB set is done using various 
#' measure for classification problems.
#' 
#' The currently supported evaluation measures include macro-averaged 
#' precision/recall/F1-score, micro-averaged precision (which is the same as 
#' recall/F1-score), Matthew's Correlation Coefficient (and its micro-averaged 
#' analog), and class-specific precision/recall/F1-score/MCC.
#' 
#' @param x actual class labels
#' @param y predicted class labels
#' @return A list with one element per evaluation measure except for the
#'   \code{cs} element, which returns a list of class-specific evaluation
#'   measures.
#' @author Derek Chiu
#' @export
#' @examples 
#' data(hgsc)
#' class <- factor(stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2])
#' set.seed(1)
#' training.id <- sample(seq_along(class), replace = TRUE)
#' test.id <- which(!seq_along(class) %in% training.id)
#' mod <- classification(hgsc[training.id, ], class[training.id], "lda")
#' pred <- prediction(mod, hgsc, test.id)
#' evaluation(class[test.id], pred)
evaluation <- function(x, y) {
  # Multiclass confusion matrix with actual as rows, predicted as columns
  cm <- as.matrix(table(Actual = x, Predicted = y))
  ocm <- ova(cm)  # One Vs. All confusion matrices
  socm <- purrr::reduce(ocm, `+`)  # Element-wise sum of ocm
  
  # Class-specific precision/recall/F1-score/MCC
  cs_p <- purrr::map_dbl(ocm, precision)
  cs_r <- purrr::map_dbl(ocm, recall)
  cs_f <- purrr::map_dbl(ocm, f1)
  cs_m <- purrr::map_dbl(ocm, mcc)
  cs <- c(precision = cs_p, recall = cs_r, f1 = cs_f, MCC = cs_m)
  
  # Discriminatory measures
  log_loss <- logloss(x, attr(y, "prob"))
  auc <- auc(x, attr(y, "prob"))
  
  # Macro-averaged precision/recall/F1-score
  macro_p <- mean(cs_p)
  macro_r <- mean(cs_r)
  macro_f <- mean(cs_f)
  
  # Micro-averaged precision (same as accuracy, micro-averaged recall/F1-score)
  micro_p <- precision(socm)
  
  # MCC and micro-averaged MCC
  MCC <- mcc(cm)
  micro_MCC <- mcc(socm)
  
  list(Logloss = log_loss, AUC = auc,
       Macro_Precision = macro_p, Macro_Recall = macro_r, Macro_F1 = macro_f,
       Micro_Precision = micro_p, MCC = MCC, Micro_MCC = micro_MCC, CS = cs)
}

#' Precision for 2 by 2 confusion matrix
#' @noRd
precision <- function(C) {
  C[1, 1] / (C[1, 1] + C[2, 1])
}

#' Recall for 2 by 2 confusion matrix
#' @noRd
recall <- function(C) {
  C[1, 1] / (C[1, 1] + C[1, 2])
}

#' F1-score for 2 by 2 confusion matrix
#' @noRd
f1 <- function(C) {
  2 * precision(C) * recall(C) / (precision(C) + recall(C))
}

#' Matthew's Correlation Coefficient (Phi Coefficient) for multiclass case
#' @references http://www.sciencedirect.com/science/article/pii/S1476927104000799
#' @noRd
mcc <- function(C) {
  N <- sum(C)
  Ct <- t(C)
  rc <- purrr::cross2(seq_len(nrow(C)), seq_len(nrow(C)))
  num <- N * sum(diag(C)) - sum(purrr::map_dbl(rc, ~ C[.x[[1]], ] %*% C[, .x[[2]]]))
  den <- sqrt(N ^ 2 - sum(purrr::map_dbl(rc, ~ C[.x[[1]], ] %*% Ct[, .x[[2]]]))) * 
    sqrt(N ^ 2 - sum(purrr::map_dbl(rc, ~ Ct[.x[[1]], ] %*% C[, .x[[2]]])))
  return(num / den)
}

#' Create One-Vs-All confusion matrices
#' @noRd
ova <- function(C) {
  # Check if there are class names to use
  if (is.null(dimnames(C)))
    nm <- seq_len(nrow(C))
  else
    nm <- dimnames(C)[[1]]
  n <- sum(C)
  purrr::map(purrr::set_names(seq_len(nrow(C)), nm), ~ {
    m <- C[.x, .x]
    cs <- sum(C[, .x])
    rs <- sum(C[.x, ])
    matrix(c(m, cs - m, rs - m, n - cs - rs + m), nrow = 2)
  })
}

#' Multi-class Log/cross-entropy Loss
#' @param x actual class labels
#' @param pred.probs predicted probabilities for each class
#' @references https://cran.r-project.org/web/packages/MLmetrics/MLmetrics.pdf 
#' @noRd
logloss <- function(x, pred.probs)
{
  if (!is.matrix(x)) {
    x <- stats::model.matrix(~ 0 + ., data.frame(as.character(x)))
  }
  eps <- 1e-15
  N <- nrow(pred.probs)
  pred.probs <- t(apply(pred.probs, 1, function(x) pmax(pmin(x, 1 - eps), eps)))
  MultiLogLoss <- (-1/N) * sum(x * log(pred.probs))
  return(MultiLogLoss)
}

#' AUC/M-index: Multiple Class Area under ROC Curve
#' @param x actual class labels
#' @param pred.probs predicted probabilities for each class
#' @references http://link.springer.com/article/10.1023/A:1010920819831
#' @noRd
auc <- function(x, pred.probs) {
  # ui-constructor for multicap class
  mcap.construct <- HandTill2001::multcap(response = x,
                                          predicted = as.matrix(pred.probs))	
  
  # multi-class auc metric	
  auc.out <- HandTill2001::auc(mcap.construct)
  return(auc.out)
}
