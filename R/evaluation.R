#' Evaluation of prediction performance
#'
#' Evaluation of prediction performance on the OOB set is done using various
#' measure for classification problems.
#'
#' The currently supported evaluation measures include discriminatory measures
#' like log loss, AUC, and PDI, macro-averaged PPV (Precision)/Sensitivity
#' (Recall)/F1-score, accuracy (same as micro-averaged PPV
#' Sensitivity/F1-score), Matthew's Correlation Coefficient (and its
#' micro-averaged analog), and class-specific PPV/Sensitivity/F1-score/MCC.
#'
#' @param x actual class labels
#' @param y predicted class labels
#' @param plot logical; if `TRUE` a discrimination plot and reliability plot are
#'   shown for each class
#' @return A list with one element per evaluation measure except for the `cs`
#'   element, which returns a list of class-specific evaluation measures.
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- factor(attr(hgsc, "class.true"))
#' set.seed(1)
#' training.id <- sample(seq_along(class), replace = TRUE)
#' test.id <- which(!seq_along(class) %in% training.id)
#' mod <- classification(hgsc[training.id, ], class[training.id], "xgboost")
#' pred <- prediction(mod, hgsc, class, test.id)
#' evaluation(class[test.id], pred)
evaluation <- function(x, y, plot = FALSE) {
  # Remove unclassified cases unless they're all unclassified or remaining cases
  # does not span all classes
  y <- y %>% forcats::lvls_expand(levels(x))  # add levels with 0 predictions
  y_thres <- y %@% "class.thres" %>%
    forcats::fct_relevel(sort(levels(.)))  # reorder levels
  keep_ind <- y_thres != "unclassified"
  n_class <- dplyr::n_distinct(y_thres[keep_ind])
  probs <- y %@% "prob"
  if (sum(keep_ind) > 0 && n_class == nlevels(x)) {
    probs <- probs[keep_ind, ]
    x <- x[keep_ind]
    y <- y_thres[keep_ind] %>% forcats::fct_drop(only = "unclassified")
  }

  # Multiclass confusion matrix with actual as rows, predicted as columns
  cm <- conf_mat(x, y)
  ocm <- ova(cm)  # One Vs. All confusion matrices
  socm <- purrr::reduce(ocm, `+`)  # Element-wise sum of ocm

  # Class-specific ppv/sensitivity/F1-score/MCC
  cs_p <- purrr::map_dbl(ocm, ppv)
  cs_s <- purrr::map_dbl(ocm, sensitivity)
  cs_f <- purrr::map_dbl(ocm, f1)
  cs_m <- purrr::map_dbl(ocm, mcc)
  cs <- c(ppv = cs_p, sensitivity = cs_s, f1 = cs_f, mcc = cs_m)

  # Discriminatory measures
  dm_funs <- tibble::lst(logloss, auc, pdi)
  if (plot) {
    dm_funs <- c(dm_funs, tibble::lst(discrimination_plot, reliability_plot))
  }
  dm <- dm_funs %>%
    purrr::invoke_map(list(list(x = x, pred.probs = probs)))

  # Accuracy (same as micro-averaged ppv/sensitivity/F1-score)
  accuracy <- sum(diag(cm)) / sum(cm)

  # Macro-averaged ppv/sensitivity/F1-score
  macro_ppv <- mean(cs_p)
  macro_sensitivity <- mean(cs_s)
  macro_f1 <- mean(cs_f)

  # MCC and micro-averaged MCC
  mcc <- mcc(cm)
  micro_mcc <- mcc(socm)

  if (plot) dm[c("discrimination_plot", "reliability_plot")]

  c(dm[c("logloss", "auc", "pdi")],
    tibble::lst(accuracy, macro_ppv, macro_sensitivity, macro_f1, mcc,
                micro_mcc, cs))
}

#' PPV (Precision) for 2 by 2 confusion matrix
#' @noRd
ppv <- function(C) {
  C[1, 1] / (C[1, 1] + C[2, 1])
}

#' Sensitivity (Recall) for 2 by 2 confusion matrix
#' @noRd
sensitivity <- function(C) {
  C[1, 1] / (C[1, 1] + C[1, 2])
}

#' F1-score for 2 by 2 confusion matrix
#' @noRd
f1 <- function(C) {
  2 * ppv(C) * sensitivity(C) / (ppv(C) + sensitivity(C))
}

#' Matthew's Correlation Coefficient (Phi Coefficient) for multiclass case
#' @references
#'   http://www.sciencedirect.com/science/article/pii/S1476927104000799
#' @noRd
mcc <- function(C) {
  N <- sum(C)
  rc <- as.data.frame(which(is.finite(C), arr.ind = TRUE))
  num <- N * sum(diag(C)) - sum(purrr::pmap_dbl(rc, ~ C[.x, ] %*% C[, .y]))
  den <- sqrt(N ^ 2 - sum(purrr::pmap_dbl(rc, ~ C[.x, ] %*% C[.y, ]))) *
    sqrt(N ^ 2 - sum(purrr::pmap_dbl(rc, ~ C[, .x] %*% C[, .y])))
  num / den
}

#' Create One-Vs-All confusion matrices
#' @noRd
ova <- function(C) {
  # Check if there are class names to use
  if (is.null(dimnames(C)))
    nm <- seq_len(nrow(C))
  else
    nm <- dimnames(C)[[1]]

  seq_len(nrow(C)) %>%
    purrr::set_names(nm) %>%
    purrr::map(~ {
      m <- C[., .]
      cs <- sum(C[, .])
      rs <- sum(C[., ])
      matrix(c(m, cs - m, rs - m, sum(C) - cs - rs + m), nrow = 2)
    })
}

#' Multi-class Log/cross-entropy Loss
#' @param x actual class labels
#' @param pred.probs predicted probabilities for each class
#' @noRd
logloss <- function(x, pred.probs) {
  MLmetrics::MultiLogLoss(as.matrix(pred.probs), x)
}

#' AUC/M-index: Multiple Class Area under ROC Curve
#' @param x actual class labels
#' @param pred.probs predicted probabilities for each class
#' @references http://link.springer.com/article/10.1023/A:1010920819831
#' @noRd
auc <- function(x, pred.probs) {
  # ui-constructor for multicap class
  mcap.construct <- HandTill2001::multcap(
    response = x,
    predicted = as.matrix(pred.probs)
  )
  HandTill2001::auc(mcap.construct)  # multi-class auc metric
}

#' Polytomous Discrimination Index (PDI)
#'
#' Based on `mcca::pdi`
#'
#' @param x actual class labels
#' @param pred.probs predicted probabilities for each class
#' @references http://onlinelibrary.wiley.com/doi/10.1002/sim.5321/abstract
#' @noRd
pdi <- function(x, pred.probs) {
  x <- as.integer(x)
  cl <- seq_len(dplyr::n_distinct(x))
  n <- purrr::map(cl, ~ which(x == .))
  pdi_all <- purrr::map_dbl(cl, function(j) {
    sum(purrr::map_dbl(seq_along(n[[j]]), function(i) {
      prod(purrr::map_int(purrr::map(n[-j], ~ pred.probs[., j]),
                          ~ sum(pred.probs[n[[j]][i], j] > .)))
    }))
  })
  sum(pdi_all) / (length(pdi_all) * prod(purrr::map_int(n, length)))
}
