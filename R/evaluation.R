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
#' @param x true class labels
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
  y_thres <- y %@% "class.thres"
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

  # Class-specific ppv/npv/sensitivity/specificity/F1-score/MCC
  cs_ppv <- purrr::map_dbl(ocm, ppv)
  cs_npv <- purrr::map_dbl(ocm, npv)
  cs_sens <- purrr::map_dbl(ocm, sensitivity)
  cs_spec <- purrr::map_dbl(ocm, specificity)
  cs_f1 <- purrr::map_dbl(ocm, f1)
  cs_mcc <- purrr::map_dbl(ocm, mcc)
  cs <- c(ppv = cs_ppv, npv = cs_npv, sensitivity = cs_sens,
          specificity = cs_spec, f1 = cs_f1, mcc = cs_mcc)

  # Discriminatory measures
  dm_funs <- tibble::lst(logloss, auc, pdi)
  if (plot) {
    dm_funs <- c(dm_funs,
                 tibble::lst(discrimination_plot, reliability_plot, roc_plot))
  }
  dm <- purrr::invoke_map(dm_funs, x = x, probs = probs)

  # Accuracy, macro-averaged PPV/NPV/Sensitivity/Specificity/F1-score, MCC
  suppressWarnings({
    accuracy <- yardstick::accuracy_vec(x, y)
    macro_ppv <- yardstick::ppv_vec(x, y)
    macro_npv <- yardstick::npv_vec(x, y)
    macro_sensitivity <- yardstick::sens_vec(x, y)
    macro_specificity <- yardstick::spec_vec(x, y)
    macro_f1 <- yardstick::f_meas_vec(x, y)
    mcc <- yardstick::mcc_vec(x, y)
  })

  c(dm[c("logloss", "auc", "pdi")],
    tibble::lst(accuracy, macro_ppv, macro_npv, macro_sensitivity,
                macro_specificity, macro_f1, mcc, cs))
}

#' PPV (Precision) for 2 by 2 confusion matrix
#' @noRd
ppv <- function(C) {
  suppressWarnings(yardstick::ppv(C)[[".estimate"]])
}

#' NPV for 2 by 2 confusion matrix
#' @noRd
npv <- function(C) {
  suppressWarnings(yardstick::npv(C)[[".estimate"]])
}

#' Sensitivity (Recall) for 2 by 2 confusion matrix
#' @noRd
sensitivity <- function(C) {
  suppressWarnings(yardstick::sens(C)[[".estimate"]])
}

#' Specificity for 2 by 2 confusion matrix
#' @noRd
specificity <- function(C) {
  suppressWarnings(yardstick::spec(C)[[".estimate"]])
}

#' F1-score for 2 by 2 confusion matrix
#' @noRd
f1 <- function(C) {
  suppressWarnings(yardstick::f_meas(C)[[".estimate"]])
}

#' Matthews correlation coefficient for 2 by 2 confusion matrix
#' @references
#'   https://www.sciencedirect.com/science/article/pii/S1476927104000799
#' @noRd
mcc <- function(C) {
  suppressWarnings(yardstick::mcc(C)[[".estimate"]])
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
#' @inheritParams evaluation
#' @inheritParams splendid_graphs
#' @noRd
logloss <- function(x, probs) {
  yardstick::mn_log_loss_vec(x, probs)
}

#' AUC/M-index: Multiple Class Area under ROC Curve
#' @inheritParams evaluation
#' @inheritParams splendid_graphs
#' @references http://link.springer.com/article/10.1023/A:1010920819831
#' @noRd
auc <- function(x, probs) {
  mcap.construct <-
    suppressWarnings(HandTill2001::multcap(response = x,
                                           predicted = as.matrix(probs)))
  HandTill2001::auc(mcap.construct)
}

#' Polytomous Discrimination Index (PDI)
#'
#' Based on `mcca::pdi`
#'
#' @inheritParams evaluation
#' @inheritParams splendid_graphs
#' @references http://onlinelibrary.wiley.com/doi/10.1002/sim.5321/abstract
#' @noRd
pdi <- function(x, probs) {
  x <- as.integer(x)
  cl <- seq_len(dplyr::n_distinct(x))
  n <- purrr::map(cl, ~ which(x == .))
  pdi_all <- purrr::map_dbl(cl, function(j) {
    sum(purrr::map_dbl(seq_along(n[[j]]), function(i) {
      prod(purrr::map_int(purrr::map(n[-j], ~ probs[., j]),
                          ~ sum(probs[n[[j]][i], j] > .)))
    }))
  })
  sum(pdi_all) / (length(pdi_all) * prod(purrr::map_int(n, length)))
}
