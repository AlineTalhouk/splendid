#' Evaluation of prediction performance
#' 
#' Evaluation of prediction performance on the OOB set is done using various 
#' measure for classification problems.
#' 
#' The currently supported evaluation measures include discriminatory measures 
#' like log loss and AUC, macro-averaged PPV (Precision)/Sensitivity 
#' (Recall)/F1-score, accuracy (same as micro-averaged PPV 
#' Sensitivity/F1-score), Matthew's Correlation Coefficient (and its 
#' micro-averaged analog), and class-specific PPV/Sensitivity/F1-score/MCC.
#' 
#' @param x actual class labels
#' @param y predicted class labels
#' @param plot logical; if \code{TRUE} a discrimination plot is shown for each 
#'   class
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
#' mod <- classification(hgsc[training.id, ], class[training.id], "xgboost")
#' pred <- prediction(mod, hgsc, test.id, class)
#' evaluation(class[test.id], pred, plot = TRUE)
evaluation <- function(x, y, plot = FALSE) {
  # Multiclass confusion matrix with actual as rows, predicted as columns
  cm <- as.matrix(table(Actual = x, Predicted = y))
  ocm <- ova(cm)  # One Vs. All confusion matrices
  socm <- purrr::reduce(ocm, `+`)  # Element-wise sum of ocm
  
  # Class-specific ppv/sensitivity/F1-score/MCC
  cs_p <- purrr::map_dbl(ocm, ppv)
  cs_s <- purrr::map_dbl(ocm, sensitivity)
  cs_f <- purrr::map_dbl(ocm, f1)
  cs_m <- purrr::map_dbl(ocm, mcc)
  cs <- c(ppv = cs_p, sensitivity = cs_s, f1 = cs_f, mcc = cs_m)
  
  # Discriminatory measures
  dm_funs <- dplyr::lst(logloss, auc)
  if (plot) {
    dm_funs <- c(dm_funs, dplyr::lst(discrimination_plot))
  }
  dm <- dm_funs %>% 
    purrr::invoke_map(list(list(x = x, pred.probs = attr(y, "prob"))))
  
  # Accuracy (same as micro-averaged ppv/sensitivity/F1-score)
  accuracy <- sum(diag(cm)) / sum(cm)
  
  # Macro-averaged ppv/sensitivity/F1-score
  macro_ppv <- mean(cs_p)
  macro_sensitivity <- mean(cs_s)
  macro_f1 <- mean(cs_f)

  # MCC and micro-averaged MCC
  mcc <- mcc(cm)
  micro_mcc <- mcc(socm)
  
  if (plot) dm$discrimination_plot
  
  c(dm[c("logloss", "auc")], dplyr::lst(accuracy, macro_ppv, macro_sensitivity,
                                        macro_f1, mcc, micro_mcc, cs))
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
#' @references http://www.sciencedirect.com/science/article/pii/S1476927104000799
#' @noRd
mcc <- function(C) {
  N <- sum(C)
  Ct <- t(C)
  rc <- purrr::cross2(seq_len(nrow(C)), seq_len(nrow(C)))
  num <- N * sum(diag(C)) - sum(purrr::map_dbl(rc,
                                               ~ C[.x[[1]], ] %*% C[, .x[[2]]]))
  den <- sqrt(N ^ 2 - sum(purrr::map_dbl(rc,
                                         ~ C[.x[[1]], ] %*% Ct[, .x[[2]]]))) * 
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

#' Discrimination plot: boxplots of the predicted probabilities for each
#' outcome category according to each observed outcome category
#' @references http://onlinelibrary.wiley.com/doi/10.1002/sim.5321/abstract
#' @noRd
discrimination_plot <- function(x, pred.probs) {
  
  # turn into long-form for plotting
  df.long <- data.frame(trueClass = x, pred.probs) %>% 
    tidyr::gather(key = "class", value = "prob", -1, factor_key = TRUE)
  
  # create prevalance (base-line) class proportion table
  df.prevalence <- df.long %>%
    dplyr::group_by_("trueClass") %>% 
    dplyr::summarise_("classCount" = ~ length(trueClass)) %>% 
    dplyr::mutate_("totalCount" = ~ sum(classCount),
                   "prevalence" = ~ classCount / totalCount)
  
  # discrimination plot
  p <- ggplot(df.long, aes(x = class, y = prob, fill = class)) +
    geom_boxplot(alpha = 0.6) + 
    geom_hline(data = df.prevalence, aes(yintercept = prevalence),
               colour = "lightgrey") +
    facet_wrap(~trueClass) + 
    labs(title = "Discrimination Plot by True Class", x = "Predicted Class",
         y = "Risk of Predicted Class") +
    theme_bw() + 
    theme(panel.grid = element_blank(), legend.position = "none")
  print(p)
}

#' Reliability plot: mean prediction vs. observed fraction on lowess smoother 
#' for each class
#' @noRd
reliability_plot <- function(x, pred.probs) {
  
  # cut each class into probability bins of 10, fit lowess
  df <- levels(x) %>% 
    purrr::set_names() %>% 
    purrr::map(~ {
      prob <- pred.probs[, .x]
      cl <- ifelse(x == .x, 1, 0)
      bin.pred <- cut(prob, 10)
      purrr::map_df(levels(bin.pred), ~ {
        idx <- .x == bin.pred
        data.frame(V1 = mean(prob[idx]),
                   V2 = sum(cl[idx]) / length(cl[idx]))
      }) %>%
        dplyr::filter(!is.nan(V1)) %>% 
        with(., stats::lowess(V1, V2)) 
    }) %>% 
    purrr::map2(names(.), ~ c(.x, class = .y)) %>% 
    purrr::map(tibble::as.tibble) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(class = factor(class))
  
  # reliability plot
  cols <- grDevices::rainbow(dplyr::n_distinct(x))
  p <- ggplot(df, aes(x, y, group = class, colour = class)) +
    geom_line(lwd = 2) + 
    geom_abline(intercept = 0, slope = 1, color = "grey") +
    scale_color_manual(values = cols) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) + 
    labs(x = "Mean Prediction", y = "Observed Fraction",
         title = "Reliability Plot") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.box.margin = margin(5, 0, 0, 5))
  print(p)
}