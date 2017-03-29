#' Evaluate class prediction labels
#' 
#' @param x actual class labels
#' @param y predicted class labels
#' @references https://github.com/saidbleik/Evaluation/blob/master/eval.R
#' @author Derek Chiu
#' @export
evaluation <- function(x, y) {
  cm <- as.matrix(table(Actual = x, Predicted = y))
  n <- sum(cm) # number of instances
  nc <- nrow(cm) # number of classes
  diag <- diag(cm) # number of correctly classified instances per class 
  rowsums <- rowSums(cm) # number of instances per class
  colsums <- colSums(cm) # number of predictions per class
  p <- rowsums / n # distribution of instances over the actual classes
  q <- colsums / n # distribution of instances over the predicted classes
  
  # Accuracy, precision, recall, F1-score (harmonic mean of precision & recall)
  accuracy <- sum(diag) / n
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * precision * recall / (precision + recall)
  
  # Macro-averaged precision, recall, F1-score
  macro_p <- mean(precision)
  macro_r <- mean(recall)
  macro_f <- mean(f1)
  
  # Sum of One vs. All matrices
  s <- purrr::map(seq_len(nc), ~ {
    m <- cm[.x, .x]
    cs <- colsums[.x]
    rs <- rowsums[.x]
    matrix(c(m, cs - m, rs - m, n - cs - rs + m), nrow = 2)
  }) %>% 
    purrr::reduce(`+`)
  
  # Average accuracy
  avg_accuracy <- sum(diag(s)) / sum(s)
  
  return(list(accuracy = accuracy, avg_accuracy = avg_accuracy,
              avg_precision = macro_p, avg_recall = macro_r, avg_F1 = macro_f))
}