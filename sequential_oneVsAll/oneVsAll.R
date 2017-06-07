oneVsAll_train <- function(dat, class)
###########################################################
# Train One-vs-All approach
# Inputs:
#   :dat:   (data.frame) predictors on which to train the model
#   :class: (numeric) true training classes
# Outputs:
#   :fits: (list) fit binary classifier on each class
###########################################################
{
  class.df <- binarize__(data.frame(class)) %>% dplyr::select(-class)
  fits <- purrr::map(class.df, ~splendid::classification(data = dat %>% as.matrix, class = .x, algs = NULL))
  return(fits)
}

oneVsAll_pred <- function(fit, dat, class.true)
##################################################################
# Predict using the One-vs-All approach
# Inputs:
#   :fit:        (list) output from the oneVsAll_train function
#   :dat:        (data.frame) predictors on which to train the model
#   :class.true: (numeric) vector of true training classes
# Outputs:
#   :pred.probs: (tibble) predicted probabilities for each class
##################################################################
{
  pred.probs <- purrr::map(fit, ~splendid::prediction(.x, dat, probability = TRUE, class = class.true) %@% "prob") %>%
    purrrlyr::dmap(~.x[,2]) %>% 
    data.table::setattr(., "class.true", class.true) %>%
    data.table::setattr(., "class.pred", apply(., 1, which.max))
  return(pred.probs)
}


