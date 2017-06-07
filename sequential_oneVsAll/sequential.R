sequential_prepare_data <- function(dat)
##########################################################
# Processes the data output from combineResults.R
# into model.rank object required sequential input
# Inputs:
#   :dat: (data.frame) accuracy of each model on each class
#         derived from combineResults.R script for cluster
# Outputs:
#   :model.rank: (data.frame) top models for each class ranked
##########################################################
{
  p <- dat %>% 
    ggplot(aes(y = accuracy, x = class, fill = model)) +
    geom_boxplot(alpha = 0.6) + facet_wrap(~model) +
    theme_bw()
  
  model.rank <- dat %>%
    group_by(class, model) %>%
    summarise(accuracy = mean(accuracy)) %>%
    group_by(class) %>% 
    arrange(accuracy = desc(accuracy)) %>%
    mutate(best = max(accuracy)) %>%
    filter(accuracy == best) %>%
    arrange(accuracy = desc(accuracy)) %>%
    data.frame(rank = 1:dim(.)[1]) %>%
    select(rank, class, model) %>%
    ungroup()
  
  print(p)
  
  return(model.rank)
}

binarize__ <- function(x)
#####################################################
# Helper function to turn class vector into binary 
# one-vs-all matrix.
# Inputs:
#   :x: (data.frame) class labels with name class
# Outputs:
#   :class.df: (data.frame) binarized classes
#####################################################
{
  for(i in seq_along(unique(x$class)))
  {
    bin.temp <- ifelse(x$class == i, i, 0)
    x <- cbind(x, bin.temp)
  }
  names(x) <- c("class", paste("class", unique(x$class)))
  x.df <- data.frame(x)
  return(x.df)
}

sequential_train <- function(dat, class, model.rank)
#####################################################
# Sequentially train top ranked algorithms on each 
# class ordered by class performance
# Inputs:
#   :dat:   (data.frame) predictors on which to train the model
#   :class: (numeric) true training classes
#   :model.rank: (data.frame) output from sequential_prepare_data
# Outputs:
#   :fits: (list) list of fits over ranked sequence
#####################################################
{
  # initialize objects
  class.df <- binarize__(data.frame(class))
  class.df.temp <- class.df
  dat.temp <- dat
  fits <- list()

  # sequentially train
  for(rank_i in seq_along(model.rank$rank))
  {
    # fit ranked model sequentially for each class
    fits[[rank_i]] <- splendid::classification(dat.temp, class.df.temp[,as.integer(model.rank[rank_i,'class'])+1], 
                                               algs = as.character(model.rank[rank_i,'model']))
    
    # drop class already fit and move to next binary fit
    dat.class.combine <- data.frame(class.df.temp, dat.temp) %>%
      filter(class != as.integer(model.rank[rank_i,'class']))
    class.df.temp <- dat.class.combine[,1:ncol(class.df.temp)]
    dat.temp <- dat.class.combine %>% select(-c(1:ncol(class.df.temp)))
  }
  return(fits)
}

sequential_pred <- function(fit, dat, class.true, model.rank)
###########################################################
# Sequentially predict a given class using the sequentially
# trained fits
# Inputs:
#   :fit:   (list) output from sequential_train
#   :dat:   (data.frame) predictors on which to train the model
#   :class: (numeric) true training classes
#   :model.rank: (data.frame) ranking matrix that determines
#                 sequence of training from sequential_prepare_data
# Outputs (list):
#   :pred: (list) predicted sequential probabilities
#   :error: (list) confusion matrices for each class
###########################################################
{
  # helper functions
  confusionMat__ <- function(class.pred, class.true) as.matrix(table(class.true, class.pred))
  classError__ <- function(confmat) 1 - (sum(diag(confmat)) / sum(confmat))
  
  # initialization objects
  preds <- list()
  confmat <- list()
  dat.temp <- dat
  class.temp <- binarize__(data.frame(class.true))
  
  # sequential prediction
  for(rank_i in seq_along(model.rank$rank))
  {
    # predict classes sequentially according to rank
    preds[[rank_i]] <- splendid::prediction(fit[[rank_i]], dat.temp, probability = TRUE, 
                                            class = class.temp$class) %@% "prob"
    pred <- apply(preds[[rank_i]], 1, function(x) as.integer(names(x)[which.max(x)])) %>% as.integer
    
    # confustion matrix for class prediction
    confmat[[rank_i]] <- confusionMat__(class.temp[,as.integer(model.rank[rank_i,'class'])+1], pred)
    attr(confmat[[rank_i]], "error") <- classError__(confmat[[rank_i]])
    
    # drop classes predicted for next sequential step
    dat.class.combine <- data.frame(class.temp, pred = pred, dat.temp) %>%
      filter(pred != as.integer(model.rank[rank_i,'class']))
    class.temp <- dat.class.combine[,1:ncol(class.temp)]
    dat.temp <- dat.class.combine %>% select(-c(1:ncol(class.temp), pred))
  }
  
  return(list(pred = preds, error = confmat))
}

