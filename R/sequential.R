#' Extract evaluation metrics for sequential model
#'
#' The by-class F1-scores are extracted for each model and bootstrap replicate.
#'
#' @param x evaluation output from \code{evals} element of
#'   \code{splendid_model}.
sequential_eval <- function(x) {
  evals <- x %>%
    purrr::imap(~ .x %>% magrittr::set_colnames(
      paste(.y, colnames(.x), sep = "."))) %>%
    unname() %>%
    purrr::invoke(cbind, .) %>%
    dplyr::mutate(measure = rownames(.)) %>%
    dplyr::filter(grepl("f1\\.", measure)) %>%
    dplyr::mutate(measure = gsub("f1\\.", "", measure)) %>%
    dplyr::rename(class = measure) %>%
    tidyr::gather(score, value, 1:4) %>%
    tidyr::separate(score, c("model", "boot"), sep = "\\.") %>%
    dplyr::select(model, boot, class, value)
  evals
}


#' Prepare data for sequential model
#'
#' Processes the data output from combineResults.R into model.rank object
#' required sequential input
#'
#' @param x data frame with F1-score of each model on each class derived from
#'   \code{sequential_eval}
#' @param boxplot if \code{TRUE}, boxplots are shown.
#' @return data frame with top models for each class ranked. A figure is also
#'   returned showing boxplots of F1-score by class and algorithm.
sequential_rank <- function(x, boxplot = FALSE) {

  tidy_evals <- sequential_eval(x)

  if (boxplot) {
    p <- tidy_evals %>%
      ggplot(aes(y = value, x = class, fill = model)) +
      geom_boxplot(alpha = 0.6) +
      facet_wrap(~model) +
      theme_bw() +
      labs(y = "F1-score")
    print(p)
  }

  model_ranks <- tidy_evals %>%
    dplyr::group_by(class, model) %>%
    dplyr::summarise(accuracy = mean(value)) %>%
    dplyr::group_by(class) %>%
    dplyr::filter(accuracy == max(accuracy)) %>%
    dplyr::arrange(desc(accuracy)) %>%
    cbind(rank = seq_len(nrow(.)), .) %>%
    dplyr::select(-accuracy)
  model_ranks
}

#' Sequentially train top ranked algorithms on each class ordered by class
#' performance
#'
#' @inheritParams splendid_ensemble
#'
#' @return (list) list of fits over ranked sequence
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sm <- splendid_model(hgsc, class, n = 2, algorithms = c("xgboost", "slda"))
#' sequential_train(sm, hgsc, class)
sequential_train <- function(sm, data, class) {
  # initialize objects
  model.rank <- sequential_rank(sm$evals)
  nseq <- nrow(model.rank) - 1
  fits <- vector("list", nseq)
  class.df <- binarize(data.frame(class))
  class.df.temp <- class.df
  dat.temp <- data

  # sequentially train
  for (rank_i in seq_len(nseq)) {
    # fit ranked model sequentially for each class
    fits[[rank_i]] <- classification(
      data = dat.temp,
      class = class.df.temp[, as.character(model.rank[rank_i, "class"])],
      algs = as.character(model.rank[rank_i, "model"])
    )

    # drop class already fit and move to next binary fit
    dat.class.combine <- data.frame(class.df.temp, dat.temp) %>%
      dplyr::filter(class != as.character(model.rank[rank_i, "class"]))
    class.df.temp <- dat.class.combine[, seq_len(ncol(class.df.temp))]
    dat.temp <- dat.class.combine %>% select(-seq_len(ncol(class.df.temp)))
  }
  fits %>% purrr::set_names(model.rank$class[seq_len(nseq)])
}

#' Sequentially predict a given class using the sequentially trained fits
#'
#' @param fit list of fitted models from \code{sequential_train}
#' @inheritParams sequential_train
#'
#' @return list of two elements
#'    pred: (list) predicted sequential probabilities
#'    error: (list) confusion matrices for each class
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sm <- splendid_model(hgsc, class, n = 2, algorithms = c("xgboost", "slda"))
#' st <- sequential_train(sm, hgsc, class)
#' sp <- sequential_pred(st, sm, hgsc, class)
sequential_pred <- function(fit, sm, data, class) {

  # initialize objects
  model.rank <- sequential_rank(sm$evals)
  nseq <- nrow(model.rank) - 1
  preds <- confmat <- vector("list", nseq)
  dat.temp <- data
  class.temp <- binarize(data.frame(class))

  # sequential prediction
  for (rank_i in seq_len(nseq)) {
    # predict classes sequentially according to rank
    prob <- prediction(fit[[rank_i]], dat.temp,
                       test.id = seq_len(nrow(dat.temp)),
                       class = class.temp$class) %@% "prob"
    if (is.null(colnames(prob))) colnames(prob) <- c("0", names(fit)[rank_i])
    preds[[rank_i]] <- prob
    pred <- apply(preds[[rank_i]], 1, function(x) names(x)[which.max(x)])

    # confustion matrix for class prediction
    confmat[[rank_i]] <- conf_mat(class.temp[, as.character(model.rank[rank_i, "class"])], pred)
    attr(confmat[[rank_i]], "error") <- class_error(confmat[[rank_i]])

    # drop classes predicted for next sequential step
    dat.class.combine <- data.frame(class.temp, pred = pred, dat.temp) %>%
      dplyr::filter(pred != as.character(model.rank[rank_i, "class"]))
    class.temp <- dat.class.combine[, seq_len(ncol(class.temp))]
    dat.temp <- dat.class.combine %>% select(-c(seq_len(ncol(class.temp)), pred))
  }
  list(pred = preds, error = confmat)
}
