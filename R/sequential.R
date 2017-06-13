#' Sequential Algorithm
#'
#' Sequentially train top ranked algorithms on each class ordered by class
#' performance and predict a given class using the sequentially trained fits.
#'
#' \code{sequential_train} sequentially trains One-Vs-All models until all
#' classes have been classified. Hence for \code{n} classes, there are \code{n -
#' 1} sequential fits. \code{sequential_pred} predicts class membership for each
#' One-Vs-All sequential model. Performance is evaluated on by-class F1-scores,
#' since these are better for evaluation than other metrics such as accuracy,
#' precision, and recall.
#'
#' @inheritParams splendid_ensemble
#' @param fit list of fitted models from \code{sequential_train}
#'
#' @return \code{sequential_train} returns a list of fits over the top-ranked
#'   sequence.
#' @return \code{sequential_pred} returns a list of two elements
#' \item{pred}{predicted sequential probabilities}
#' \item{error}{confusion matrices for each class}
#' @name sequential
#' @author Dustin Johnson, Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sm <- splendid_model(hgsc, class, n = 2, algorithms = c("xgboost", "slda"))
#' st <- sequential_train(sm, hgsc, class)
#' sp <- sequential_pred(st, sm, hgsc, class)
sequential_train <- function(sm, data, class) {
  # initialize objects
  class.bin <- binarize(class)
  model.rank <- sequential_rank(sm$evals)
  nseq <- nrow(model.rank) - 1
  fits <- vector("list", nseq) %>%
    purrr::set_names(model.rank$class[seq_len(nseq)])

  # sequentially train
  for (rank_i in seq_len(nseq)) {
    # class and alg to fit ova model on
    cl <- as.character(model.rank[rank_i, "class"])
    alg <- as.character(model.rank[rank_i, "model"])

    # fit ranked model sequentially for each class
    fits[[rank_i]] <- classification(
      data = data,
      class = class.bin[, cl],
      algs = alg
    )

    # drop class already fit and move to next binary fit
    cl.rm <- class != cl
    data <- data[cl.rm, ]
    class.bin <- class.bin[cl.rm, ]
    class <- class[cl.rm]
  }
  fits
}

#' @name sequential
#' @export
sequential_pred <- function(fit, sm, data, class) {
  # initialize objects
  class.bin <- binarize(class)
  model.rank <- sequential_rank(sm$evals)
  nseq <- nrow(model.rank) - 1
  preds <- cm <- vector("list", nseq) %>%
    purrr::set_names(model.rank$class[seq_len(nseq)])

  # sequential prediction
  for (rank_i in seq_len(nseq)) {
    # predict classes sequentially according to rank
    prob <- prediction(mod = fit[[rank_i]], data = data,
                       test.id = seq_len(nrow(data)),
                       class = class) %@% "prob"
    if (is.null(colnames(prob))) colnames(prob) <- c("0", names(fit)[rank_i])
    preds[[rank_i]] <- prob
    pred <- apply(preds[[rank_i]], 1, function(x) names(x)[which.max(x)])

    # confustion matrix for class prediction and class error as attribute
    cl <- as.character(model.rank[rank_i, "class"])
    cm[[rank_i]] <- conf_mat(class.bin[, cl], pred)
    attr(cm[[rank_i]], "error") <- class_error(cm[[rank_i]])

    # drop classes predicted for next sequential step
    cl.rm <- pred != cl
    data <- data[cl.rm, ]
    class.bin <- class.bin[cl.rm, ]
    class <- class[cl.rm]
  }
  dplyr::lst(preds, cm)
}

#' Rank top models for each sequentially fitted class based on maximum average
#' F1-score across bootstrap replicates.
#' @inheritParams sequential_ensemble
#' @param boxplot if \code{TRUE}, boxplots are shown.
#' @noRd
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

#' Extract by-class F1-scores for each model and bootstrap replicate in tidy
#' format.
#' @inheritParams splendid_ensemble
#' @noRd
sequential_eval <- function(sm) {
  evals <- sm %>%
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
