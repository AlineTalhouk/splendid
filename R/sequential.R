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
#' \item{preds}{predicted sequential probabilities}
#' \item{cm}{confusion matrices for each class}
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
  # prepare data
  model_rank <- sequential_rank(sm[["evals"]])
  class_bin <- sequential_binarize(model_rank, class)

  # storage vector
  fits <- purrr::list_along(class_bin) %>%
    purrr::set_names(head(model_rank[["class"]], -1))

  # sequentially train
  for (rank_i in seq_along(class_bin)) {
    # class and alg to fit ova model on
    cl <- model_rank[["class"]][rank_i]
    alg <- model_rank[["model"]][rank_i]

    # fit ranked model sequentially for each class
    fits[[rank_i]] <- classification(
      data = data,
      class = class_bin[, cl],
      algs = alg
    )

    # drop class already fit and move to next binary fit
    cl.keep <- class != cl
    data <- data[cl.keep, ]
    class_bin <- class_bin[cl.keep, ]
    class <- class[cl.keep]
  }
  fits
}

#' @name sequential
#' @export
sequential_pred <- function(fit, sm, data, class) {
  # prepare data
  model_rank <- sequential_rank(sm[["evals"]])
  class_bin <- sequential_binarize(model_rank, class)

  # storage vectors
  preds <- cm <- purrr::list_along(class_bin) %>%
    purrr::set_names(names(fit))

  # sequential prediction
  for (rank_i in seq_along(class_bin)) {
    # predict classes sequentially according to rank
    prob <- prediction(mod = fit[[rank_i]], data = data,
                       test.id = seq_len(nrow(data)), class = class) %@% "prob"
    if (is.null(colnames(prob))) colnames(prob) <- c("0", names(fit)[rank_i])
    preds[[rank_i]] <- prob
    pred <- apply(prob, 1, function(x) names(x)[which.max(x)])

    # confustion matrix for class prediction and class error as attribute
    cl <- model_rank[["class"]][rank_i]
    cm[[rank_i]] <- conf_mat(class_bin[, cl], pred)
    attr(cm[[rank_i]], "error") <- class_error(cm[[rank_i]])

    # drop classes predicted for next sequential step
    cl.keep <- pred != cl
    data <- data[cl.keep, ]
    class_bin <- class_bin[cl.keep, ]
    class <- class[cl.keep]
  }
  dplyr::lst(preds, cm)
}

#' Rank top models for each sequentially fitted class based on maximum average
#' F1-score across bootstrap replicates.
#' @inheritParams sequential_ensemble
#' @param boxplot if \code{TRUE}, boxplots are shown.
#' @noRd
sequential_rank <- function(sm, boxplot = FALSE) {
  tidy_evals <- sequential_eval(sm)
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
    tidyr::gather(score, value, -class) %>%
    tidyr::separate(score, c("model", "boot"), sep = "\\.") %>%
    dplyr::select(model, boot, class, value)
  evals
}

#' Prepare binarized classes from ranked models for sequential algorithm
#' @param mr output from \code{sequential_rank}
#' @noRd
sequential_binarize <- function(mr, class) {
  # Binarize classes and combine the last two ranked classes
  last_two <- tail(mr[["class"]], 2)
  class_bin <- class %>%
    binarize() %>%
    tidyr::unite_(col = last_two[1], from = last_two) %>%
    dplyr::mutate_at(.vars = last_two[1], .funs = funs(gsub("_0|0_", "", .)))
  class_bin
}
