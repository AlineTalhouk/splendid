#' Sequential Algorithm
#'
#' Sequentially train top ranked algorithms on each class ordered by class
#' performance and predict a given class using the sequentially trained fits.
#'
#' `sequential_train` sequentially trains One-Vs-All models until all classes
#' have been classified. Hence for `n` classes, there are `n - 1` sequential
#' fits. `sequential_pred` predicts class membership for each One-Vs-All
#' sequential model. Performance is evaluated on by-class F1-scores, since these
#' are better for evaluation than other metrics such as accuracy, precision, and
#' recall.
#'
#' @inheritParams splendid_ensemble
#' @param fit list of fitted models from `sequential_train`
#' @param boxplot if `TRUE`, boxplots are plotted showing the distribution of
#'   F1-scores per class, for every algorithm.
#'
#' @return `sequential_train` returns a list of fits over the top-ranked
#'   sequence.
#' @return `sequential_pred` returns a list of two elements
#' * `prob`: predicted sequential probabilities
#' * `cm`: confusion matrices for each class
#' @name sequential
#' @author Dustin Johnson, Derek Chiu
#' @export
#' @examples
#' dat <- iris[, 1:4]
#' class <- iris$Species
#' sm <- splendid_model(dat, class, n = 2, algorithms = c("slda", "xgboost"))
#' st <- sequential_train(sm, dat, class)
#' sp <- sequential_pred(st, sm, dat, class)
sequential_train <- function(sm, data, class, boxplot = FALSE) {
  # prepare data and setup storage object(s)
  model_rank <- sequential_rank(sm[["evals"]], boxplot = boxplot)
  class_bin <- sequential_binarize(model_rank, class)
  fits <- class_bin %>%
    purrr::list_along() %>%
    purrr::set_names(head(model_rank[["class"]], -1))

  # sequentially train
  for (rank_i in seq_along(class_bin)) {
    # class and alg to fit ova model on
    cl <- model_rank[["class"]][rank_i]
    algorithms <- model_rank[["model"]][rank_i]
    ova <- if (algorithms == "knn") TRUE else FALSE  # ova is always on for knn

    # fit ranked model sequentially for each class
    fits[[rank_i]] <- class_bin[[cl]] %>%
      classification(data = data, algorithms = algorithms, ova = ova)

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
sequential_pred <- function(fit, sm, data, class, boxplot = FALSE) {
  # prepare data and setup storage object(s)
  model_rank <- sequential_rank(sm[["evals"]], boxplot = boxplot)
  class_bin <- sequential_binarize(model_rank, class)
  prob <- cm <- class_bin %>%
    purrr::list_along() %>%
    purrr::set_names(names(fit))

  # sequential prediction
  for (rank_i in seq_along(class_bin)) {
    # predict classes sequentially according to rank
    prob[[rank_i]] <- fit[[rank_i]] %>%
      prediction(data = data, test.id = seq_len(nrow(data)), class = class,
                 train.id = seq_len(nrow(data))) %>%
      `%@%`("prob") %>%
      purrr::when(
        is.null(colnames(.)) ~
          magrittr::set_colnames(., c("0", names(fit)[rank_i])),
        ~ .
      )
    pred <- apply(prob[[rank_i]], 1, function(x) names(x)[which.max(x)])

    # confustion matrix for class prediction and class error as attribute
    cl <- model_rank[["class"]][rank_i]
    cm[[rank_i]] <- conf_mat(class_bin[[cl]], pred) %>%
      structure(error = class_error(.))

    # drop classes predicted for next sequential step
    cl.keep <- class != cl
    data <- data[cl.keep, ]
    class_bin <- class_bin[cl.keep, ]
    class <- class[cl.keep]
  }
  tibble::lst(prob, cm)
}

#' Rank top models for each sequentially fitted class based on maximum average
#' F1-score across bootstrap replicates.
#' @inheritParams sequential_ensemble
#' @noRd
sequential_rank <- function(sm, boxplot) {
  tidy_evals <- sequential_eval(sm)
  if (boxplot) {
    p <- tidy_evals %>%
      ggplot(aes_(y = ~value, x = ~class, fill = ~model)) +
      geom_boxplot(alpha = 0.6) +
      facet_wrap(~model) +
      theme_bw() +
      labs(y = "F1-score")
    print(p)
  }
  model_ranks <- tidy_evals %>%
    dplyr::group_by(.data$class, .data$model) %>%
    dplyr::summarise(!!"metric" := mean(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$class) %>%
    dplyr::summarize(!!"model" := .data$model[which.max(.data$metric)],
                     !!"metric" := max(.data$metric)) %>%
    dplyr::arrange(desc(.data$metric)) %>%
    cbind(rank = seq_len(nrow(.)), .) %>%
    dplyr::select(-"metric")
  model_ranks
}

#' Extract by-class F1-scores for each model and bootstrap replicate in tidy
#' format.
#' @inheritParams splendid_ensemble
#' @noRd
sequential_eval <- function(sm) {
  evals <- sm %>%
    purrr::invoke(cbind, .) %>%
    tibble::rownames_to_column("class") %>%
    dplyr::filter(grepl("f1\\.", .data$class)) %>%
    dplyr::mutate(!!"class" := gsub("f1\\.", "", .data$class)) %>%
    tidyr::gather(!!"score", !!"value", -!!"class") %>%
    tidyr::separate(!!"score", c("model", "boot"), sep = "\\.") %>%
    dplyr::select("model", "boot", "class", "value")
  evals
}

#' Prepare binarized classes from ranked models for sequential algorithm
#' @param mr model_rank output from `sequential_rank`
#' @noRd
sequential_binarize <- function(mr, class) {
  # Binarize classes and combine the last two ranked classes
  last_two <- tail(mr[["class"]], 2)
  class_bin <- class %>%
    binarize() %>%
    tidyr::unite_(last_two[1], last_two) %>%
    dplyr::mutate_at(last_two[1], funs(gsub("_class_0|class_0_", "", .)))
  class_bin
}
