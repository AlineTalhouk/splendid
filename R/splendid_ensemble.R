#' Combine classification models into an ensemble
#'
#' @inheritParams splendid
#' @param sm a `splendid_model` object
#' @export
#' @examples
#' dat <- iris[, 1:4]
#' class <- iris$Species
#' sm <- splendid_model(dat, class, n = 3, algorithms = c("xgboost", "slda"))
#' se <- splendid_ensemble(sm, dat, class)
splendid_ensemble <- function(sm, data, class, top = 3, seed_rank = 1,
                              rfe = FALSE, sequential = FALSE) {
  # vector of best performing algorithms from each bootstrap replicate
  bests <- sm$evals %>%
    do.call(cbind, .) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(logloss = -logloss,
                  boot = gsub(".*\\.", "\\1", rownames(.))) %>%
    split(.$boot) %>%
    purrr::map(dplyr::select, -dplyr::matches("\\.|boot")) %>%
    purrr::map(~ purrr::map(., ~ names(sm$models)[order(
      rank(-., ties.method = "random"))])) %>%
    purrr::map(~ rlang::exec(rbind, !!!.)) %>%
    purrr::map_chr(~ {
      if (ncol(.) > 1) {
        RankAggreg::RankAggreg(., ncol(.), method = "GA", seed = seed_rank,
                               verbose = FALSE)$top.list[1]
      } else {
        .[1]
      }
    })

  # Distinct number of top algorithms and models on full data
  ensemble_algs <- bests %>%
    table() %>%
    sort() %>%
    rev() %>%
    utils::head(top) %>%
    names()
  ensemble_mods <- ensemble_algs %>%
    purrr::map(classification, data = data, class = class, rfe = rfe)

  # Conditionally evaluate sequential model and prediction on full data
  if (sequential) {
    seq_mods <- sequential_train(sm, data, class)
    seq_preds <- sequential_pred(seq_mods, sm, data, class)
  } else {
    seq_mods <- seq_preds <- NULL
  }

  tibble::lst(bests, ensemble_algs, ensemble_mods, seq_mods, seq_preds)
}
