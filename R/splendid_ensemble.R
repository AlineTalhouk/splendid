#' Combine classification models into an ensemble
#'
#' @inheritParams splendid
#' @param sm a \code{splendid_model} object
#' @export
#' @examples
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sm <- splendid_model(hgsc, class, n = 3, algorithms = c("xgboost", "lda"))
#' se <- splendid_ensemble(sm, hgsc, class)
splendid_ensemble <- function(sm, data, class, top = 3, seed = 1, rfe = FALSE,
                              sequential = FALSE) {
  bests <- sm$evals %>%
    do.call(cbind, .) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(logloss = -logloss,
                  boot = gsub(".*\\.", "\\1", rownames(.))) %>%
    split(.$boot) %>%
    purrr::map(~ dplyr::select(.x, -dplyr::matches("\\.|boot"))) %>%
    purrr::map(~ purrr::map(.x, ~ names(sm$models)[order(
      rank(-.x, ties.method = "random"))])) %>%
    purrr::map(~ purrr::invoke(rbind, .x)) %>%
    purrr::map_chr(~ {
      if (ncol(.x) > 1) {
        RankAggreg::RankAggreg(.x, ncol(.x), method = "GA",
                               seed = seed, verbose = FALSE) %>%
          magrittr::use_series("top.list") %>%
          utils::head(1)
      } else {
        .x[1]
      }
    })
  ensemble_algs <- bests %>%
    table() %>%
    sort() %>%
    rev() %>%
    head(top) %>%
    names()
  ensemble_mods <- ensemble_algs %>%
    purrr::map(classification, data = data, class = class, rfe = rfe)
  if (sequential) {
    seq_mods <- sequential_train(sm, data, class)
    seq_preds <- sequential_pred(seq_mods, sm, data, class)
  } else {
    seq_mods <- seq_preds <- NULL
  }
  dplyr::lst(bests, ensemble_algs, ensemble_mods, seq_mods, seq_preds)
}
