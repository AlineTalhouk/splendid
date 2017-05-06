#' Combine classification models into an ensemble
#' 
#' @export
#' @examples 
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sm <- splendid_model(hgsc, class, n = 2, algorithms = c("svm", "lda"))
#' se <- splendid_ensemble(sm, hgsc, class)
splendid_ensemble <- function(sm, data, class, top = 3, seed = 1, rfe = FALSE) {
  bests <- sm$evals %>% 
    do.call(cbind, .) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(boot = gsub(".*\\.", "\\1", rownames(.))) %>%
    split(.$boot) %>% 
    purrr::map(`[`, 1:6) %>% 
    purrr::map(~ purrr::map(.x, ~ names(sm$models)[order(
      rank(-.x, ties.method = "random"))])) %>% 
    purrr::map(~ purrr::invoke(rbind, .x)) %>% 
    purrr::map_chr(~ {
      if (ncol(.x) > 1) {
        RankAggreg::RankAggreg(.x, ncol(.x), method = "GA",
                               seed = seed, verbose = FALSE) %>% 
          magrittr::use_series("top.list") %>% 
          head(1)
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
  ensemble_mods <- purrr::map(ensemble_algs, ~ classification(data, class, .x,
                                                              rfe = rfe))
  dplyr::lst(bests, ensemble_algs, ensemble_mods)
}