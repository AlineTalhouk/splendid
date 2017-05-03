#' @export
#' @examples 
#' data(hgsc)
#' class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]
#' sm <- splendid_model(hgsc, class, n = 2, algorithms = c("svm", "lda"))
#' se <- splendid_ensemble(sm, hgsc, class)
splendid_ensemble <- function(sm, data, class, top = 3, seed = 1) {
  bests <- sm$evals %>% 
    split(.$boot) %>% 
    purrr::map(`[`, 1:7) %>% 
    purrr::map(~ .x %>%
                 as.data.frame() %>%
                 magrittr::set_rownames(.$name) %>%
                 dplyr::select(-name)) %>% 
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
        colnames(.x)
      }
    })
  ensemble_algs <- bests %>% 
    table() %>% 
    sort() %>% 
    rev() %>% 
    head(top) %>% 
    names()
  ensemble_mods <- purrr::map(ensemble_algs, ~ classification(data, class, .x))
  dplyr::lst(bests, ensemble_algs, ensemble_mods)
}