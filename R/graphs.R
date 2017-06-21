#' Discriminating graphs
#'
#' Graphs a \code{discrimination_plot} and \code{reliability_plot} based on true
#' classes and predicted class probabilities.
#'
#' A \code{discrimination_plot} shows boxplots of the predicted probabilities
#' for each class, separated by panels of the true class labels. The class
#' prevalence is also drawn as a horizontal line for each panel.
#'
#' A \code{reliability_plot} shows mean prediction vs. observed fraction on
#' lowess smoother for each class. A line going thru the origin with slope of 1
#' serves as a reference for perfect reliability.
#'
#' Both plots can be called from within \code{\link{evaluation}}.
#'
#' @param x true class labels
#' @param pred.probs matrix of predicted class probabilities. Number of rows
#'   must equal length of \code{x}
#' @return \code{ggplot} objects for the desired plot
#' @name splendid_graphs
#' @author Dustin Johnson, Derek Chiu
#' @references http://onlinelibrary.wiley.com/doi/10.1002/sim.5321/abstract
#' @export
#' @examples
#' data(hgsc)
#' class <- factor(attr(hgsc, "class.true"))
#' set.seed(1)
#' training.id <- sample(seq_along(class), replace = TRUE)
#' test.id <- which(!seq_along(class) %in% training.id)
#' mod <- classification(hgsc[training.id, ], class[training.id], "xgboost")
#' pred <- prediction(mod, hgsc, test.id, class = class)
#' discrimination_plot(class[test.id], attr(pred, "prob"))
#' reliability_plot(class[test.id], attr(pred, "prob"))
discrimination_plot <- function(x, pred.probs) {

  # turn into long-form for plotting
  df.long <- data.frame(trueClass = x, pred.probs) %>%
    tidyr::gather(key = "class", value = "prob", -1, factor_key = TRUE)

  # create prevalance (base-line) class proportion table
  df.prevalence <- df.long %>%
    dplyr::group_by_("trueClass") %>%
    dplyr::summarise_("classCount" = ~ length(trueClass)) %>%
    dplyr::mutate_("totalCount" = ~ sum(classCount),
                   "prevalence" = ~ classCount / totalCount)

  # discrimination plot
  cols <- grDevices::rainbow(dplyr::n_distinct(x))
  p <- ggplot(df.long, aes_(x = ~class, y = ~prob, fill = ~class)) +
    geom_boxplot(alpha = 0.6) +
    geom_hline(data = df.prevalence, aes_(yintercept = ~prevalence),
               colour = "lightgrey") +
    scale_fill_manual(values = cols) +
    facet_wrap(~trueClass) +
    labs(title = "Discrimination Plot by True Class", x = "Predicted Class",
         y = "Risk of Predicted Class") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.position = "none")
  print(p)
}

#' @name splendid_graphs
#' @export
reliability_plot <- function(x, pred.probs) {

  # cut each class into probability bins of 10, fit lowess
  df <- levels(x) %>%
    purrr::set_names() %>%
    purrr::map(~ {
      prob <- pred.probs[, .x]
      cl <- ifelse(x == .x, 1, 0)
      bin.pred <- cut(prob, 10)
      purrr::map_df(levels(bin.pred), ~ {
        idx <- .x == bin.pred
        data.frame(V1 = mean(prob[idx]),
                   V2 = sum(cl[idx]) / length(cl[idx]))
      }) %>%
        dplyr::filter(!is.nan(V1)) %>%
        with(., stats::lowess(V1, V2))
    }) %>%
    purrr::map2(names(.), ~ c(.x, class = .y)) %>%
    purrr::map(tibble::as.tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(class = factor(class))

  # reliability plot
  cols <- grDevices::rainbow(dplyr::n_distinct(x))
  p <- ggplot(df, aes_(~x, ~y, group = ~class, colour = ~class)) +
    geom_line(lwd = 2) +
    geom_abline(intercept = 0, slope = 1, color = "grey") +
    scale_color_manual(values = cols) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    labs(x = "Mean Prediction", y = "Observed Fraction",
         title = "Reliability Plot") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.box.margin = margin(5, 0, 0, 5))
  print(p)
}
