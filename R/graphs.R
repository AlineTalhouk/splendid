#' Discriminating graphs
#'
#' Graphs a `discrimination_plot`, `reliability_plot`, and `roc_plot` based on
#' true classes and predicted class probabilities.
#'
#' A `discrimination_plot` shows boxplots of the predicted probabilities for
#' each class, separated by panels of the true class labels. The class
#' prevalence is also drawn as a horizontal line for each panel.
#'
#' A `reliability_plot` shows mean prediction vs. observed fraction on lowess
#' smoother for each class. A line going thru the origin with slope of 1 serves
#' as a reference for perfect reliability.
#'
#' A `roc_plot` shows the multi-class ROC curves for each class using
#' 1-Specificity vs. Sensitivity.
#'
#' All plots can be called from within [evaluation()].
#'
#' @inheritParams evaluation
#' @param probs matrix of predicted class probabilities. Number of rows
#'   must equal length of `x`
#' @return `ggplot` objects for the desired plot
#' @name splendid_graphs
#' @author Dustin Johnson, Derek Chiu
#' @references http://onlinelibrary.wiley.com/doi/10.1002/sim.5321/abstract
#' @export
#' @examples
#' data(hgsc)
#' class <- attr(hgsc, "class.true")
#' set.seed(1)
#' training.id <- sample(seq_along(class), replace = TRUE)
#' test.id <- which(!seq_along(class) %in% training.id)
#' mod <- classification(hgsc[training.id, ], class[training.id], "xgboost")
#' pred <- prediction(mod, hgsc, test.id, class = class)
#' discrimination_plot(class[test.id], attr(pred, "prob"))
#' reliability_plot(class[test.id], attr(pred, "prob"))
#' roc_plot(class[test.id], attr(pred, "prob"))
discrimination_plot <- function(x, probs) {

  # turn into long-form for plotting
  df.long <- data.frame(true_class = factor(x), probs) %>%
    tidyr::gather(key = "class", value = "prob", -1, factor_key = TRUE)

  # create prevalance (base-line) class proportion table
  df.prevalence <- df.long %>%
    dplyr::group_by(.data$true_class) %>%
    dplyr::summarise(!!"class_count" := length(.data$true_class)) %>%
    dplyr::mutate(
      !!"total_count" := sum(.data$class_count),
      !!"prevalence" := .data$class_count / .data$total_count
    )

  # discrimination plot
  cols <- grDevices::rainbow(dplyr::n_distinct(x))
  p <- ggplot(df.long, aes(x = .data$class, y = .data$prob, fill = .data$class)) +
    geom_boxplot(alpha = 0.6) +
    geom_hline(data = df.prevalence, aes(yintercept = .data$prevalence),
               colour = "lightgrey") +
    scale_fill_manual(values = cols) +
    facet_wrap(~true_class) +
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
reliability_plot <- function(x, probs) {

  # cut each class into probability bins of 10, fit lowess
  df <- x %>%
    as.factor() %>%
    levels() %>%
    purrr::set_names() %>%
    purrr::map(~ {
      prob <- probs[, .]
      cl <- ifelse(x == ., 1, 0)
      bin.pred <- cut(prob, 10)
      purrr::map_df(levels(bin.pred), ~ {
        idx <- . == bin.pred
        data.frame(MP = mean(prob[idx]), OF = mean(cl[idx]))
      }) %>%
        dplyr::filter(!is.nan(MP)) %>%
        with(., stats::lowess(MP, OF))
    }) %>%
    purrr::imap_dfr(~ purrr::list_flatten(list(.x, class = rep(.y, 10)))) %>%
    dplyr::mutate(class = factor(class))

  # reliability plot
  cols <- grDevices::rainbow(dplyr::n_distinct(x))
  p <- ggplot(df, aes(.data$x, .data$y, group = .data$class, colour = .data$class)) +
    geom_line(lwd = 2) +
    geom_abline(intercept = 0, slope = 1, color = "grey") +
    scale_color_manual(values = cols) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    labs(x = "Mean Prediction",
         y = "Observed Fraction",
         title = "Reliability Plot") +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0, 1),
      legend.justification = c(0, 1),
      legend.box.margin = margin(5, 0, 0, 5)
    )
  print(p)
}

#' @name splendid_graphs
#' @export
roc_plot <- function(x, probs) {

  # rename probability matrix
  roc_df <- stats::model.matrix(~ x - 1) %>%
    as.data.frame() %>%
    dplyr::mutate_all(as.integer) %>%
    dplyr::rename_all(~ gsub("x(.*)$", "\\1_true", .)) %>%
    cbind(probs) %>%
    dplyr::rename_if(is.double, ~ paste0(., "_pred_model"))

  # plotting format
  plot_roc_df <- roc_df %>%
    multiROC::multi_roc(force_diag = TRUE) %>%
    suppressWarnings() %>%
    multiROC::plot_roc_data() %>%
    dplyr::mutate(!!"Group" := forcats::fct_relevel(.data$Group, "Macro", "Micro", after = Inf))

  # multi-class ROC curves
  cols <- c(grDevices::rainbow(dplyr::n_distinct(x)), c("grey60", "grey80"))
  p <- ggplot(plot_roc_df, aes(1 - .data$Specificity, .data$Sensitivity)) +
    geom_path(aes(color = .data$Group), linewidth = 1) +
    annotate(
      geom = "segment",
      x = 0,
      y = 0,
      xend = 1,
      yend = 1,
      colour = "grey",
      linetype = "dotdash"
    ) +
    scale_color_manual(values = cols) +
    labs(title = "Multi-Class ROC Curves") +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.justification = c(1, 0),
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.05),
      legend.title = element_blank(),
      legend.background = element_rect(
        linewidth = 0.5,
        linetype = "solid",
        colour = "black"
      )
    )
  print(p)
}
