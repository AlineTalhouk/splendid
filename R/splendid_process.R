#' Process data
#'
#' Process the data by converting categorical predictors to dummy variables,
#' standardizing continuous predictors, and apply subsampling techniques.
#'
#' If all the variables in the original data are already continuous, nothing is
#' done. Otherwise, conversion is performed if `convert = TRUE` using
#' [dummify()]. An error message is thrown if there are categorical variables
#' and `convert = FALSE`, indicating exactly which algorithms specified require
#' data conversion. Classification algorithms LDA and the MLR family have such a
#' limitation.
#'
#' Continuous predictors can be scaled to have zero mean and unit variance with
#' `standardize = TRUE`. Dummy variables coded to 0 or 1 are never standardized.
#'
#' Subsampling techniques can be applied with `sampling` methods passed to
#' [subsample()].
#'
#' @inheritParams splendid
#' @return A pre-processed data frame for model training
#' @author Derek Chiu
#' @seealso [dummify()], [subsample()]
#' @export
#' @examples
#' data(hgsc)
#' cl <- attr(hgsc, "class.true")
#'
#' # Nothing happens if data is all continuous
#' data_same <- splendid_process(hgsc, class = cl, algorithms = "lda", convert =
#' TRUE)
#' identical(hgsc, data_same)
#'
#' # Dummy variables created if there are categorical variables
#' data_dummy <- splendid_process(iris, class = iris$Species, algorithms =
#' "lda", convert = TRUE)
#' head(data_dummy)
#'
#' # Some algorithms are robust to the covariate data structure
#' data_robust <- splendid_process(iris, class = iris$Species, algorithms =
#' "rf", convert = FALSE)
#' identical(iris, data_robust)
#'
#' # Standardize and down-sample
#' iris2 <- iris[1:130, ]
#' data_scale_down <- splendid_process(iris2, class = iris2$Species, algorithms
#' = "rf", standardize = TRUE, sampling = "down")
#' dim(data_scale_down)
#'
#' # Other algorithms require conversion
#' \dontrun{
#' splendid_process(iris, class = iris$Species, algorithms = "lda", convert =
#' FALSE)
#' }
splendid_process <- function(data, class, algorithms, convert = FALSE,
                             standardize = FALSE,
                             sampling = c("none", "up", "down", "smote"),
                             seed_samp = NULL) {
  if (!all(purrr::map_lgl(data, is.numeric))) {
    if (convert) {
      data <- dummify(data)
      if (standardize) {
        data <- data %>%
          dplyr::mutate_at(dplyr::vars(-c(attr(., "dummy_vars"))), scale)
      }
    }
    else {
      alg.cont <- intersect(algorithms, ALG.CONT)
      if (length(alg.cont)) {
        stop("Algorithms ", paste(sQuote(alg.cont), collapse = ", "),
             " need all continuous predictors.
             Remove categorical predictors or set `convert = TRUE` to use dummy variables.")
      }
    }
  } else {
    if (standardize) {
      data <- dplyr::mutate_all(data, scale)
    }
  }
  data_processed <- subsample(data, class, sampling, seed_samp)
  data_processed
}

#' Create dummy variables
#'
#' Convert all categorical variables in a dataset to dummy variables
#'
#' @inheritParams splendid
#' @return A numeric data frame where dummy variables encode categorical
#'   variables using multiple columns. Continuous variables are unchanged.
#' @author Derek Chiu
#' @export
#' @examples
#' dummify(mtcars)
#' dummify(iris)
dummify <- function(data) {
  desmat <- data %>%
    purrr::map(~ stats::model.matrix(~ .x - 1)) %>%
    purrr::imap(~ magrittr::set_colnames(.x, gsub("\\.x", .y, colnames(.x))))
  dummy_vars <- purrr::map(desmat, colnames) %>%
    purrr::keep(~ length(.) > 1) %>%
    purrr::flatten_chr()
  desmat %>%
    purrr::invoke(cbind, .) %>%
    as.data.frame() %>%
    magrittr::set_rownames(NULL) %>%
    `attr<-`("dummy_vars", dummy_vars)
}

#' Subsampling Imbalanced Data
#'
#' Subsampling imbalanced data using up-sampling, down-sampling, or SMOTE.
#'
#' To deal with class imbalances, we can subsample the data so that the class
#' proportions are more uniform.
#'
#' @inheritParams splendid
#' @return A subsampled dataset where corresponding strata of `class` are more
#'   balanced. The resulting `class` variable is not included in the data
#'   output.
#' @author Derek Chiu
#' @export
#' @examples
#' # Create imbalanced version of iris dataset
#' iris_imbal <- iris[1:130, ]
#'
#' # Up-sampling
#' iris_up <- subsample(iris_imbal, iris_imbal$Species, sampling = "up")
#' nrow(iris_up)
#'
#' # Down-sampling
#' iris_down <- subsample(iris_imbal, iris_imbal$Species, sampling = "down")
#' nrow(iris_down)
#'
#' # SMOTE
#' iris_smote <- subsample(iris_imbal, iris_imbal$Species, sampling = "smote")
#' nrow(iris_smote)
subsample <- function(data, class,
                      sampling = c("none", "up", "down", "smote"),
                      seed_samp = NULL) {
  sampling <- match.arg(sampling)
  class <- as.factor(class)
  if (sampling == "none") {
    return(data)
  } else {
    if (!is.null(seed_samp)) set.seed(seed_samp)
    switch(
      sampling,
      up = caret::upSample(data, class, yname = "class"),
      down = caret::downSample(data, class, yname = "class"),
      smote = DMwR::SMOTE(class ~ ., cbind(data, class))
    ) %>%
      dplyr::select(-.data[["class"]])
  }
}
