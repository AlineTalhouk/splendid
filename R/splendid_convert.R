#' Convert covariate data
#'
#' Converts all categorical predictors to dummy variables in the dataset.
#' Classification algorithms LDA and the MLR family have such a limitation.
#'
#' If all the variables in the original data are already continuous, nothing is
#' done. Otherwise, conversion is performed if \code{convert = TRUE}. An error
#' message is thrown if there are categorical variables and \code{convert =
#' FALSE}, indicating exactly which algorithms specified require data
#' conversion.
#'
#' @inheritParams splendid
#' @return A (potentially) transformed data frame.
#' @author Derek Chiu
#' @export
#' @examples
#' data(hgsc)
#'
#' # Nothing happens if data is all continuous
#' data_same <- splendid_convert(hgsc, algorithms = "lda", convert = TRUE)
#' identical(hgsc, data_same)
#'
#' # Dummy variables created if there are categorical variables
#' data_dummy <- splendid_convert(iris, algorithms = "lda", convert = TRUE)
#' head(data_dummy)
#'
#' # Some algorithms are robust to the covariate data structure
#' data_robust <- splendid_convert(iris, algorithms = "rf", convert = FALSE)
#' identical(iris, data_robust)
#'
#' # Other algorithms require conversion
#' \dontrun{
#' splendid_convert(iris, algorithms = "lda", convert = FALSE)
#' }
splendid_convert <- function(data, algorithms, convert = FALSE) {
  if (!all(purrr::map_lgl(data, is.numeric))) {
    if (convert) {
      data <- dummify(data)
    }
    else {
      alg.cont <- intersect(algorithms, ALG.CONT)
      if (length(alg.cont)) {
        stop("Algorithms ", paste(sQuote(alg.cont), collapse = ", "),
             " need all continuous predictors. Remove categorical predictors in the data or set `convert = TRUE` to use dummy variables.")
      }
    }
  }
  data
}

#' Convert all categorical variables in a dataset to dummy variables
#' @param data data frame
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
    purrr::imap(~ magrittr::set_colnames(.x,
                                         gsub("\\.x", .y, colnames(.x))))
  dummy_vars <- purrr::map(desmat, colnames) %>%
    purrr::keep(~ length(.) > 1) %>%
    purrr::flatten_chr()
  desmat %>%
    purrr::invoke(cbind, .) %>%
    as.data.frame() %>%
    magrittr::set_rownames(NULL) %>%
    magrittr::set_attr("dummy_vars", dummy_vars)
}
