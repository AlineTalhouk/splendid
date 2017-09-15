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
