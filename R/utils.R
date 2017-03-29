# magrittr placeholder
globalVariables(".")

# classification algorithms


#' Redirect any console printouts from print() or cat() to null device
#' @noRd
sink_output <- function(expr) {
  sink("/dev/null")
  mod <- eval(expr)
  sink()
  return(mod)
}