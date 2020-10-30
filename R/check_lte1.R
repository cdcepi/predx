#' Check that all probabilities are positive
#'
#' @param probs vector of probabilities for a single forecast
#'
#' @return  TRUE or error message
#'
check_lte1 <- function(x, colname) {
  if (all(x <= 1)) TRUE
  else paste0("there are ", colname, " greater than one")
}
