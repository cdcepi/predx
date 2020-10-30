#' Check that all probabilities are positive
#'
#' @param probs vector of probabilities for a single forecast
#'
#' @return  TRUE or error message
#'
check_gte0 <- function(x, colname) {
  if (all(x >= 0)) TRUE
  else paste0("there are ", colname, " less than zero")
}
