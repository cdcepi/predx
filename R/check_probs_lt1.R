#' Check that all probabilities are positive
#'
#' @param probs vector of probabilities for a single forecast
#'
#' @return  TRUE or error message
#'
check_probs_lt1 <- function(probs) {
  if (all(probs <= 1)) TRUE
  else "there are probabilities greater than 1.0"
}
