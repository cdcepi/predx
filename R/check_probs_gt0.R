#' Check that all probabilities are positive
#'
#' @param probs vector of probabilities for a single forecast
#'
#' @return  TRUE or error message
#'
check_probs_gt0 <- function(probs) {
  if (all(probs >= 0)) TRUE
  else "there are negative probabilities"
}
