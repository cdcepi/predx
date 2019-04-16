#' Check that all probabilities are positive
#'
#' @param probs vector of probabilities for a single forecast
#'
#' @return TRUE or error message
#'
check_single_value <- function(x) {
  if (length(x) == 1) TRUE
  else "A vector was entered rather than a single value"
}
