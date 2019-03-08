#' Check that probabilities sum to 1 (or approximately 1)
#'
#' @param probs vector of probabilities for a single forecast
#' @param tolerance tolerance relative to 1, default = .Machine$double.eps^0.5
#'
#' @return  TRUE or error message
#'
check_probs_sum_to_one <- function(probs, msg=T, tolerance=.Machine$double.eps^0.5) {
  if (abs(sum(probs) - 1) < tolerance) TRUE
  else if (msg) "the probabilities do not sum to 1.0"
  else FALSE
}

