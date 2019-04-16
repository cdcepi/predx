#' Check that number of forecast bins matches number of probabilties.
#'
#' @param probs Probability vector.
#' @param bins Bin vector.
#'
#' @return TRUE or error message.
#'
check_probs_bins_length_match <- function(probs, bins) {
  if (length(probs) == length(bins)) TRUE
  else "the number of bins and the number of probabilities do not match"
}
