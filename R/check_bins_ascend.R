#' Title
#'
#' @param bins
#'
#' @return  TRUE or error message
#'
check_bins_ascend <- function(bins) {
  if (all(diff(bins) > 0)) TRUE
  else "the bins are not in ascending order"
}
