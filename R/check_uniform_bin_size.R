#' Check that all bins have the same width.
#'
#' @param bins
#'
#' @return TRUE or error message
#'
check_uniform_bin_size <- function(bins) {
  if (diff(range(diff(bins))) < .Machine$double.eps^0.5) TRUE
  else "the bin widths are not all equal or are not ordered"
}
