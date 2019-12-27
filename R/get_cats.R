#' Get vector of categories for a BinCat or SampleCat class predx object
#'
#' @param x an object of class BinCat or SampleCat OR a list of those objects
#'
#' @return character vector of categories
#'
#' @export
get_cats <- function(x) {
  if (is.list(x)) {
    out <- lapply(x, function(x) x@predx$cat)
  } else if (!(is.BinCat(x) || is.SampleCat(x))) {
    stop('Function for BinCat and SampleCat objects only')
  } else out <- x@predx$cat
  return(out)
}
