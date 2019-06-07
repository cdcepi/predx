#' Get vector of categories for a BinCat or SampleCat class predx object
#'
#' @param x an object of class BinCat or SampleCat
#'
#' @return character vector of categories
#'
#' @export
get_cats <- function(x) {
  if (!(is.BinCat(x) || is.SampleCat(x))) stop('function for BinCat and SampleCat objects only')
  x@predx$cat
}
