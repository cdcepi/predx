#' Helper functions for working with lists of predx objects (e.g. a predx column containing a variety of predx classes)
#'
#' `to_predx` returns a list of predx objects.
#'
#' This function converts a list of data.frames (with appropriate columns as
#' required for each predx class) and a vector of predx class names. It
#' returns a list of predx objects with (hopefully helpful) errors should any conversions fail.
#'
#' @param x list of data.frames (see Note)
#' @param class character vector of predx class names
#'
#' @return list of predx objects with error messages for failed conversions.
#' @export
#'
#' @examples
to_predx <- function(x, class) {
  if (length(x) != length(class)) stop('requires a predx_class name for each data.frame in x')
  predx <- vector("list", length(x))
  predx[class == 'Point'] <- lapply_Point(x[class == 'Point'])
  predx[class == 'Binary'] <- lapply_Binary(x[class == 'Binary'])
  predx[class == 'BinCat'] <- lapply_BinCat(x[class == 'BinCat'])
  predx[class == 'BinLwr'] <- lapply_BinLwr(x[class == 'BinLwr'])
  return(predx)
}

#' @export
is.predx <- function(x) {
  sapply(x, function(x) class(x)[1] %in% c('Point', 'Binary', 'BinCat', 'BinLwr'))
}

get_predx_cols <- function() {
  c('point', 'prob', 'cat', 'lwr')
}
