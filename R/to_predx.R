#' Helper function for creating lists of \code{predx} objects (e.g. a column in a data frame containing a variety of \code{predx} objects)
#'
#' This function converts a data.frame or list of data frames (with appropriate columns and/or attributes as required for each predx class) and a vector of predx class names to a list of predx objects with (hopefully helpful) errors should any conversions fail.
#'
#' @param x A list of data.frames.
#' @param class A character vector of \code{predx} class names
#'
#' @return A list of \code{predx} objects with error messages for failed conversions.
#' @export
#'
#' @examples
to_predx <- function(x, class) {
  if (length(x) != length(class)) stop('requires a predx_class name for each data.frame in x')

  predx <- vector("list", length(x))
  predx[class == 'Point'] <- lapply_Point(x[class == 'Point'])
  predx[class == 'PointCat'] <- lapply_PointCat(x[class == 'PointCat'])
  predx[class == 'Binary'] <- lapply_Binary(x[class == 'Binary'])
  predx[class == 'BinCat'] <- lapply_BinCat(x[class == 'BinCat'])
  predx[class == 'BinLwr'] <- lapply_BinLwr(x[class == 'BinLwr'])
  predx[class == 'Sample'] <- lapply_Sample(x[class == 'Sample'])
  predx[class == 'SampleCat'] <- lapply_SampleCat(x[class == 'SampleCat'])

  return(predx)
}

#' @export
is.predx <- function(x) {
  class(x) %in% c('Point', 'PointCat', 'Binary', 'BinCat', 'BinLwr', 'Sample', 'SampleCat')
}
