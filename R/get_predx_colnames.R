#' Internal function returns the names of columns used for conversion to predx objects
#'
#' @param classes A vector of predx classes names (e.g. 'Point', 'Binary').
#'
#' @return `predx` tibble
#'
#' @examples
#' get_predx_colnames(c("Binary", "Point", "BinCat"))
get_predx_colnames <- function(classes) {
  all_colnames <- list(
    Binary = 'prob',
    BinCat = c('cat', 'prob'),
    BinLwr = c('lwr', 'prob'),
    Point = 'point',
    PointCat = 'point',
    Sample = 'sample',
    SampleCat = 'sample'
  )
  these_colnames <- all_colnames[unique(classes)]
  unique(unlist(these_colnames))
}
