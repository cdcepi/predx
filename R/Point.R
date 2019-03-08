#' Title
#'
#' @slot predx numeric.
#'
#' @return
#' @export
#'
#' @examples
#' @include get_predx.R
#'
setClass('Point', #S4 class
  slots = c(predx = 'numeric')
)

setValidity('Point', function(object) {
  ### structure checks
  collect_tests <- check_no_NAs(object@predx)
  ### content checks
  if (all(collect_tests == TRUE)) {
    collect_tests <- c(collect_tests,
      check_single_value(object@predx)
    )
  }
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

Point <- function(x) {
  new("Point", predx = x)
}

lapply_Point <- function(x) {
  lapply(x, function(x, ...) tryCatch(Point(x$point),
    error=function(e) identity(e)))
}

is.Point <- function(object) {
  class(object) == 'Point'
}

setMethod("get_predx", "Point",
  function(x, ...) { x@predx })

setMethod("as.list", "Point",
  function(x, ...) { list(point = x@predx) })

setMethod("as.data.frame", "Point",
  function(x, ...) { data.frame(point = x@predx) })

# methods
setMethod("quantile", "Point", function(x) { NA })

setMethod("median", "Point", function(x) { NA })



