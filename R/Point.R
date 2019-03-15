#' Point predictions
#'
#' This predx class is used to capture point predictions. It contains a single numeric point predictions with no other restrictions. In JSON and CSV representations, this value is named 'point'.
#'
#' @slot predx Contains a single numeric point prediction.
#'
#' @return
#' @export
#'
#' @examples
#'
setClass('Point', #S4 class
  slots = c(predx = 'numeric')
)

setValidity('Point', function(object) {
  ### structure checks
  collect_tests <- c(
    check_no_NAs(object@predx),
    if (is.numeric(object@predx)) TRUE else "requires a numeric value"
    )
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

setMethod("as.list", "Point",
  function(x, ...) { list(point = x@predx) })

setMethod("as.data.frame", "Point",
  function(x, ...) { data.frame(point = x@predx) })

# methods
setMethod("quantile", "Point", function(x) { NA })

setMethod("median", "Point", function(x) { x@predx })



