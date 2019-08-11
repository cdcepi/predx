#' Point class: Point predictions
#'
#' A \code{predx} class for point predictions.
#'
#' \code{Point} objects contain a single numeric point prediction with no other restrictions.
#'
#' In JSON and CSV representations, this value is named \code{point}.
#'
#' @slot predx A single numeric point prediction.
#'
#' @return
#' @export
#' @include transform_predx.R predx_to_json.R
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

#' @export
#' @rdname Point-class
Point <- function(x) {
  if (is.list(x)) x <- x$point
  new("Point", predx = x)
}

lapply_Point <- function(x) {
  lapply(x, function(x, ...) tryCatch(Point(x),
    error=function(e) identity(e)))
}

#' @export
#' @rdname Point-class
is.Point <- function(object) {
  class(object) == 'Point'
}

#' @export
#' @rdname Point-class
setMethod("predx_to_json", "Point",
  function(x) { list(point = x@predx) })

#' @export
#' @rdname Point-class
setMethod("as.data.frame", "Point",
  function(x, ...) { data.frame(point = x@predx) })

#' @export
#' @rdname Point-class
setMethod("transform_predx", "Point",
  function(x, to_class, ...) {
    if (to_class == class(x)) {
      return(x)
    } else {
      warning(paste0('NAs introduced by coercion, ', class(x), ' to ',
        to_class, ' not available'))
      return(NA)
    }
  })

# methods
setMethod("quantile", "Point", function(x) { NA })

setMethod("median", "Point", function(x) { x@predx })



