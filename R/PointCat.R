#' PointCat class: Point predictions for categorical variables
#'
#' A \code{predx} class for point predictions for categorical variables.
#'
#' \code{PointCat} objects contain a single point prediction of type character
#' with no other restrictions.
#'
#' In JSON and CSV representations, this value is named \code{point}.
#'
#' @slot predx A single point prediction of type character.
#'
#' @return
#' @export
#' @include transform_predx.R predx_to_json.R
#'
#' @examples
#'
setClass('PointCat', #S4 class
  slots = c(predx = 'character')
)

setValidity('PointCat', function(object) {
  ### structure checks
  collect_tests <- c(
    check_no_NAs(object@predx),
    if (is.character(object@predx)) TRUE else "requires a character value"
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
#' @rdname PointCat-class
PointCat <- function(x) {
  new("PointCat", predx = x)
}

lapply_PointCat <- function(x) {
  lapply(x, function(x, ...) tryCatch(PointCat(x$point),
    error=function(e) identity(e)))
}

#' @export
#' @rdname PointCat-class
is.PointCat <- function(object) {
  class(object) == 'PointCat'
}

#' @export
#' @rdname Point-class
setMethod("predx_to_json", "PointCat",
  function(x) { list(point = x@predx) })

#' @export
#' @rdname PointCat-class
setMethod("as.data.frame", "PointCat",
  function(x, ...) { data.frame(point = x@predx, stringsAsFactors = FALSE) })

#' @export
#' @rdname PointCat-class
setMethod("transform_predx", "PointCat",
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
setMethod("quantile", "PointCat", function(x) { NA })

setMethod("median", "PointCat", function(x) { NA })



