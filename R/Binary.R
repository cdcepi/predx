#' Binary class: Binary predictions
#'
#' A \code{predx} class for binary probabilistic predictions.
#'
#' A single numeric probability that is greater than or equal to 0 and less than or equal to 1.
#'
#' In JSON and CSV representations, this probability is named \code{prob}.
#'
#' @slot predx A single numeric probability.
#'
#' @return
#' @export
#' @include transform_predx.R predx_to_json.R
#'
#' @examples
setClass('Binary', #S4 class
  slots = c(predx = 'numeric')
)

setValidity('Binary', function(object) {
  ### structure checks
  collect_tests <- check_no_NAs(object@predx)
  ### content checks
  if (all(collect_tests == TRUE)) {
    collect_tests <- c(collect_tests,
      check_probs_gt0(object@predx),
      check_probs_lt1(object@predx),
      check_single_value(object@predx)
    )
  }
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

#' @export
#' @rdname Binary-class
Binary <- function(x) {
  if (is.list(x)) x <- x$prob
  new("Binary", predx = x)
}

lapply_Binary <- function(x) {
  lapply(x, function(x, ...) tryCatch(Binary(x),
    error=function(e) identity(e)))
}

#' @export
#' @rdname Binary-class
is.Binary <- function(object) {
  class(object) == 'Binary'
}

#' @export
#' @rdname Binary-class
setMethod("predx_to_json", "Binary",
  function(x) { c(prob = x@predx) })

#' @export
#' @rdname Binary-class
setMethod("as.data.frame", "Binary",
  function(x, ...) { data.frame(prob = x@predx) })

#' @export
#' @rdname Binary-class
setMethod("transform_predx", "Binary",
  function(x, to_class, ...) {
    if (to_class == class(x)) {
      return(x)
    } else {
      warning(paste0('NAs introduced by coercion, ', class(x), ' to ',
        to_class, ' not available'))
      return(NA)
    }
  })
