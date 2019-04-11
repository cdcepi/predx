#' Binary class: Binary predictions
#'
#' A predx class to capture binary probabilistic predictions.
#'
#' It contains a single numeric probability that is greater than or equal to 0 and less than or equal to 1.
#'
#' In JSON and CSV representations, this probability is named \code{prob}.
#'
#' @slot predx A single numeric probability.
#'
#' @return
#' @export
#' @include check_no_NAs.R check_probs_gt0.R check_probs_lt1.R check_single_value.R
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
  new("Binary", predx = x)
}

lapply_Binary <- function(x) {
  lapply(x, function(x, ...) tryCatch(Binary(x$prob),
    error=function(e) identity(e)))
}

#' @export
#' @rdname Binary-class
is.Binary <- function(object) {
  class(object) == 'Binary'
}

#' @export
#' @rdname Binary-class
setMethod("as.list", "Binary",
  function(x, ...) { list(prob = x@predx) })

#' @export
#' @rdname Binary-class
setMethod("as.data.frame", "Binary",
  function(x, ...) { data.frame(prob = x@predx) })


