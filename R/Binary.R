#' Binary predictions
#'
#' This predx class is used to capture binary probabilistic predictions. It contains a single numeric probability that is greater than or equal to 0 and less than or equal to 1. In JSON and CSV representations, this probability is named 'prob'.
#'
#' @slot predx Contains a single numeric probability (0 <= p <= 1).
#'
#' @return
#' @export
#' @include get_predx.R
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

Binary <- function(x) {
  new("Binary", predx = x)
}

lapply_Binary <- function(x) {
  lapply(x, function(x, ...) tryCatch(Binary(x$prob),
    error=function(e) identity(e)))
}

is.Binary <- function(object) {
  class(object) == 'Binary'
}

setMethod("as.list", "Binary",
  function(x, ...) { list(prob = x@predx) })

setMethod("as.data.frame", "Binary",
  function(x, ...) { data.frame(prob = x@predx) })


