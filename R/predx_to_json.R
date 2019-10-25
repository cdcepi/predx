#' Internal code to convert \code{predx} to JSON friendly representation.
#'
#' @param x a \code{predx} object or list of \code{predx} objects
#'
#' @return A \code{predx} object or list of \code{predx} objects, with NAs and warning messages for failed conversions.
#'
setGeneric("predx_to_json", function(x) {
  if (!is.list(x) & length(x) == 1) {
    if (!is.predx(x)) {
      stop('x must be a predx object or list of predx objects')
    }
  } else if (!all(sapply(x, is.predx))) {
    stop('x must be a predx object or list of predx objects')
  }
  standardGeneric("predx_to_json") })

#' @rdname predx_to_json
setMethod("predx_to_json", "list",
  function(x) {
    lapply(x, predx_to_json)
  })

