#' Transform a \code{predx} objects to a target predx class.
#'
#' @details This function converts a \code{predx} object or list of \code{predx}
#' objects to another specified \code{predx} class when possible.
#'
#' @param x a \code{predx} object or list of \code{predx} objects
#' @param to_class character specifying \code{predx} class to convert to, e.g. "BinLwr"
#' @param cat if to_class is "BinCat", character vector of categorical bin names
#' @param lwr if to_class is "BinLwr", numeric vector of lower bounds for bins
#'
#' @return A \code{predx} object or list of \code{predx} objects, with NAs and warning messages for failed conversions.
#' @export
setGeneric("transform_predx", function(x, to_class, ...) {
  if (length(x) == 1) {
    if (!is.predx(x)) {
      stop('x must be a predx object or list of predx objects')
    }
  } else if (!all(sapply(x, is.predx))) {
    stop('x must be a predx object or list of predx objects')
  }
  if (length(to_class) != 1) {
    stop('to_class must be a single predx class name')
  }
  standardGeneric("transform_predx") })

#' @export
#' @rdname transform_predx
setMethod("transform_predx", "list",
  function(x, to_class, ...) {
    result <- lapply(x, transform_predx, to_class=to_class, ...)
  })

