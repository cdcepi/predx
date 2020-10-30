#' Quant class: Predictions specified as a set of quantiles
#'
#' A \code{predx} class for binned probabilistic predictions with sequential numeric bins specified by the (inclusive) lower bound of each bin.
#'
#' Bins are equally spaced and specified by an inclusive lower bound (\code{lwr}). If the bin width is 0.1, \code{lwr} could be 0, 0.1, 0.2, etc. in which case the bins are 0 <= x < 0.1, 0.1 <= x < 0.2, etc. The non-inclusive upper bound of the final bin is assumed to be the maximum value of \code{lwr} plus the uniform bin width. Individual probabilities (\code{prob}) must be greater than or equal to 0 and less than or equal to 1 and the vector of probabilities must sum to 1.
#'
#' \code{lwr} and \code{prob} must be paired when a BinLwr object is created, but do not need to be in the order of \code{lwr}. The BinLwr object will order them.
#'
#' In JSON and CSV representations, the bins are named \code{lwr} and the probabilities are named \code{prob}.
#'
#' @slot predx A numeric matrix with two columns: \code{lwr} and \code{prob}.
#'
#' @export
#' @include transform_predx.R predx_to_json.R
#'
#' @examples
setClass('Quant', #S4 class
  slots = c(predx = 'matrix')
)

setValidity('Quant', function(object) {
  ### structure checks
  collect_tests <- c(
    check_no_NAs(object@predx),
    if (is.numeric(object@predx)) TRUE else "requires numeric matrix",
    if (ncol(object@predx) == 2) TRUE else "requires 2 columns",
    if (colnames(object@predx)[1] == 'quant') TRUE
      else "First colname should be 'quant'",
    if (colnames(object@predx)[2] == 'value') TRUE
      else "Second colname should be 'value'"
    )
  ### content checks
  if (all(collect_tests == TRUE)) {
    collect_tests <- c(collect_tests,
      check_gte0(object@predx[ , 'quant'], 'quantiles'),
      check_lte1(object@predx[ , 'quant'], 'quantiles'),
      check_ascend(object@predx[ , 'quant'], 'quantiles'),
      check_ascend(object@predx[ , 'value'], 'values', strict=F)
    )
  }
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

#' @export
#' @rdname Quant-class
Quant <- function(x) {
  if (is.list(x)) x <- cbind(quant=x$quant, value=x$value)
  if (is.character(x)) x <- apply(x, 2, as.numeric)
  x <- x[order(x[ , 'quant']), ]
  new("Quant", predx = x)
}

lapply_Quant <- function(x) {
  if (length(x) > 0) {
    if (is.list(x[[1]])) { # list or data.frame
      lapply(x, function(x, ...) tryCatch(Quant(cbind(quant=x$quant, value=x$value)),
        error=function(e) identity(e)))
    } else { # matrix
      lapply(x, function(x, ...) tryCatch(Quant(x[ , c('quant', 'value')]),
        error=function(e) identity(e)))
    }
  }
}

#' @export
#' @rdname Quant-class
is.Quant <- function(x) {
  class(x) == 'Quant'
}

#' @export
#' @rdname Quant-class
setMethod("predx_to_json", "Quant",
  function(x) { list(quant=x@predx[ , 'quant'], value=x@predx[ , 'value']) })

#' @export
#' @rdname Quant-class
setMethod("as.data.frame", "Quant",
  function(x, ...) { as.data.frame(x@predx) })

#' @export
#' @rdname Quant-class
setMethod("transform_predx", "Quant",
  function(x, to_class, ...) {
    if (to_class == class(x)) {
      return(x)
    } else {
      warning(paste0('NAs introduced by coercion, ', class(x), ' to ',
        to_class, ' not available'))
      return(NA)
    }
  })

######################################################################
### methods
setMethod("quantile", "Quant", function(x, p) {
  if (any(x@predx[ , 'quant'] == p)) x@predx[ , 'value'][x@predx[ , 'quant'] == p]
  else NA
})

setMethod("median", "Quant", function(x) {
  quantile(x, 0.5)
})



