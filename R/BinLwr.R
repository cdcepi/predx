#' BinLwr class: Binned predictions with sequential numeric bins
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
#' @return
#' @export
#' @include transform_predx.R
#'
#' @examples
setClass('BinLwr', #S4 class
  slots = c(predx = 'matrix')
)

setValidity('BinLwr', function(object) {
  ### structure checks
  collect_tests <- c(
    check_no_NAs(object@predx),
    if (is.numeric(object@predx)) TRUE else "requires numeric matrix",
    if (ncol(object@predx) == 2) TRUE else "requires 2 columns",
    if (colnames(object@predx)[1] == 'lwr') TRUE
      else "First colname should be 'lwr'",
    if (colnames(object@predx)[2] == 'prob') TRUE
      else "Second colname should be 'prob'"
    )
  ### content checks
  if (all(collect_tests == TRUE)) {
    collect_tests <- c(collect_tests,
      check_probs_gt0(object@predx[ , 'prob']),
      check_probs_lt1(object@predx[ , 'prob']),
      check_probs_sum_to_one(object@predx[ , 'prob']),
      check_bins_ascend(object@predx[ , 'lwr']),
      check_uniform_bin_size(object@predx[ , 'lwr'])
    )
  }
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

#' @export
#' @rdname BinLwr-class
BinLwr <- function(x) {
  if (is.list(x)) x <- cbind(lwr=x$lwr, prob=x$prob)
  if (is.character(x)) x <- apply(x, 2, as.numeric)
  x <- x[order(x[ , 'lwr']), ]
  new("BinLwr", predx = x)
}

lapply_BinLwr <- function(x) {
  if (length(x) > 0) {
    if (is.list(x[[1]])) { # list or data.frame
      lapply(x, function(x, ...) tryCatch(BinLwr(cbind(lwr=x$lwr, prob=x$prob)),
        error=function(e) identity(e)))
    } else { # matrix
      lapply(x, function(x, ...) tryCatch(BinLwr(x[ , c('lwr', 'prob')]),
        error=function(e) identity(e)))
    }
  }
}

#' @export
#' @rdname BinLwr-class
is.BinLwr <- function(x) {
  class(x) == 'BinLwr'
}

#' @export
#' @rdname BinLwr-class
setMethod("as.list", "BinLwr",
  function(x, ...) { list(lwr=x@predx[ , 'lwr'], prob=x@predx[ , 'prob']) })

#' @export
#' @rdname BinLwr-class
setMethod("as.data.frame", "BinLwr",
  function(x, ...) { as.data.frame(x@predx) })

#' @export
#' @rdname BinLwr-class
setMethod("transform_predx", "BinLwr",
  function(x, to_class, ...) {
    if (to_class == 'BinLwr') {
      return(x)
    } else {
      warning(paste0('NAs introduced by coercion, BinLwr to ', to_class, ' not available'))
      return(NA)
    }
  })

######################################################################
### methods
#setMethod("quantile", "BinLwr", function(x, p, side) {
#  mean(x@lwr[sum(cumsum(x@prob) < 0.5) + 1:2])
#})

#setMethod("median", "BinLwr", function(x) { NA })

setMethod("median", "BinLwr", function(x) {
  mean(x@predx[[1]][sum(cumsum(x@predx[[2]]) < 0.5) + 1:2])
})

setGeneric("bin_width", function(x)
  { standardGeneric("bin_width") })
setMethod("bin_width", "BinLwr", function(x) {
  x@predx$lwr[2] - x@predx$lwr[1]
  })

setGeneric("bin_range", function(x)
  { standardGeneric("bin_range") })
setMethod("bin_range", "BinLwr", function(x) {
  c(min(x@pred$lwr), max(x@pred$lwr) + bin_width(x))
  })


