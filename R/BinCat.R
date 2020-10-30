#' BinCat class: Binned predictions with categorical bins
#'
#' A \code{predx} class for binned probabilistic predictions with categorical bins specified by strings.
#'
#' \code{BinCat} is flexible as it can be used for any binned predictions. Individual probabilities (\code{prob}) must be greater than or equal to 0 and less than or equal to 1 and the vector of probabilities must sum to 1.
#'
#' In JSON and CSV representations, the bins are named \code{cat} and the probabilities are named \code{prob}.
#'
#' @slot predx A data.frame with two columns: \code{cat} (character) and \code{prob} (numeric).
#'
#' @export
#' @include transform_predx.R predx_to_json.R
#'
#' @examples
#' x <- BinCat(data.frame(
#'   cat = c('a', 'b', 'c'),
#'   prob = c(0, 0.3, 0.7)
#' ))
#' x
#' # create predx dataframe
#' x_df <- as.predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = list(x, x)
#' ))
setClass('BinCat', #S4 class
  slots = c(predx = 'data.frame')
)

setValidity('BinCat', function(object) {
  ### structure checks
  collect_tests <- c(
    check_no_NAs(object@predx),
    if (ncol(object@predx) == 2) TRUE else "requires 2 columns",
    if (names(object@predx)[1] == 'cat') TRUE
      else "First colname should be 'cat'",
    if (names(object@predx)[2] == 'prob') TRUE
      else "Second colname should be 'prob'",
    if (is.character(object@predx$cat)) TRUE else "requires strings for 'cat'",
    if (is.numeric(object@predx$prob)) TRUE else "requires numeric for 'prob'"
    )
  ### content checks
  if (all(collect_tests == TRUE)) {
    collect_tests <- c(collect_tests,
      if (!any(duplicated(object@predx$cat))) TRUE else "duplicated 'cat', all must be unique",
      check_gte0(object@predx[ , 'prob'], 'probs'),
      check_lte1(object@predx[ , 'prob'], 'probs'),
      check_probs_sum_to_one(object@predx$prob)
    )
  }
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

#' @export
#' @rdname BinCat-class
BinCat <- function(x) {
  if (is.matrix(x)) x <- as.data.frame(x)
  if (!is.character(x$cat)) x$cat <- as.character(x$cat)
  new("BinCat", predx = x)
}

lapply_BinCat <- function(x) {
#  if (length(x) == 1) {
#    tryCatch(dplyr::select(x, cat, prob),
#        error=function(e) identity(e))
#  } else {
    lapply(x, function(x) {
        tryCatch(BinCat(data.frame(cat=as.character(x$cat), prob=x$prob,
        stringsAsFactors=F)),
        error=function(e) identity(e))
      })
#  }
}

#' @export
#' @rdname BinCat-class
is.BinCat <- function(x) {
  class(x) == 'BinCat'
}

#' @export
#' @rdname BinCat-class
setMethod("predx_to_json", "BinCat",
  function(x) { list(cat=x@predx$cat, prob=x@predx$prob) })

#' @export
#' @rdname BinCat-class
setMethod("as.data.frame", "BinCat",
  function(x, ...) { x@predx })

#' @export
#' @rdname BinCat-class
setMethod("transform_predx", "BinCat",
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
setMethod("quantile", "BinCat", function(x) { NA })

setMethod("median", "BinCat", function(x) { NA })

setMethod("mean", "BinCat", function(x) { NA })
