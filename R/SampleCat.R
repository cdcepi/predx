#' SampleCat class: Samples from a predictive distribution over categorical bins.
#'
#' A \code{predx} class for samples from a predictive distribution over
#' categorical bins.
#'
#' In JSON and CSV representations, the samples are named \code{sample}.
#'
#' @slot predx A list with two entries: \code{cat} (character) and \code{sample}
#'   (character).  \code{cat} contains all possible categorical bins and
#'   \code{sample} is a vector of samples of those bins.
#'
#' @return
#' @export
#' @include transform_predx.R
#'
#' @examples
setClass('SampleCat', #S4 class
  slots = c(predx = 'list')
)

setValidity('SampleCat', function(object) {
  ### structure checks
  collect_tests <- c(
    if (length(object@predx) == 2) TRUE else "requires 2 entries",
    if (length(object@predx) == 2) check_no_NAs(object@predx[[1]]) else NULL, # only check NA's if exists
    if (length(object@predx) == 2) check_no_NAs(object@predx[[2]]) else NULL, # only check NA's if exists
    if (all.equal(sort(names(object@predx)), c('cat', 'sample'))) TRUE
      else "Should contain components 'cat' and 'sample'",
    if (is.character(object@predx$cat)) TRUE else "requires strings for 'cat'",
    if (is.character(object@predx$sample)) TRUE else "requires character for 'sample'"
  )
  ### content checks
  if (all(collect_tests == TRUE)) {
    collect_tests <- c(collect_tests,
      if (!any(duplicated(object@predx$cat))) TRUE else "duplicated 'cat', all must be unique",
      if (all(object@predx$sample %in% object@predx$cat)) TRUE else "'sample' included values not in 'cat', 'cat' should list all possible categories."
    )
  }
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

#' @export
#' @rdname SampleCat-class
SampleCat <- function(x) {
  new("SampleCat", predx = x)
}

lapply_SampleCat <- function(x) {
  lapply(x, function(x) {
      tryCatch(SampleCat(
        list(
          cat=unique(c(attr(x, "cat"), as.character(x$cat))),
          sample=as.character(x$sample))),
        error=function(e) identity(e))
    })
}

#' @export
#' @rdname SampleCat-class
is.SampleCat <- function(x) {
  class(x) == 'SampleCat'
}

#' @export
#' @rdname SampleCat-class
setMethod("as.list", "SampleCat",
  function(x, ...) { list(cat = x@predx$cat, sample = x@predx$sample) })

#' @export
#' @rdname SampleCat-class
setMethod("as.data.frame", "SampleCat",
  function(x, ...) {
    result <- data.frame(sample = x@predx$sample, stringsAsFactors = FALSE)
    attr(result, "cat") <- x@predx$cat
    return(result)
  })

#' @export
#' @rdname SampleCat-class
setMethod("transform_predx", "SampleCat",
  function(x, to_class, cat, ...) {
    if (to_class == class(x)) {
      return(x)
    } else if (to_class == 'BinCat') {
      if (any(!(get_cats(x) %in% cat))) {
        stop('All categories present in x must be included in the cat argument')
      }
      bincat_df <- data.frame(
        cat = cat,
        prob = sapply(cat, function(cat_i) {
          mean(x@predx$sample == cat_i)
        }),
        stringsAsFactors = FALSE)
      return(BinCat(bincat_df))
    } else {
      warning(paste0('NAs introduced by coercion, ', class(x), ' to ',
        to_class, ' not available'))
      return(NA)
    }
  })

######################################################################
### methods
setMethod("quantile", "SampleCat", function(x, ...) { NA })

setMethod("median", "SampleCat", function(x, ...) { NA })

setMethod("mean", "SampleCat", function(x, ...) { NA })
