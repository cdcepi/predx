#' SampleCat class: Samples from a distribution of categories or classifications.
#'
#' A \code{predx} class for samples from a distribution of
#' categories or classifications specified as character strings.
#'
#' In JSON and CSV representations, the samples are named \code{sample}.
#'
#' @export
#' @include transform_predx.R predx_to_json.R
#'
#' @examples
#' x <- SampleCat(sample(letters, size = 10))
#' x
#'
setClass('SampleCat', #S4 class
  slots = c(predx = 'character')
)

setValidity('SampleCat', function(object) {
  ### structure checks
  collect_tests <- c(
    check_no_NAs(object@predx),
    if (is.character(object@predx)) TRUE else "requires character vector"
  )
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

#' @export
#' @rdname SampleCat-class
SampleCat <- function(x) {
  new("SampleCat", predx = x)
}

check_old_SampleCat <- function(x) {
  if (is.list(x)) {
    if ('cat' %in% names(x)) {
      if (any(!is.na(x$cat))) {
        warning("SampleCat contains only samples. To maintain categories with no observations use BinCat.")
      }
    }
  }
}

# conversion of a list
lapply_SampleCat <- function(x) {
  lapply(x, function(x) {
    check_old_SampleCat(x)
    tryCatch(SampleCat(as.character(x$sample)),
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
setMethod("predx_to_json", "SampleCat",
  function(x) { list(sample = x@predx) })

#' @export
#' @rdname SampleCat-class
setMethod("as.data.frame", "SampleCat",
  function(x, ...) {
    data.frame(sample = x@predx, stringsAsFactors = FALSE)
  })

#' @export
#' @rdname SampleCat-class
setMethod("transform_predx", "SampleCat",
  function(x, to_class, cat, ...) {
    if (to_class == class(x)) {
      return(x)
    } else if (to_class == 'BinCat') {
      bincat_df <- data.frame(
        cat = cat,
        prob = sapply(cat, function(cat_i) {
          mean(x@predx == cat_i)
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
