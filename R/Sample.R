#' Sample class: Samples from a predictive distribution
#'
#' A \code{predx} class for samples from a predictive distribution.
#'
#' For now, the samples must be for a quantitative variable.
#'
#' In JSON and CSV representations, the samples are named \code{sample}.
#'
#' @slot predx A vector of samples.
#'
#' @return
#' @export
#' @include transform_predx.R predx_to_json.R
#'
#' @examples
setClass('Sample', #S4 class
  slots = c(predx = 'numeric')
)

setValidity('Sample', function(object) {
  ### structure checks
  collect_tests <- c(
    check_no_NAs(object@predx),
    if (is.vector(object@predx, mode = "numeric")) TRUE else "requires numeric vector"
  )
  if (all(collect_tests == TRUE)) TRUE
  else collect_tests[collect_tests != TRUE]
})

#' @export
#' @rdname Sample-class
Sample <- function(x) {
  new("Sample", predx = x)
}

lapply_Sample <- function(x) {
  lapply(x, function(x, ...) tryCatch(Sample(x$sample),
    error=function(e) identity(e)))
}

#' @export
#' @rdname Sample-class
is.Sample <- function(x) {
  class(x) == 'Sample'
}

#' @export
#' @rdname Sample-class
setMethod("predx_to_json", "Sample",
  function(x) { list(sample = x@predx) })

#' @export
#' @rdname Sample-class
setMethod("as.data.frame", "Sample",
  function(x, ...) { data.frame(sample = x@predx) })

#' @export
#' @rdname Sample-class
setMethod("transform_predx", "Sample",
  function(x, to_class, lwr, ...) {
    if (to_class == class(x)) {
      return(x)
    } else if (to_class == 'BinLwr') {
      if(missing(lwr)) {
        stop('argument lwr must be specified for transformations to BinLwr class.')
      }
      bin_width <- lwr[2] - lwr[1]
      bins <- c(lwr, lwr[length(lwr)] + bin_width)
      bin_counts <- hist(x@predx, bins, right = FALSE, plot = FALSE)$counts

      binlwr_df <- data.frame(
        lwr = lwr,
        prob = bin_counts / sum(bin_counts)
      )
      return(BinLwr(binlwr_df))
    } else {
      warning(paste0('NAs introduced by coercion, ', class(x), ' to ',
        to_class, ' not available'))
      return(NA)
    }
  })


######################################################################
### methods
setMethod("quantile", "Sample", function(x, probs = seq(0, 1, 0.25), names = TRUE, type = 7, ...) {
  quantile(x@predx, probs = probs, names = names, type = type)
})

setMethod("median", "Sample", function(x, ...) {
  median(x@predx, ...)
})
