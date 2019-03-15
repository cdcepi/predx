#' Title
#'
#'
#' @return
#' @export
#' @include to_predx.R
#'
#' @examples
#' predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = list(Binary(0), Binary(0), Binary(1))
#' ))
#' @name predx_df
validate_predx_df <- function(x, return=F) {
  ### structure checks
  collect_tests <- c(
    if (any(class(x) == 'tbl_df')) TRUE else "object is not a tbl_df object",
    if ('predx' %in% names(x)) TRUE else "predx column not found",
    if ('predx_class' %in% names(x)) TRUE else "predx_class column not found"
  )
  ### content checks
  if (all(collect_tests == TRUE)) {
    collect_tests <- c(collect_tests,
      if (all(is.predx(x$predx))) TRUE else "non-predx objects found in predx column",
      if (all(x$predx_class == sapply(x$predx, class))) TRUE
      else "predx_class column does not match predx classes"
    )
  }
  if (any(collect_tests != TRUE)) {
    stop(paste0(c('', collect_tests[collect_tests != TRUE]), collapse='\n'))
  } else if (return) {
    return(TRUE)
  }
}

predx_df <- function(x) {
  if (is.list(x)) x <- dplyr::as_tibble(x)
  if (!('predx_class' %in% names(x))) x$predx_class <- sapply(x$predx, class)
  validate_predx_df(x)
  return(x)
}

is.predx_df <- function(x) {
  validate_predx_df(x, return=T)
}

as.predx_df <- function(x) {
  if (is.predx_df(x)) return(x)
  x <- try(predx_df(x))
  if ('try-error' %in% class(x)) stop("x cannot be interpreted as a predx_df object")
  return(x)
}

