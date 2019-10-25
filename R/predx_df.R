#' Tools to check the \code{predx} data frames contain valid \code{predx_class} and \code{predx} columns and created them if needed.
#'
#' \code{is.predx_df} will verify that a \code{predx} data frame contains valid \code{predx_class} and \code{predx} columns. \code{as.predx_df} will create a \code{predx} data frame from a list or tibble and add the \code{predx_class} column if not found.
#'
#' @param x A list or a tibble including a list or column named \code{predx} containing only \code{predx} objects. Not a base data.frame because the \code{predx} objects cannot be embedded, \code{as.predx_df} will embed them.
#'
#' @return Error message or TRUE.
#' @export
#' @include to_predx.R
#'
#' @examples
#' as.predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = list(Binary(0), Binary(0), Binary(1))
#' ))
#' x <- as.predx_df(
#'   list(
#'     target = c('demoCat', 'demoBinary'),
#'     date = 'tomorrow',
#'     predx = list(
#'       BinCat(
#'         data.frame(
#'           cat = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
#'           prob = c(0, rep(0.1, 4), 0.25, 0.15, 0.1, rep(0.05, 2))
#'           )
#'         ),
#'       Binary(0.1)
#'       )
#'     )
#'   )
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
      if (all(sapply(x$predx, is.predx))) TRUE else "non-predx objects found in predx column",
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

#' @export
#' @rdname predx_df
is.predx_df <- function(x) {
  validate_predx_df(x, return=T)
}

#' @export
#' @rdname predx_df
as.predx_df <- function(x) {
  if (is.list(x)) x <- dplyr::as_tibble(x)
  if (!('predx_class' %in% names(x))) x$predx_class <- sapply(x$predx, class)
  validate_predx_df(x)
  return(x)
}
