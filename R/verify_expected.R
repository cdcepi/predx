#' Verify inclusion of expected predictions
#'
#' Function to check a set of predictions for a set of expected predictions
#' (e.g. targets, locations)
#'
#' @param x predx_df
#' @param expected_list list of lists (see Note)
#' @param return_list if \code{TRUE} will return a list of missing predictions
#'
#' @note \code{expected_list} is a two-level list of sets of expected predictions.
#' The lower level (e.g. \code{expected_list[[1]]}) is a list of named character vectors
#' with specific expected predictions. The names should match column names
#' in \code{x}. The function checks that all combinations of those elements are
#' present in \code{x}. A named vector \code{predx_class} may be used to check
#' predx types.
#'
#' Different sets in the first level list (e.g.
#' \code{expected_list[[1]]} and \code{expected_list[[2]]}) are checked separately.
#'
#' @return If any forecasts in \code{expected_list} are not found, the function returns
#' a list of data.frames of combinations of expected variables for which no
#' predictions were present in \code{x}.
#' @export
#'
#' @examples
#' predx_demo <- predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = as.Binary(c(1e-4, 1e-4, 1))
#' ))
#'
#' expected_demo <- list(
#'   list(
#'     location = c('Mercury', 'Venus', 'Earth'),
#'     target = 'habitability',
#'     predx_class = 'Binary')
#'   )
#'
#' expected_demo2 <- list(
#'   list(
#'     location = c('Mercury', 'Mars'),
#'     target = 'habitability',
#'     predx_class = 'Binary')
#'   )
#'
#' verify_expected(predx_demo, expected_demo)
#' verify_expected(predx_demo, expected_demo2)

verify_expected <- function(x, expected_list, return_df=F) {
  if (!is.predx_df(x)) stop("x is not a predx_df")
  if (!is.list(expected_list)) stop("expected_list is not a list")

  all_exp <- dplyr::bind_rows(lapply(expected_list, expand.grid, stringsAsFactors=F))
  # check missing predictions
  missing_predx <- dplyr::setdiff(all_exp, x[ , names(all_exp)])
  if (nrow(missing_predx) > 0) {
    print('The following predictions are missing:')
    print(missing_predx)
  }

  # check additional predictions found
  additional_predx <- dplyr::setdiff(x[ , names(all_exp)], all_exp)
  if (nrow(additional_predx) > 0) {
    print('The following additional predictions were found:')
    print(additional_predx)
  }

  if (nrow(missing_predx) == 0) cat("All expected predictions found.\n")
  if (return_df) {
    return(dplyr::bind_rows(
      dplyr::mutate(missing_predx, status = 'missing'),
      dplyr::mutate(additional_predx, status = 'additional')
      ))
  }
}


