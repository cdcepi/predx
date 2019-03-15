#' Verify inclusion of expected predictions
#'
#' Function to check a set of predictions for a set of expected predictions
#' (e.g. targets, locations)
#'
#' @param x predx_df
#' @param expected_list list of lists (see Note)
#' @param return_df if \code{TRUE} (default is \{FALSE}) will return a list of missing predictions
#' @param print_output if \code{TRUE} (default) prints results
#'
#' @note \code{expected_list} is a two-level list of sets of expected predictions.
#' The lower level (e.g. \code{expected_list[[1]]}) is a list of named character vectors
#' with specific expected predictions. The names should match column names
#' in \code{x}. The function checks that all combinations of those elements are
#' present in \code{x}. A named vector \code{predx_class} may be used to check
#' predx types.
#'
#' @return If any predictions in \code{expected_list} are not found or if additional predictions are found, the function will print the missing and/or additional rows (\code{print_out = TRUE}) or return a data frame with a status designation for each missing and/or additional row (\code{return_df = TRUE}).
#'
#' @export
#'
#' @examples
#' predx_demo <- as.predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = list(Binary(1e-4), Binary(1e-4), Binary(1))
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

verify_expected <- function(x, expected_list, return_df = FALSE,
    print_ouput = !return_df) {
  if (!is.predx_df(x)) stop("x is not a predx_df")
  if (!is.list(expected_list)) stop("expected_list is not a list")

  all_exp <- dplyr::bind_rows(lapply(expected_list, expand.grid, stringsAsFactors=F))
  # check missing predictions
  missing_predx <- dplyr::setdiff(all_exp, x[ , names(all_exp)])
  if (nrow(missing_predx) > 0 & print_ouput) {
    print('The following predictions are missing:')
    print(dplyr::as_tibble(missing_predx))
  }

  # check additional predictions found
  additional_predx <- dplyr::setdiff(x[ , names(all_exp)], all_exp)
  if (nrow(additional_predx) > 0 & print_ouput) {
    print('The following additional predictions were found:')
    print(dplyr::as_tibble(additional_predx))
  }

  if (nrow(missing_predx) == 0 & print_ouput) cat("All expected predictions found.\n")
  if (return_df) {
    return(dplyr::bind_rows(
      dplyr::mutate(missing_predx, status = 'missing'),
      dplyr::mutate(additional_predx, status = 'additional')
      ))
  }
}


