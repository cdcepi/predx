#' Import a \code{predx}-formatted JSON file as a \code{predx} data frame
#'
#' @param json_file
#'
#' @return A \code{predx} data frame.
#' @export
#'
#' @examples
#' predx_demo <- as.predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = list(Binary(1e-4), Binary(1e-4), Binary(1))
#' ))
#' json_tempfile <- tempfile()
#' export_json(predx_demo, json_tempfile)
#' import_json(json_tempfile)
import_json <- function(file) {
  x <- jsonlite::fromJSON(file, flatten = T)
  if (!('predx_class' %in% names(x))) {
    stop('predx_class missing')
  }

  # identify and convert predx columns from import
  these_predx_cols <- names(x)[stringr::str_detect(names(x), 'predx\\.')]
  these_predx_cols <- stringr::str_remove(these_predx_cols, 'predx\\.')
  names(x) <- stringr::str_remove(names(x), 'predx\\.')

  # convert to predx_df
  x <- dplyr::as_tibble(x)
  x <- tidyr::nest(x, these_predx_cols, .key='predx')
  x$predx <- lapply(x$predx,
       function(x) {
         lapply(x, { function(x) x[[1]] } )
         })
  x$predx <- to_predx(x$predx, x$predx_class)

  if (any(check_conversion_errors(x$predx))) {
    print("Conversion errors found. Check predx column for errors.")
    return(x)
  } else {
    validate_predx_df(x)
    return(x)
  }
}

