#' Import a \code{predx}-formatted CSV file as a \code{predx} data frame
#'
#' @param file A csv file/path *or* a data.frame object. Either should have a \code{predx_class} column and whatever columns are needed for the included classes (e.g. point, bin, prob).
#'
#' @return A \code{predx} data frame.
#' @export
#' @include get_predx_colnames.R to_predx.R
#'
#' @examples
#' #' predx_demo <- as.predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = list(Binary(1e-4), Binary(1e-4), Binary(1))
#' ))
#' csv_tempfile <- tempfile()
#' export_csv(predx_demo, csv_tempfile)
#' import_csv(csv_tempfile)
import_csv <- function(file=NULL) {
  if (is.data.frame(file)) {
    x <- file
  } else {
    x <- read.csv(file, stringsAsFactors=F)
  }
  these_predx_cols <- get_predx_colnames(x$predx_class)

  x <- tidyr::nest(x, !! these_predx_cols, .key='predx')
  x <- dplyr::mutate(x, predx = to_predx(predx, predx_class))

  if (any(check_conversion_errors(x$predx))) {
    print("Conversion errors found. Check predx column for errors.")
    return(x)
  }

  as.predx_df(x)
}
