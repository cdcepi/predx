#' Import predx formatted csv
#'
#'
#' @param file A csv file/path *or* a data.frame object. Either should have a `predx_class` column and whatever columns are needed for the incldued classes (e.g. point, bin, prob).
#'
#' @return `predx` tibble
#' @export
#' @include to_predx.R
#'
#' @examples
#' import_csv('vignettes/fcast_demo.csv')
import_csv <- function(file=NULL) {
  if (is.data.frame(file)) {
    x <- file
  } else {
    x <- read.csv(file, stringsAsFactors=F)
  }
  all_predx_cols <- get_predx_cols()
  these_predx_cols <- all_predx_cols[all_predx_cols %in% names(x)]

  x <- tidyr::nest(x, !! these_predx_cols, .key='predx')
  x <- dplyr::mutate(x, predx = to_predx(predx, predx_class))

  if (any(check_conversion_errors(x$predx))) {
    print("Conversion errors found. Check predx column for errors.")
    return(x)
  }

  predx_df(x)
}
