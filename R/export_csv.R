#' Title
#'
#' @param forecast predx_df object.
#' @param filename Path or file. If NA, data.frame is returned.
#' @param overwrite Overwrite the file if it already exists? Default is FALSE.
#'
#' @return
#' @export
#'
#' @examples
export_csv <- function(x, filename = NULL, overwrite = F) {
  x <- as.predx_df(x)
  x <- dplyr::mutate(x,
    predx = lapply(predx, function(x) as.data.frame(x)))
  x <- tidyr::unnest(x, predx)

  if (!is.null(filename)) {
    if (!overwrite & file.exists(filename)) {
      stop(paste0('"', filename, '" already exists. Use "overwrite = T" to replace.'))
    } else {
      write.csv(x, filename, row.names=F)
    }
  } else {
    return(x)
  }
}
