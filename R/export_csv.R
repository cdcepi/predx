#' Export a \code{predx} data frame as a \code{predx}-formatted CSV file
#'
#' @param x A \code{predx} data frame.
#' @param filename Path or file. If NA, data.frame is returned.
#' @param overwrite Overwrite the file if it already exists? Default is FALSE.
#'
#' @return Silently saves a \code{predx}-formatted CSV if \code{filename} is specified. Otherwise returns a flat data frame in the format of the \code{predx} csv (i.e. without embedded \code{predx} objects).
#'
#' @export
#'
#' @examples
#' predx_demo <- as.predx_df(list(
#'  location = c('Mercury', 'Venus', 'Earth'),
#'  target = 'habitability',
#'  predx = list(Binary(1e-4), Binary(1e-4), Binary(1))
#' ))
#' csv_tempfile <- tempfile()
#' export_csv(predx_demo, csv_tempfile)
#' import_csv(csv_tempfile)
export_csv <- function(x, filename = NULL, overwrite = F) {
  x <- as.predx_df(x)
  x <- dplyr::mutate(x,
    predx = lapply(predx, function(x) as.data.frame(x) %>% dplyr::mutate_all(as.character)))
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
