#' Title
#'
#' @param forecast predx_df.
#' @param filename character. Filename to write JSON file. If NA, JSON is returned.
#' @param overwrite boolean. Overwrite the file if it already exists.
#'
#' @return
#' @export
#'
#' @examples
#' export_json(x, 'inst/data/flusight.json')
export_json <- function(x, filename = NA, overwrite = F) {
  x <- as.predx_df(x)
  # make distr a list for efficient storage
  x$predx = lapply(x$predx, function(x) as.list(x))
  out <- jsonlite::toJSON(x, na='string', digits=NA)
  if (!is.na(filename)) {
    if (!overwrite & file.exists(filename)) {
      stop(paste0('"', filename, '" already exists. Use "overwrite = T" to replace.'))
    } else {
      writeLines(out, filename)
    }
  } else {
    return(out)
  }
}
