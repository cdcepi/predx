#' Export a \code{predx} data frame as a \code{predx}-formatted JSON file
#'
#' @param forecast A \code{predx} data frame.
#' @param filename character. Filename to write JSON file. If NA, JSON is returned.
#' @param overwrite boolean. Overwrite the file if it already exists.
#'
#' @return Silently saves a \code{predx}-formatted JSON if \code{filename} is specified. Otherwise returns a string in the format of the \code{predx} JSON.
#'
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
