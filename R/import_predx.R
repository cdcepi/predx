#' Import from \code{predx}-formatted JSON or \code{predx}-formatted CSV file as a \code{predx} data frame
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
import_predx <- function(x) {
  if (!is.data.frame(x)) {
    if (str_detect(tolower(x), '\.json$')) {
      import_json(x)
    }
  } else {
    import_csv(x)
  }
}
