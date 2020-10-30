#' Title
#'
#' @param bins
#'
#' @return  TRUE or error message
#'
check_ascend <- function(bins, colname, strict=T) {
  if (strict) {
    if (all(diff(bins) > 0)) return(TRUE)
    else return(paste0(colname, " are not all in ascending order"))
  } else {
    if (all(diff(bins) >= 0)) return(TRUE)
    else return(paste0(colname, " are not all in ascending order"))
  }
}

