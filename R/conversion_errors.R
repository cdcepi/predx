#' Title
#'
#' @param x list of predx objects.
#'
#' @return
#' @export
#'
#' @examples
check_conversion_errors <- function(x, msg=F) {
  if (msg) {
    if (is.list(x)) sapply(x, collect_error)
    else collect_error(x)
  }
  else {
    if (is.list(x)) sapply(x, function(x) inherits(x, "simpleError"))
    else FALSE
  }
}

#' @describeIn check_conversion_errors function to collect individual errors
collect_error <- function(x) {
  if (inherits(x, "simpleError")) as.character(x)
  else "none"
}
