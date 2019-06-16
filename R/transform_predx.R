#' Transform a \code{predx} objects to a target predx class.
#'
#' @details This function converts a \code{predx} object or list of \code{predx}
#' objects to a specified \code{predx} class.  Currently, the only supported
#' transformations are from Sample to BinLwr and from SampleCat to BinCat.
#'
#' @param x a \code{predx} object or list of \code{predx} objects
#' @param class character specifying \code{predx} class to convert to, e.g. "BinLwr"
#' @param cat if class is "BinCat", character vector of categorical bin names
#' @param lwr if class is "BinLwr", numeric vector of lower bounds for bins
#'
#' @return A \code{predx} object or list of \code{predx} objects, with NAs and warning messages for failed conversions.
#' @export
transform_predx <- function(x, class, cat, lwr) {
  if (length(class) != 1) {
    stop('requires a single predx class name')
  }
  
  predx_classes <- c('Point', 'Binary', 'BinCat', 'BinLwr', 'Sample', 'SampleCat')
  if(!(class %in% predx_classes)) {
    stop('target class must be a valid predx class')
  }
  
  call_args <- list()
  if(class == "BinCat") {
    call_args$cat <- cat
  } else if(class == "BinLwr") {
    call_args$lwr <- lwr
  }
  
  if(is.predx(x)) {
    call_args$x <- x
    result <- do.call(
      paste("transform", class(x), "to", class, sep = "_"),
      args = call_args
    )
  } else if(is.list(x)) {
    if(!all(sapply(x, is.predx))) {
      stop('x must be a predx object or list of predx objects')
    }
    result <- lapply(x, function(x_i) {
      call_args$x <- x_i
      return(do.call(
        paste("transform", class(x_i), "to", class, sep = "_"),
        args = call_args
      ))
    })
  } else {
    stop('x must be a predx object or list of predx objects')
  }
  return(result)
}

# identity transformations return x
transform_Binary_to_Binary <-
  transform_BinCat_to_BinCat <-
  transform_BinLwr_to_BinLwr <-
  transform_Sample_to_Sample <-
  transform_SampleCat_to_SampleCat <-
  transform_Point_to_Point <- function(x, ...) {
  return(x)
}

# transformations from Binary to other classes not yet supported
transform_Binary_to_BinCat <-
  transform_Binary_to_BinLwr <-
  transform_Binary_to_Sample <-
  transform_Binary_to_SampleCat <-
  transform_Binary_to_Point <- function(x, ...) {
  warning("NAs introduced by coercion")
  return(NA)
}

# transformations from BinCat to other classes not yet supported
transform_BinCat_to_Binary <-
  transform_BinCat_to_BinLwr <-
  transform_BinCat_to_Sample <-
  transform_BinCat_to_SampleCat <-
  transform_BinCat_to_Point <- function(x, ...) {
  warning("NAs introduced by coercion")
  return(NA)
}

# transformations from BinLwr to other classes not yet supported
transform_BinLwr_to_Binary <-
  transform_BinLwr_to_BinCat <-
  transform_BinLwr_to_Sample <-
  transform_BinLwr_to_SampleCat <-
  transform_BinLwr_to_Point <- function(x, ...) {
  warning("NAs introduced by coercion")
  return(NA)
}

# transformations from Sample to BinLwr are ok, from Sample to other classes not yet supported
transform_Sample_to_BinLwr <- function(x, lwr) {
  bin_width <- lwr[2] - lwr[1]
  bins <- c(lwr, lwr[length(lwr)] + bin_width)
  bin_counts <- hist(x@predx, bins, right = FALSE, plot = FALSE)$counts
  
  binlwr_df <- data.frame(
    lwr = lwr,
    prob = bin_counts / sum(bin_counts)
  )
  return(BinLwr(binlwr_df))
}

transform_Sample_to_Binary <-
  transform_Sample_to_BinCat <-
  transform_Sample_to_SampleCat <-
  transform_Sample_to_Point <- function(x, ...) {
  warning("NAs introduced by coercion")
  return(NA)
}

# transformations from SampleCat to BinCat are ok, from SampleCat to other classes not yet supported
transform_SampleCat_to_BinCat <- function(x, cat) {
  if(!isTRUE(all.equal(cat, x@predx$cat))) {
    stop("For transformation from SampleCat to BinCat, cat argument must match x@predx$cat")
  }
  
  bincat_df <- data.frame(
    cat = cat,
    prob = sapply(cat, function(cat_i) {
      mean(x@predx$sample == cat_i)
    }),
    stringsAsFactors = FALSE
  )
  return(BinCat(bincat_df))
}

transform_SampleCat_to_Binary <-
  transform_SampleCat_to_BinLwr <-
  transform_SampleCat_to_Sample <-
  transform_SampleCat_to_Point <- function(x, ...) {
  warning("NAs introduced by coercion")
  return(NA)
}

# transformations from Point to other classes not yet supported
transform_Point_to_Binary <-
  transform_Point_to_BinCat <-
  transform_Point_to_BinLwr <-
  transform_Point_to_Sample <-
  transform_Point_to_SampleCat <- function(x, ...) {
  warning("NAs introduced by coercion")
  return(NA)
}
