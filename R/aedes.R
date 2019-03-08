#' Tools for working with Aedes challenge forecasts
#'
#' These functions are used to convert Epidemic Prediction Initiative (EPI) Aedes Forecasting Challenge-formatted csv and JSON files to `predx_df` objects.
#'
#' @param file a csv file formatted for the EPI Aedes Forecasting Challenge
#'
#' @return
#' @export
#'
#' @examples
#' x = import_aedes_csv('vignettes/aedes_null_forecast.csv',
#'   add_vars=list(due_date='2019-03-31', team='null'))
#' @name aedes

#' @rdname aedes
import_aedes_csv <- function(file, add_vars=NULL) {
  x <- read.csv(file, stringsAsFactors=F)

  # add columns for additional variables
  if (!is.null(add_vars)) {
    for (i in 1:length(add_vars)) {
      if (names(add_vars)[i] %in% names(x)) stop(paste(names(add_vars)[i], 'column already exists'))
      if (length(add_vars[[i]]) != 1) {
        stop(paste(names(add_vars)[i]), 'should only have one argument.')
      }
      x[names(add_vars)[i]] <- add_vars[[i]][1]
    }
  }

  x <- dplyr::mutate(x, predx_class = 'Binary')
  x <- dplyr::rename(x, prob = value)
  import_csv(x)
}

#' @export
#' @name aedes
export_aedes_csv <- function(x, filename = NULL, overwrite = F) {
  x$value <- sapply(x$predx, get_predx)
  export_csv(x, filename, overwrite)
}

#' @export
#' @rdname aedes
aedes_expected <- function() {
  list(
    list(
      target = c("Ae. aegypti", "Ae. albopictus"),
      location =
        c("California-Alameda", "California-Butte", "California-Colusa",
        "California-Contra Costa", "California-Fresno", "California-Glenn",
        "California-Imperial", "California-Inyo", "California-Kern",
        "California-Kings", "California-Lake", "California-Los Angeles",
        "California-Madera", "California-Marin", "California-Merced",
        "California-Mono", "California-Monterey", "California-Napa",
        "California-Orange", "California-Placer", "California-Riverside",
        "California-Sacramento", "California-San Benito", "California-San Bernardino",
        "California-San Diego", "California-San Francisco", "California-San Joaquin",
        "California-San Luis Obispo", "California-San Mateo", "California-Santa Barbara",
        "California-Santa Clara", "California-Santa Cruz", "California-Shasta",
        "California-Solano", "California-Sonoma", "California-Stanislaus",
        "California-Sutter", "California-Tulare", "California-Ventura",
        "California-Yolo", "California-Yuba", "Connecticut-Fairfield",
        "Connecticut-New Haven", "Florida-Calhoun", "Florida-Collier",
        "Florida-Escambia", "Florida-Gadsden", "Florida-Hillsborough",
        "Florida-Holmes", "Florida-Jackson", "Florida-Jefferson", "Florida-Lee",
        "Florida-Liberty", "Florida-Madison", "Florida-Manatee", "Florida-Martin",
        "Florida-Miami-Dade", "Florida-Okaloosa", "Florida-Osceola",
        "Florida-Pasco", "Florida-Pinellas", "Florida-Polk", "Florida-Santa Rosa",
        "Florida-St. Johns", "Florida-Taylor", "Florida-Wakulla", "Florida-Walton",
        "Florida-Washington", "New Jersey-Cumberland", "New Jersey-Essex",
        "New Jersey-Mercer", "New Jersey-Monmouth", "New Jersey-Morris",
        "New Jersey-Salem", "New Jersey-Sussex", "New Jersey-Warren",
        "New York-Bronx", "New York-Kings", "New York-Nassau", "New York-New York",
        "New York-Queens", "New York-Richmond", "New York-Rockland",
        "New York-Westchester", "North Carolina-Forsyth", "North Carolina-New Hanover",
        "North Carolina-Pitt", "North Carolina-Transylvania", "North Carolina-Wake",
        "Texas-Cameron", "Texas-Hidalgo", "Texas-Tarrant", "Wisconsin-Dane",
        "Wisconsin-Milwaukee", "Wisconsin-Waukesha"),
      predx_class = "Binary"
    )
  )
}
