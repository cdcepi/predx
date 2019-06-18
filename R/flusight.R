#' Tools for working with FluSight forecasts
#'
#'These functions are used to convert an original FluSight-formatted csv file to \code{predx} data frame.
#'
#' @param file A csv file formatted for the FluSight forecasting challenge.
#'
#' @return A \code{predx} data frame.
#'
#' @export
#' @importFrom magrittr '%>%'
#'
#' @examples
#' csv_tempfile <- tempfile('EW42-Hist-Avg-2018-10-29', fileext='.csv')
#' write.csv(flusightdemo, csv_tempfile, row.names=F)
#' import_flusight_csv(csv_tempfile)
#' @name flusight

#' @export
#' @rdname flusight
import_flusight_csv <- function(file) {
  team <- stringr::str_extract(file,
    "(?<=EW\\d{1,2}-).*(?=-\\d{4}-\\d{2}-\\d{2})")
  mmwr_week <- stringr::str_extract(file, "(?<=EW)\\d{1,2}")
  submission_date <- stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}")
  read.csv(file, stringsAsFactors=F) %>%
    dplyr::mutate(
      team = team,
      mmwr_week = mmwr_week,
      submission_date = submission_date
    ) %>%
    prep_flusight() %>%
    import_csv()
}

#' @export
#' @rdname flusight
export_flusight_csv <- function(x, dir=NULL, overwrite=F) {
  team <- unique(x$team)
  mmwr_week <- unique(x$mmwr_week)
  submission_date <- unique(x$submission_date)
  if (length(team) != 1 | length(mmwr_week) != 1 | length(submission_date) != 1) {
    stop(paste('x contains more than one team, mmwr_week, or submission_date.',
      'These variables are not included in the FluSight template'))
  }
  forecast_name <- paste0('EW', mmwr_week, '_', team, '_', as.character(submission_date))
  x <- as.predx_df(x) %>%
    export_csv() %>%
    dplyr::mutate(
      type = ifelse(predx_class %in% c('BinLwr', 'BinCat'), 'Bin', NA),
      type = ifelse(predx_class == 'Point', 'Point', type),
      bin_start_incl = ifelse(unit == 'percent' & type == 'Bin', lwr, NA),
      bin_start_incl = ifelse(unit == 'week' & type == 'Bin', cat, bin_start_incl),
      bin_end_notincl = ifelse(unit == 'percent' & type == 'Bin', lwr + 0.1, NA),
      bin_end_notincl = ifelse(unit == 'percent' & type == 'Bin' & lwr == 13, 100, bin_end_notincl),
      bin_end_notincl = ifelse(unit == 'week' & type == 'Bin',
        recode_flusight_bin_end_notincl(cat), bin_end_notincl),
      value = ifelse(predx_class %in% c('BinLwr', 'BinCat'), prob, NA),
      value = ifelse(predx_class %in% c('Point'), point, value)
      ) %>%
    dplyr::select(location, target, type, unit, bin_start_incl, bin_end_notincl, value)
  if (!is.null(dir)) {
    filename <- paste0(stringr::str_remove('dir', '/$'), '/', forecast_name, '.csv')
    if (!overwrite & file.exists(filename)) {
      stop(paste0('"', filename, '" already exists. Use "overwrite = T" to replace.'))
    } else {
      write.csv(x, filename, row.names = F)
    }
  } else {
    return(x)
  }
}

#' @export
#' @rdname flusight
to_flusight_pkg_format <- function(x) {
  mmwr_week <- as.numeric(x$mmwr_week[1])
  x <- export_flusight_csv(x) %>%
    dplyr::mutate(
      bin_start_incl = ifelse(stringr::str_detect(bin_start_incl, '^\\d+$'),
        paste0(bin_start_incl, '.0'), bin_start_incl),
      forecast_week = mmwr_week
    )
  return(x)
}

#' @rdname flusight
prep_flusight <- function(x) {
  names(x) <- tolower(names(x))
  # assign appropriate predx classes
  dplyr::mutate(x,
    predx_class = NA,
    predx_class = ifelse(type == 'Point',
      'Point', predx_class),
    predx_class = ifelse(type == 'Bin' &
      target %in% c('Season onset', 'Season peak week'),
      'BinCat', predx_class),
    predx_class = ifelse(type == 'Bin' &
      target %in% c('Season peak percentage',
      '1 wk ahead', '2 wk ahead', '3 wk ahead', '4 wk ahead'),
      'BinLwr', predx_class)
    ) %>%
  # create variables needed for each component
  dplyr::mutate(
    point = ifelse(predx_class == 'Point', value, NA),
    prob = ifelse(predx_class %in% c('BinCat', 'BinLwr'), value, NA),
    cat = ifelse(predx_class == 'BinCat', bin_start_incl, NA),
    lwr = ifelse(predx_class == 'BinLwr', bin_start_incl, NA)
    ) %>%
  # normalize probabilities where needed
  dplyr::group_by(location, target, predx_class) %>%
  dplyr::mutate(sum_prob = sum(prob)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    prob = ifelse(!is.na(prob) & 0.9 <= sum_prob & sum_prob <= 1.1,
      prob / sum_prob, prob)
    ) %>%
  dplyr::select(-sum_prob, -type, -bin_start_incl, -bin_end_notincl, -value)
}

#' @export
#' @rdname flusight
flusight_ilinet_expected <- function() {
  list(
    list(
      target = c("Season peak percentage", "1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead"),
      location = c("HHS Region 1", "HHS Region 10", "HHS Region 2", "HHS Region 3",
        "HHS Region 4", "HHS Region 5", "HHS Region 6", "HHS Region 7",
        "HHS Region 8", "HHS Region 9", "US National"),
      predx_class = c("Point", "BinLwr")
    ),
    list(
      target = c("Season onset", "Season peak week"),
      location = c("HHS Region 1", "HHS Region 10", "HHS Region 2", "HHS Region 3",
        "HHS Region 4", "HHS Region 5", "HHS Region 6", "HHS Region 7",
        "HHS Region 8", "HHS Region 9", "US National"),
      predx_class = c("Point", "BinCat")
    )
  )
}

#' @export
#' @rdname flusight
flusight_state_ilinet_expected <- function() {
  list(
    list(
      target = c("Season peak percentage", "1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead"),
      location = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
          "Connecticut", "Delaware", "District of Columbia", "Georgia",
          "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
          "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
          "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
          "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
          "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
          "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina",
          "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virgin Islands",
          "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
          ),
      predx_class = c("Point", "BinLwr")
    ),
    list(
      target = c("Season peak week"),
      location = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
          "Connecticut", "Delaware", "District of Columbia", "Georgia",
          "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
          "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
          "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
          "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
          "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
          "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina",
          "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virgin Islands",
          "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
          ),
      predx_class = c("Point", "BinCat")
    )
  )
}

#' @export
#' @rdname flusight
flusight_hospitalization_expected <- function() {
    list(
    list(
      target = c("Season peak percentage", "1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead"),
      location = c("Overall", "0-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", "65+ yr"),
      predx_class = c("Point", "BinLwr")
    ),
    list(
      target = c("Season peak week"),
      location = c("Overall", "0-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", "65+ yr"),
      predx_class = c("Point", "BinCat")
    )
  )
}

#' @rdname flusight
recode_flusight_bin_end_notincl <- function(x) {
  dplyr::recode(x,
    `40` = "41",
    `41` = "42",
    `42` = "43",
    `43` = "44",
    `44` = "45",
    `45` = "46",
    `46` = "47",
    `47` = "48",
    `48` = "49",
    `49` = "50",
    `50` = "51",
    `51` = "52",
    `52` = "53",
    `1` = "2",
    `2` = "3",
    `3` = "4",
    `4` = "5",
    `5` = "6",
    `6` = "7",
    `7` = "8",
    `8` = "9",
    `9` = "10",
    `10` = "11",
    `11` = "12",
    `12` = "13",
    `13` = "14",
    `14` = "15",
    `15` = "16",
    `16` = "17",
    `17` = "18",
    `18` = "19",
    `19` = "20",
    `20` = "21",
    none = "none"
  )
}

