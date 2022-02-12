#' Prepare input crime for GP-SMART
#'
#' @description
#' Checks that the minimum necessary variables are present and calculates additional attributes needed for [fn_gpsmart()].
#'
#' @param input_crime_raw A data frame with 1 row and 9 columns:
#' \describe{
#'  \item{`case_id`}{A unique reference number for the input crime. Can be a character or numeric vector. Used to name gp-smart outputs.}
#'  \item{`offence_type`}{The type of input crime. A factor with levels "burglary", "robbery" or "sex", being the crimes GP-SMART is calibrated for use with.}
#'  \item{`x`}{Easting coordinate. Must be in metres to enable distance calculations.}
#'  \item{`y`}{Northing coordinate. Must be in metres to enable distance calculations.}
#'  \item{`start_date`}{Start date of the crime. A date in format "YYYY-MM-DD".}
#'  \item{`end_date`}{End date of the crime, if the exact date is not known. A date in format "YYYY-MM-DD".}
#'  \item{`start_time`}{Start time of the crime. A difftime in format "HH:MM:SS".}
#'  \item{`end_time`}{End time of the crime, if the exact time is not known. A difftime in format "HH:MM:SS".}
#'  \item{`location_type`}{The type of location in which the crime was committed. A factor with levels "residential", "commercial", "public", "street" or "unknown".}
#' }
#'
#' @author Sophie Curtis-Ham
#'
#' @importFrom rlang .data
#' @importFrom dplyr case_when
#'
#' @return A data frame the same as the input with 7 additional columns:
#' \itemize{
#'  \item{`offence_subtype_ic`}{The subtype of input crime, abbreviated (resburg, nonresburg, comrob, persrob, sex).}
#'  \item{`daypart_ic`}{The day part of the input crime (daytime, evening, night).}
#'  \item{`weekpart_ic`}{The week part of the input crime (Weekday, weekend).}
#'  \item{`spring_ic`}{Whether the input crime occurred in spring (1 = yes, 0 = no).}
#'  \item{`summer_ic`}{Whether the input crime occurred in summer (1 = yes, 0 = no).}
#'  \item{`autumn_ic`}{Whether the input crime occurred in autumn (1 = yes, 0 = no).}
#'  \item{`winter_ic`}{Whether the input crime occurred in winter (1 = yes, 0 = no).}
#' }
#'
#' @export
#'
#' @details
#' \itemize{
#'  \item{The function will return an error if the input does not contain the correct columns in the correct format.}
#'  \item{Season variables are based on Southern Hemisphere.}
#'  }
#'
#' @seealso
#' * [fn_prepare_suspect_data()] checks that the minimum necessary variables are present and creates the `input_suspects` data frame for use in [fn_gpsmart()].
#' * [fn_gpsmart()] filters and ranks `input_suspects` based on their probability of committing the `input_crime`.
#'
#' @examples
#' data(example_input_crime_raw)
#' fn_prepare_input_crime(example_input_crime_raw)
#'
fn_prepare_input_crime <- function(input_crime_raw) {

  # check minimum variables needed in raw data are present

  n_problem_cols <- 9 - sum(unlist(lapply(
    names(input_crime_raw),
    function(x) {
      x %in% c("case_id", "offence_type", "x", "y", "start_date", "end_date", "start_time", "end_time", "location_type")
    }
  )))

  if (n_problem_cols > 0) {
    # error if minimum variables not present
    stop(paste0(n_problem_cols, " incorrect columns or column names in input_crime_raw. Check there are 9 columns; see ?fn_prepare_input_crime for expected names."))
  }

  # calculate variables

  set.seed(99) # set seeed for replicable random dates and times
  input_crime_for_gpsmart <- input_crime_raw %>%
    dplyr::mutate(
      # offence_subtype
      offence_subtype = factor(dplyr::case_when(
        stringr::str_detect(offence_type, "burg") & stringr::str_detect(location_type, "res") ~ "resburg",
        stringr::str_detect(offence_type, "burg") & !stringr::str_detect(location_type, "res") ~ "nonresburg",
        stringr::str_detect(offence_type, "rob") & stringr::str_detect(location_type, "com") ~ "comrob",
        stringr::str_detect(offence_type, "rob") & !stringr::str_detect(location_type, "com") ~ "persrob",
        stringr::str_detect(offence_type, "sex") ~ "sex",
        TRUE ~ NA_character_
      )),

      # fill in blank end date and end time
      end_date = dplyr::case_when(is.na(.data$end_date) ~ .data$start_date, TRUE ~ .data$end_date),
      end_time = dplyr::case_when(is.na(.data$end_time) ~ .data$start_time, TRUE ~ .data$end_time),

      # create start and end date-time
      temp_start_date_time = lubridate::ymd_hms(paste(
        as.character(.data$start_date),
        as.character(hms::as_hms(.data$start_time))
      )),
      temp_end_date_time = lubridate::ymd_hms(paste(
        as.character(.data$end_date),
        as.character(hms::as_hms(.data$end_time))
      )),

      # create random date-time (this is the most reliable way of managing crimes where the exact date and time is not known)
      temp_date_time_random = .data$temp_end_date_time - (.data$temp_end_date_time - .data$temp_start_date_time) * stats::runif(length(.data$temp_start_date_time)),

      # calculate hour for daypart
      temp_hour = lubridate::hour(.data$temp_date_time_random),

      # calculate day for weekpart
      temp_day = lubridate::wday(.data$temp_date_time_random, week_start = 1),

      # calculate daypart from hour
      daypart = factor(ifelse(.data$temp_hour %in% c(7:18), "daytime", ifelse(.data$temp_hour %in% c(19:22), "evening", "night")),
        levels = c("daytime", "evening", "night", "all")
      ),

      # calculate weekpart from day
      weekpart = factor(ifelse(.data$temp_day >= 0 & .data$temp_day <= 4, "weekday", "weekend"),
        levels = c("weekday", "weekend", "both")
      ),

      # calculate season variables
      spring = ifelse(lubridate::month(.data$temp_date_time_random) %in% c(9, 10, 11), 1, 0),
      summer = ifelse(lubridate::month(.data$temp_date_time_random) %in% c(12, 1, 2), 1, 0),
      autumn = ifelse(lubridate::month(.data$temp_date_time_random) %in% c(3, 4, 5), 1, 0),
      winter = ifelse(lubridate::month(.data$temp_date_time_random) %in% c(6, 7, 8), 1, 0)
    ) %>%
    # remove temprary variables
    dplyr::select(-dplyr::contains("temp"))

  # add column name suffix to identify these variables as relating to the input crime
  names(input_crime_for_gpsmart) <- paste0(names(input_crime_for_gpsmart), "_ic")

  return(input_crime_for_gpsmart)
}

