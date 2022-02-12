#' Prepare suspect data for GP-SMART
#'
#' @description
#' Checks that the minimum necessary variables are present and calculates additional attributes needed for [fn_gpsmart()].
#'
#' @param input_suspects_raw A data frame with at least one row and 14 columns:
#' \describe{
#'  \item{`person_id`}{A unique reference number for the suspect. Can be a character or numeric vector.}
#'  \item{`death_date`}{Suspect's date of death if deceased. A date in format "YYYY-MM-DD"}
#'  \item{`x`}{Easting coordinate. Must be in metres to enable distance calculations.}
#'  \item{`y`}{Northing coordinate. Must be in metres to enable distance calculations.}
#'  \item{`node_category`}{The general category of the activity location. A character vector with values "span" or "event". Span and event nodes are treated differently when calculating activity node attributes.}
#'  \item{`node_type`}{The type of activity location. A factor with levels "home", "family_immediate", "family_ip", "family_other", "school", "work", "offence", "victim_witness", "incident", "police_other". Node type determines some activity node attribute values.}
#'  \item{`prior_offence_type`}{The type of offence, if it's an offence activity location and one of the crime types for which GP-SMART is currently calibrated. A factor with levels "burglary", "robbery" or "sex", being the crimes GP-SMART is calibrated for. Used for calculating behaviour similarity.}
#'  \item{`span_start_date`}{Start date of the activity location, if a span node. A date in format "YYYY-MM-DD".}
#'  \item{`span_end_date`}{End date of the activity location, if an event node. A date in format "YYYY-MM-DD".}
#'  \item{`event_start_date`}{Start date of the event, if an event node. A date in format "YYYY-MM-DD".}
#'  \item{`event_end_date`}{End date of the event, if an event node. A date in format "YYYY-MM-DD".}
#'  \item{`event_start_time`}{Start time of the event, if an event node. A difftime in format "HH:MM:SS". Time is not present for "police_other" event nodes in the package data.}
#'  \item{`event_end_time`}{End time of the event, if an event node. A difftime in format "HH:MM:SS". Time is not present for "police_other" event nodes in the package data.}
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
#'  \item{`event_date_time_random`}{Random date-time generated for event nodes.}
#'  \item{`daypart`}{The day part of the node. A factor with levels "daytime", "evening", "night" or "all".}
#'  \item{`weekpart`}{The week part of the activity node. A factor with levels "weekday", "weekend" or "both".}
#'  \item{`spring`}{Whether the node dates include any days in spring (1 = yes, 0 = no)}
#'  \item{`summer`}{Whether the node dates include any days in summer (1 = yes, 0 = no)}
#'  \item{`autumn`}{Whether the node dates include any days in autumn (1 = yes, 0 = no)}
#'  \item{`winter`}{Whether the node dates include any days in winter (1 = yes, 0 = no)}
#' }
#' @export
#'
#' @details
#' \itemize{
#'  \item {The function will return an error if the input does not contain the correct columns in the correct format.}
#'  \item {Season variables are based on Southern Hemisphere.}
#'  }
#'
#' @seealso
#'  * [fn_prepare_input_crime()] checks that the minimum necessary variables are present and creates the `input_crime` data frame for use in [fn_gpsmart()].
#'  * [fn_gpsmart()] filters and ranks `input_suspects` based on their probability of committing the `input_crime`.
#'
#' @examples
#' \dontrun{
#' data(example_input_suspects_raw)
#' fn_prepare_supect_data(example_input_suspects_raw)
#' }
#'
fn_prepare_suspect_data <- function(input_suspects_raw) {

  # check minimum variables needed in raw data are present

  n_problem_cols <- 14 - sum(unlist(lapply(
    names(input_suspects_raw),
    function(x) {
      x %in% c(
        "person_id", "death_date", "x", "y", "node_category", "node_type", "prior_offence_type",
        "span_start_date", "span_end_date", "event_start_date", "event_end_date", "event_start_time",
        "event_end_time", "location_type"
      )
    }
  )))

  if (n_problem_cols > 0) {
    # error if minimum variables not present
    stop(paste0(n_problem_cols, " incorrect columns or column names in input_suspects_raw. Check there are 14 columns; see ?fn_prepare_suspect_data for expected names."))
  }

  # calculate variables

  #  daypart (from rand_datetime or span), weekpart (from rand_datetime or span), season (from rand_datetime or span, dummied)

  set.seed(99) # set seeed for replicable random dates and times
  input_suspects_for_gpsmart <- input_suspects_raw %>%
    dplyr::mutate(

      # fill in blank end date and end time
      event_end_date = dplyr::case_when(is.na(.data$event_end_date) ~ .data$event_start_date, TRUE ~ .data$event_end_date),
      event_end_time = dplyr::case_when(is.na(.data$event_end_time) ~ .data$event_start_time, TRUE ~ .data$event_end_time),

      # fill in blank start and end time for event nodes with no time, using midnight as a default, to enable random date-time generation
      temp_event_start_time = ifelse(.data$node_category == "event" & is.na(.data$event_start_time),
        "00:00:00",
        as.character(hms::as_hms(.data$event_start_time))
      ),
      temp_event_end_time = ifelse(.data$node_category == "event" & is.na(.data$event_end_time),
        "00:00:00",
        as.character(hms::as_hms(.data$event_end_time))
      ),

      # create start and end date-time
      temp_event_start_date_time = lubridate::ymd_hms(ifelse(.data$node_category == "event",
        paste(
          as.character(.data$event_start_date),
          .data$temp_event_start_time # as.character(hms::as_hms(.data$event_start_time))
        ),
        NA
      )),
      temp_event_end_date_time = lubridate::ymd_hms(ifelse(.data$node_category == "event",
        paste(
          as.character(.data$event_end_date),
          .data$temp_event_end_time
        ),
        NA
      )),

      # create random date-time (this is the most reliable way of managing event nodes where the exact date and time is not known)
      event_date_time_random = .data$temp_event_end_date_time - (.data$temp_event_end_date_time - .data$temp_event_start_date_time) * stats::runif(length(.data$temp_event_start_date_time)),

      # calculate hour for daypart
      temp_hour = ifelse(.data$node_category == "event", lubridate::hour(.data$event_date_time_random), NA),

      # calculate day for weekpart
      temp_day = ifelse(.data$node_category == "event", lubridate::wday(.data$event_date_time_random, week_start = 1), NA)
    ) %>%
    # calculate list of all dates in the span for span nodes of less than 10 months (anything 10 months or more must include all seasons)
    dplyr::rowwise() %>%
    dplyr::mutate(
      temp_span_dates = ifelse(.data$node_category == "event", NA,
        ifelse(.data$span_end_date - .data$span_start_date < 300,
          list(seq(.data$span_start_date, .data$span_end_date, by = "days")), NA
        )
      )
    ) %>%
    # calculate list of all seasons in the span for span nodes of less than a year
    dplyr::rowwise() %>%
    dplyr::mutate(
      temp_span_months = toString(unique(stringr::str_sub(as.character(.data$temp_span_dates), start = 6, end = 7))),
      temp_event_month = stringr::str_sub(as.character(.data$event_date_time_random), start = 6, end = 7),


      # calculate daypart from hour of offences and incident event nodes, fixed assumptions for span nodes, and leave NA for 'other police contact' nodes
      daypart = factor(ifelse(.data$node_type == "police_other", NA_character_,
        ifelse(.data$node_type %in% c("work", "school"), "daytime",
          ifelse(.data$node_category == "span", "all",
            ifelse(.data$temp_hour %in% c(7:18), "daytime", ifelse(.data$temp_hour %in% c(19:22), "evening", "night"))
          )
        )
      )),

      # calculate weekpart from random day of event nodes, fixed assumptions for span nodes
      weekpart = factor(ifelse(.data$node_type %in% c("work", "school"), "weekday",
        ifelse(.data$node_category == "span", "both",
          ifelse(.data$temp_day >= 0 & .data$temp_day <= 4, "weekday", "weekend")
        )
      )),

      # calculate season variables from random date of event nodes and span of span node
      spring = ifelse(.data$node_category == "event" & .data$temp_event_month %in% c("09", "10", "11"), 1,
        ifelse(.data$node_category == "event", 0,
          ifelse(.data$span_end_date - .data$span_start_date >= 300, 1,
            ifelse(.data$node_category == "span" & stringr::str_detect(.data$temp_span_months, rebus::or("9", "10", "11")), 1, 0)
          )
        )
      ),
      summer = ifelse(.data$node_category == "event" & .data$temp_event_month %in% c("12", "01", "02"), 1,
        ifelse(.data$node_category == "event", 0,
          ifelse(.data$span_end_date - .data$span_start_date >= 300, 1,
            ifelse(.data$node_category == "span" & stringr::str_detect(.data$temp_span_months, rebus::or("12", "1", "2")), 1, 0)
          )
        )
      ),
      autumn = ifelse(.data$node_category == "event" & .data$temp_event_month %in% c("03", "04", "05"), 1,
        ifelse(.data$node_category == "event", 0,
          ifelse(.data$span_end_date - .data$span_start_date >= 300, 1,
            ifelse(.data$node_category == "span" & stringr::str_detect(.data$temp_span_months, rebus::or("3", "4", "5")), 1, 0)
          )
        )
      ),
      winter = ifelse(.data$node_category == "event" & .data$temp_event_month %in% c("06", "07", "08"), 1,
        ifelse(.data$node_category == "event", 0,
          ifelse(.data$span_end_date - .data$span_start_date >= 300, 1,
            ifelse(.data$node_category == "span" & stringr::str_detect(.data$temp_span_months, rebus::or("6", "7", "8")), 1, 0)
          )
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    # remove temprary variables
    dplyr::select(-dplyr::contains("temp"))

  return(input_suspects_for_gpsmart)
}

