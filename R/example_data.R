#' @title Example input crime data
#'
#' @description
#' A dataset containing an example input crime (a burglary), in raw form.
#' It includes the minimum information needed for the function [fn_prepare_input_crime()] to operate.
#' This example is fictional; the location, time, case id, and location type are made up,
#' but are typical of the crimes in the data used in Curtis-Ham et al (2022) to calibrate and test GP-SMART.
#'
#' @name example_input_crime_raw
#'
#' @docType data
#'
#' @format A data frame of 1 row and 9 columns:
#' \describe{
#'  \item{`case_id`}{A unique reference number for the input crime. Can be a character or numeric vector.}
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
#' @references
#' Curtis-Ham S., Bernasco, W., Medvedev, O. N., & Polaschek, D. L. L (2022).
#'   'A new geographic profiling method for mapping and ranking suspects in crime investigations: GP-SMART'.
#'   Journal of Investigative Psychology and Offender Profiling. https://doi.org/10.1002/jip.1585
#'
#' @usage data(example_input_crime_raw)
#'
#' @source Based on crime data provided by New Zealand Police
"example_input_crime_raw"

#' @title Example input suspect data
#'
#' @description
#' A dataset containing an example set of suspect activity locations (nodes), in raw form.
#' It includes the minimum information needed for the function [fn_prepare_suspect_data()] to operate.
#' The example data are fictional; they were simulated based on the suspect activity location data used in Curtis-Ham et al (2022) to calibrate and test GP-SMART.
#' The example data reflect the general location and distribution of the original data (in space, in time, per node type and per suspect).
#' They do not represent the exact locations and times of suspects' activity locations in the original data.
#' The simulation process meant that node and location types do not necessarily match the map location of the nodes.
#' For example, a node with location type "residential" might fall on a commercial premises.
#' In the original data, nodes with location type "residential" fall on residential premises.
#'
#' @name example_input_suspects_raw
#'
#' @docType data
#'
#' @format A data frame of 14826 row and 14 columns:
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
#'  \item{`event_start_time`}{Start time of the event, if an event node. A difftime in format "HH:MM:SS". Time is not present for "police_other" event nodes.}
#'  \item{`event_end_time`}{End time of the event, if an event node. A difftime in format "HH:MM:SS". Time is not present for "police_other" event nodes.}
#'  \item{`location_type`}{The type of location in which the crime was committed. A factor with levels "residential", "commercial", "public", "street" or "unknown".}
#' }
#'
#' @references
#' Curtis-Ham S., Bernasco, W., Medvedev, O. N., & Polaschek, D. L. L (2022).
#'   'A new geographic profiling method for mapping and ranking suspects in crime investigations: GP-SMART'.
#'   Journal of Investigative Psychology and Offender Profiling. https://doi.org/10.1002/jip.1585
#'
#' @usage data(example_input_suspects_raw)
#'
#' @source Based on Offender activity location data provided by New Zealand Police.
"example_input_suspects_raw"


