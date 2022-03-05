#' Run GP-SMART for an input crime and input suspects
#'
#' @description
#' Calculates suspect variables relative to the input crime,
#' calculates probability of crime per suspect activity location (node) within the user's input search radius,
#' ranks suspects according to their highest probability activity node,
#' and returns ranked suspects and (optionally) their activity nodes for mapping.
#'
#' @param input_crime A data frame with 1 row and 16 columns, created by [fn_prepare_input_crime()].
#'
#' @param input_suspects A data frame with 1 row per suspect activity node and 21 columns, created by [fn_prepare_suspect_data()].
#'
#' @param search_radius The distance (in kilometres) around the input crime to search for suspect nodes. Suspects with nodes within this distance are shortlisted. The default is 10km.
#'
#' @param weights A list of 8 data frames, one per weighting variable (node attribute),
#'   specifying the weight to use for each value of each attribute for each input crime type.
#'   If `NULL`, the default, built-in weights will be used (see Details).
#'
#' @param return_node_predictions Logical. Whether to return per-node predictions for the shortlisted suspects, required as input for [fn_map_gpsmart_output()].
#'
#' @author Sophie Curtis-Ham
#'
#' @importFrom rlang .data
#' @importFrom utils data
#'
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#'
#' @return A list with four elements:
#' \describe{
#'  \item{`suspects_ranked`}{A data frame with n rows = n suspects with nodes within the `search_radius` of the input crime,
#'    ranked by their maximum predicted crime probability.}
#'  \item{`node_predictions`}{If `return_node_predictions` = `TRUE`, a data frame with n rows = n suspect nodes within search radius,
#'    including all node attributes and predicted crime probability per node;
#'    if `return_node_predictions` = `FALSE`, a character string "no node predictions".}
#'  \item{`input_crime`}{The data frame entered as input for [fn_gpsmart()].
#'    It is included in the output for use in the [fn_map_gpsmart_output()] function.}
#'  \item{`search_radius`}{The search radius (in kilometres), entered as input for [fn_gpsmart()].
#'    It is included in the output for use in the [fn_map_gpsmart_output()] function.}
#' }
#'
#' @export
#'
#' @details
#' \itemize{
#'  \item{In calculating the probability of crime per suspect activity node,
#'   the function weights the node (adjusts the probability) based on its values across 7 attributes (weighting variables).
#'   The weighting variables are: frequency, recency, duration, behaviour similarity, location type similarity, daypart similarity, weekpart similarity and season similarity.
#'   The default (built in) weights were estimated using a New Zealand sample of offenders and activity locations as described in Curtis-Ham et al (2022).
#'   To create your own weights, use the built in list of data frames as a template, replacing only the weight values.
#'   (use \code{gpsmartr.private:::node_weights_lookups} to access the built in weights).}
#'  \item{The function will return an error if there are no suspect nodes within the search radius.}
#'  \item{Season variables are based on Southern Hemisphere.}
#' }
#'
#' @seealso
#' * [fn_prepare_input_crime()] checks that the minimum necessary variables are present and creates the `input_crime` data frame for use in this function.
#' * [fn_prepare_suspect_data()] checks that the minimum necessary variables are present and creates the `input_suspects` data frame for use in this function.
#' * [fn_map_gpsmart_output()] creates an interactive map visualising the output of this function.
#'
#' @references
#' Curtis-Ham S., Bernasco, W., Medvedev, O. N., & Polaschek, D. L. L (2022).
#'   'A new geographic profiling method for mapping and ranking suspects in crime investigations: GP-SMART'.
#'   Journal of Investigative Psychology and Offender Profiling. https://doi.org/10.1002/jip.1585
#'
#' @examples
#' \dontrun{
#' data(example_input_crime_raw)
#' data(example_input_suspects_raw)
#' fn_gpsmart(
#'   input_crime = fn_prepare_input_crime(example_input_crime_raw),
#'   input_suspects = fn_prepare_suspect_data(example_input_suspects_raw),
#'   search_radius = 10,
#'   return_node_predictions = FALSE
#' )
#' }
#'
fn_gpsmart <- function(input_crime, input_suspects, search_radius = 10, weights = NULL, return_node_predictions = FALSE) {

  `.` <- list # this defines "." so that the package check doesn't give a note: no visible global function definition for '.'

  # define these variables locally so that the package doesn't give a note: no visible binding for global variable
  all_weights <- autumn <- beh_sim <- daypart <- daypart_sim <- death_date <- dist <- NULL
  duration_cont <- event_date_time_random <- event_end_date <- event_start_date <- NULL
  frequency_cont <- location_type <- loc_sim <- max_event_random_date <- NULL
  max_predicted_probability <- max_span_end_date <- min_event_random_date <- NULL
  min_event_start_date <- min_span_start_date <- n_event_dates <- node_category <- NULL
  node_type <- person_id <- pred_inv_dist <- predicted_probability <- prior <- NULL
  prior_odds_invdist <- prior_offence_type <- recency_diff <- season_sim <- NULL
  span_end_date <- span_start_date <- spring <- summer <- weekpart <- weekpart_sim <- NULL
  weight_value_beh_sim <- weight_value_daypart_sim <- weight_value_duration <- NULL
  weight_value_frequency <- weight_value_loc_sim <- weight_value_recency <- NULL
  weight_value_season_sim <- weight_value_weekpart_sim <- winter <- x <- y <- NULL

  # Step 1. Filter to pre-crime activity nodes within search radius ---------------

  node_shortlist <- data.table::setDT(input_suspects) %>%
    # filter to pre-crime nodes
    dplyr::mutate(prior = ifelse(.data$node_type == "police_other" &
      .data$event_start_date < input_crime$start_date_ic[[1]], TRUE, # treat other police contact nodes like span nodes for establishing they are prior
    ifelse(.data$node_category == "event" &
      .data$event_end_date < input_crime$start_date_ic[[1]], TRUE, # event node end date has to be before input crime start date
    ifelse(.data$node_category == "span" &
      .data$span_start_date < input_crime$start_date_ic[[1]], TRUE, # span node (home, family home, school, work) start has to be before input crime start date
    FALSE
    )
    )
    )) %>%
    .[prior == TRUE, , ] %>%
    # filter to alive at input crime date
    .[is.na(death_date) | death_date > input_crime$end_date_ic[[1]], , ] %>%
    # calculate distance to test offence
    .[, dist := sqrt((x - input_crime$x_ic[[1]])^2 + (y - input_crime$y_ic[[1]])^2) / 1000, ] %>%
    # filter to nodes within search radius of the input crime
    .[dist < search_radius, , ]

  # end the function with an error if there are no suspects with nodes in the search radius
  if (nrow(node_shortlist) == 0) {
    stop(paste0("For case id ", input_crime$case_id_ic[[1]], " no suspects with nodes in search radius."))
  }

  # Step 2. Calculate node variables relative to input crime ----------------

  suppressWarnings(

  node_shortlist_relative_to_crime <- node_shortlist %>%

    # calculate behaviour, location type and timing similarity

    .[, `:=`(
      beh_sim = ifelse(is.na(prior_offence_type), 0,
        ifelse(input_crime$offence_type_ic[[1]] == prior_offence_type, 1, 0)
      ),
      loc_sim = ifelse(is.na(location_type) | is.na(input_crime$location_type_ic[[1]]), 0,
        ifelse(location_type == input_crime$location_type_ic[[1]], 1, 0)
      ),
      daypart_sim = ifelse(is.na(daypart), 0, # NAs for the other police contacts with no day part go in the not similar category at this point
        ifelse(daypart == "all", 1,
          ifelse(daypart == input_crime$daypart_ic[[1]], 1, 0)
        )
      ),
      weekpart_sim = ifelse(weekpart == input_crime$weekpart_ic[[1]], 1,
        ifelse(weekpart == "both", 1, 0)
      ),
      season_sim = ifelse(spring == TRUE & input_crime$spring_ic[[1]] == TRUE, 1,
        ifelse(summer == TRUE & input_crime$summer_ic[[1]] == TRUE, 1,
          ifelse(autumn == TRUE & input_crime$autumn_ic[[1]] == TRUE, 1,
            ifelse(winter == TRUE & input_crime$winter_ic[[1]] == TRUE, 1, 0)
          )
        )
      )
    ), ] %>%

    # take min of start/event date and max of end/event date and similarity values per activity node per node type

    .[, .(
      min_span_start_date = min(span_start_date, na.rm = TRUE),
      max_span_end_date = max(span_end_date, na.rm = TRUE),
      min_event_random_date = lubridate::ymd(stringr::str_sub(as.character(min(event_date_time_random, na.rm = TRUE)), start = 1, end = 10)), # convert to just date not date-time
      max_event_random_date = lubridate::ymd(stringr::str_sub(as.character(max(event_date_time_random, na.rm = TRUE)), start = 1, end = 10)),
      min_event_start_date = min(event_start_date, na.rm = TRUE),
      max_event_end_date = max(event_end_date, na.rm = TRUE),
      beh_sim = max(beh_sim, na.rm = TRUE),
      loc_sim = max(loc_sim, na.rm = TRUE),
      daypart_sim = max(daypart_sim, na.rm = TRUE),
      weekpart_sim = max(weekpart_sim, na.rm = TRUE),
      season_sim = max(season_sim, na.rm = TRUE),
      n_event_dates = dplyr::n_distinct(lubridate::ymd(stringr::str_sub(as.character(event_date_time_random), start = 1, end = 10))) # n_dates is needed for frequency_rate calculation for events
    ), by = .(person_id, x, y, node_category, node_type, dist)] %>%

    # calculate frequency, recency and duration relative to input crime and convert to categorical variables

    # recency
    .[, `:=`(recency_diff = ifelse(
      node_category == "span",
      ifelse(input_crime$start_date_ic[[1]] <= max_span_end_date, 1L, # if input crime start date is on or before end of span, recency = 1
        (input_crime$start_date_ic[[1]] - max_span_end_date) + 1L
      ), # if input crime start date is after end of span, recency = difference in days + 1
      ifelse(input_crime$start_date_ic[[1]] <= max_event_random_date, 1L, # for police_other nodes the random date could be on or after input crime start date, in which case recency = 1
        (input_crime$start_date_ic[[1]] - max_event_random_date) + 1L
      ) # for other events all end dates are by definition prior to input crime start date, but use random date for accuracy
    )), ] %>%
    .[, `:=`(
      recency = cut(recency_diff, breaks = c(0, 2, 30, 365, 1826, 36500), labels = c(
        "rec_1to2d", "rec_3to30d", "rec_1to12m", "rec_1to5y", "rec_over5y"
      ), ordered_result = TRUE),

      # duration
      duration_cont = ifelse(
        node_category == "span",
        ifelse(input_crime$start_date_ic[[1]] <= max_span_end_date, # if input crime start date is on or before end of span,
          (input_crime$start_date_ic[[1]] - min_span_start_date) + 1L, # duration is between node start and input crime start date + 1 to avoid 0 durations
          (max_span_end_date - min_span_start_date) + 1L
        ), # if input crime start date is after end of span, duration is between node start and end dates + 1 to avoid 0 durations
        ifelse(input_crime$start_date_ic[[1]] <= max_event_random_date, # for police_other nodes the end date could be on or after input crime start date,
          (input_crime$start_date_ic[[1]] - min_event_start_date) + 1L, # in which case duration is between node start and input crime + 1 to avoid 0 durations
          (max_event_random_date - min_event_random_date) + 1L
        ) # for other events all end dates are by definition prior to input crime start date, but use random dates for accuracy
      )
    ), ] %>%
    .[, `:=`(
      duration = cut(duration_cont, breaks = c(0, 2, 30, 365, 1826, 36500), labels = c(
        "dur_1to2d", "dur_3to30d", "dur_1to12m", "dur_1to5y", "dur_over5y"
      ), ordered_result = TRUE),

      # frequency
      frequency_cont = ifelse(
        node_category == "event", n_event_dates / duration_cont, # in this instance, treat police_other nodes as one-off events so as not to overestimate frequency
        ifelse(node_type == "home", 1, # daily
          ifelse(stringr::str_detect(node_type, rebus::or("family", "partner", "relative")), 26 / 365, # fortnightly, based on survey data
            ifelse(node_type == "work", 240 / 365, # 52 - 4 weeks annual leave = 48 weeks x 5 days a week = 240 days a year
              200 / 365
            )
          )
        )
      ) # school should be the only other option remaining, 52 - 12 weeks holidays = 40 weeks x 5 days a week = 200 days a year
    ), ] %>%
    .[, `:=`(
      frequency = cut(frequency_cont, breaks = c(0, 0.03, 0.14, 1), labels = c("freq_yearly", "freq_monthly", "freq_weekly"), ordered_result = TRUE)
    ), ] %>%
    # adjust distance and add offence for join to the node weights lookups
    .[, `:=`(
      dist = ifelse(dist == 0, 0.001, dist), # offset 0 distances by 1m so that there's no zero distances
      offence = input_crime$offence_subtype_ic[[1]] # this is enables joining to the node weights lookup by offence
    ), ]
  )   # Without this wrapper this step will produce a series of warnings, which can be ignored, reflecting the change of NAs to Inf.


  # Step 3. Predict crime probability per node -------------------------------

  # join the node weights to the node data

  if (is.null(weights)) {
     node_weights_lookups <- node_weights_lookups # use default node weights lookups if argument is NULL in the function call
  }
  else {
    node_weights_lookups <- weights # assign the weights entered by the user
  }

  weight_variables_list <- c("frequency", "recency", "duration", "beh_sim", "loc_sim", "daypart_sim", "weekpart_sim", "season_sim")

  for (nth_weight_variable in 1:length(weight_variables_list)) {
    node_shortlist_relative_to_crime <- node_shortlist_relative_to_crime %>%
      data.table::merge.data.table(
        y = node_weights_lookups[[nth_weight_variable]],
        by = c("offence", weight_variables_list[[nth_weight_variable]]),
        all.x = TRUE
      )
  }

  # calculate predicted crime probability per node
  node_shortlist_predictions <- node_shortlist_relative_to_crime %>%
    .[, `:=`(pred_inv_dist = 1 / ((dist + 0.001) * 1000)), ] %>% # take reciprocal/inverse of the distance for base probability; have to further offset distance by 1m to prevent NaN odds from 1/0
    .[, `:=`(prior_odds_invdist = pred_inv_dist / (1 - pred_inv_dist)), ] %>% # convert to odds
    .[, `:=`(all_weights = weight_value_frequency * weight_value_recency * weight_value_duration *
      weight_value_beh_sim * weight_value_loc_sim *
      weight_value_daypart_sim * weight_value_weekpart_sim * weight_value_season_sim), ] %>% # multiply all the weights (odds ratios) together
    .[, `:=`(predicted_probability = ((all_weights) * prior_odds_invdist) /
      (((all_weights) * prior_odds_invdist) + 1)), ] %>% # update the base odds by the combined weights (odds ratios) and convert to probability
    dplyr::select(
      -.data$pred_inv_dist, -.data$prior_odds_invdist, -.data$all_weights,
      -.data$n_event_dates, -.data$recency_diff, -.data$duration_cont, -.data$frequency_cont
    ) # %>% # remove extraneous columns


  # Step 4. Rank suspects by maximum crime probability per suspect ----------

  # take maximum predicted probability per suspect
  suspect_max_predictions <- node_shortlist_predictions %>%
    data.table::setDT() %>%
    .[, .(number_of_nodes = .N, max_predicted_probability = max(predicted_probability, na.rm = TRUE)), by = person_id]

  # rank suspects
  set.seed(99) # set seed for tie breaking randomly consistently
  suspects_ranked <- suspect_max_predictions %>%
    .[, `:=`(rank = rank(-max_predicted_probability, ties.method = "random")), ] %>%
    dplyr::arrange(.data$rank)

  # join the ranks back to the node predictions for sorting
  node_shortlist_predictions_output <- node_shortlist_predictions %>%
    # join ranks to node predictions
    data.table::merge.data.table(
      y = suspects_ranked[, .(person_id, suspect_rank = rank)],
      by = "person_id",
      all.x = TRUE
    ) %>%
    # sort the suspects by rank then node predicted probability
    dplyr::arrange(.data$suspect_rank, dplyr::desc(.data$predicted_probability))


  # Step 5. Create object to return from function ----------

  if (return_node_predictions == TRUE) {
    gpsmart_output <- list(
      "suspects_ranked" = data.frame(suspects_ranked),
      "node_predictions" = node_shortlist_predictions_output,
      "input_crime" = input_crime,
      "search_radius" = search_radius
    )
  } else {
    gpsmart_output <- list(
      "suspects_ranked" = data.frame(suspects_ranked),
      "node_predictions" = "no node predictions",
      "input_crime" = input_crime,
      "search_radius" = search_radius
    )
  }

  return(gpsmart_output)
}
