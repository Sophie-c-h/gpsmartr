Sys.setenv("R_TESTS" = "")

# load the example data
data("example_input_crime_raw")
data("example_input_suspects_raw")

# select a random sample to test ensuring representation of:
# - example input crime offender
# - records of each node_type for other suspects
# - records of each level of each reliability and relevance factor
set.seed(99)
input_suspects <-  example_input_suspects_raw %>%
  dplyr::group_by(node_type) %>%
  dplyr::slice_sample(n=1000)  %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(
    example_input_suspects_raw %>%
      dplyr::filter(stringr::str_detect(person_id, stringr::str_sub(example_input_crime_raw$case_id[[1]], start = 1L, end = 7L)))
  ) %>% fn_prepare_suspect_data()

# create test outputs
output_default_weights_node_preds <- fn_gpsmart(input_crime = fn_prepare_input_crime(example_input_crime_raw),
                     input_suspects = input_suspects,
                     search_radius = 10,
                     return_node_predictions = TRUE)

output_user_weights_no_node_preds <- fn_gpsmart(input_crime = fn_prepare_input_crime(example_input_crime_raw),
                                     input_suspects = input_suspects,
                                     search_radius = 10,
                                     weights = gpsmartr.private:::node_weights_lookups,
                                     return_node_predictions = FALSE)

new_cols <- setdiff(names(output_default_weights_node_preds$node_predictions),c(names(input_suspects)))

# run tests
test_that("output no NAs", {

  # no NAs in the suspect ranks
  expect_false(any(is.na(output_default_weights_node_preds$suspects_ranked)))
  expect_false(any(is.na(output_user_weights_no_node_preds$suspects_ranked)))
  # no NAs in the node predictions (new non-date columns only)
  expect_false(any(is.na(output_default_weights_node_preds$node_predictions %>%
                           dplyr::select(new_cols) %>%
                           dplyr::select(-dplyr::contains("date")))))
  # not all NAs (or Infinites) in in the new date columns
  num_rows <- nrow(output_default_weights_node_preds$node_prediction)
  expect_false(sum(is.infinite(output_default_weights_node_preds$node_predictions$min_span_start_date)) == num_rows)
  expect_false(sum(is.infinite(output_default_weights_node_preds$node_predictions$max_span_end_date)) == num_rows)
  expect_false(sum(is.infinite(output_default_weights_node_preds$node_predictions$min_event_start_date)) == num_rows)
  expect_false(sum(is.infinite(output_default_weights_node_preds$node_predictions$min_event_random_date)) == num_rows)
  expect_false(sum(is.infinite(output_default_weights_node_preds$node_predictions$max_event_random_date)) == num_rows)
  expect_false(sum(is.infinite(output_default_weights_node_preds$node_predictions$max_event_end_date)) == num_rows)
  expect_false(sum(is.na(output_default_weights_node_preds$node_predictions$min_span_start_date)) == num_rows)
  expect_false(sum(is.na(output_default_weights_node_preds$node_predictions$max_span_end_date)) == num_rows)
  expect_false(sum(is.na(output_default_weights_node_preds$node_predictions$min_event_start_date)) == num_rows)
  expect_false(sum(is.na(output_default_weights_node_preds$node_predictions$min_event_random_date)) == num_rows)
  expect_false(sum(is.na(output_default_weights_node_preds$node_predictions$max_event_random_date)) == num_rows)
  expect_false(sum(is.na(output_default_weights_node_preds$node_predictions$max_event_end_date)) == num_rows)
  })

test_that("output cols", {

  # number of columns in suspects ranked
  expect_equal(ncol(output_default_weights_node_preds$suspects_ranked), 4)
  expect_equal(ncol(output_user_weights_no_node_preds$suspects_ranked), 4)
  # column names in suspects ranked
  expected_names <- c("person_id", "number_of_nodes", "max_predicted_probability", "rank")
  expect_setequal(names(output_default_weights_node_preds$suspects_ranked), expected_names)
  expect_setequal(names(output_user_weights_no_node_preds$suspects_ranked), expected_names)

  # number of columns in node predictions
  expect_equal(ncol(output_default_weights_node_preds$node_predictions), 31)
  # column names in node predictions
  expected_names <- c("offence", "season_sim", "weekpart_sim", "daypart_sim", "loc_sim", "beh_sim", "duration",
                      "recency", "frequency", "dist", "min_span_start_date", "max_span_end_date", "min_event_random_date", "max_event_random_date",
                      "min_event_start_date", "max_event_end_date"  , "weight_value_frequency", "weight_value_recency", "weight_value_duration", "weight_value_beh_sim",
                      "weight_value_loc_sim", "weight_value_daypart_sim",  "weight_value_weekpart_sim", "weight_value_season_sim", "predicted_probability", "suspect_rank" )
  expect_setequal(setdiff(names(output_default_weights_node_preds$node_predictions),c(names(input_suspects))), expected_names)

})

test_that("output factor levels present", { # all levels should be present in the test sample

  expect_length(unique(output_default_weights_node_preds$node_predictions$frequency), 3)
  expect_length(unique(output_default_weights_node_preds$node_predictions$recency), 5)
  expect_length(unique(output_default_weights_node_preds$node_predictions$duration), 5)
  expect_setequal(unique(output_default_weights_node_preds$node_predictions$beh_sim), c(0,1))
  expect_setequal(unique(output_default_weights_node_preds$node_predictions$loc_sim), c(1,0))
  expect_setequal(unique(output_default_weights_node_preds$node_predictions$daypart_sim), c(1,0))
  expect_setequal(unique(output_default_weights_node_preds$node_predictions$weekpart_sim), c(1,0))
  expect_setequal(unique(output_default_weights_node_preds$node_predictions$season_sim), c(1,0))

})

test_that("error if no suspect nodes in search radius", {

  expect_error(fn_gpsmart(input_crime = fn_prepare_input_crime(example_input_crime_raw),
                          input_suspects = input_suspects %>%
                            # remove the example input crime offender from the suspects
                            dplyr::filter(!stringr::str_detect(person_id, stringr::str_sub(example_input_crime_raw$case_id[[1]], start = 1L, end = 7L))) %>%
                            # sample so that there are no other suspects with nodes in the radius in the test sample
                            dplyr::slice_head(n=10),
                          search_radius = 10,
                          return_node_predictions = FALSE),
               "no suspects with nodes",
               fixed = TRUE)

})

# run snapshot comparison to prompt user to accept changes as intended
test_that("output matches expected output for test input using defaults", {

  expect_snapshot_output(output_default_weights_node_preds)

})

