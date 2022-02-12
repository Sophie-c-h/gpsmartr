Sys.setenv("R_TESTS" = "")

# load the example data
data("example_input_crime_raw")
data("example_input_suspects_raw")

# select a random sample to test ensuring representation of:
# - example input crime offender
# - records of each node_type for other suspects
# - but this time we don't need records of each level of each reliability and relevance factor, to check that the function copes without all levels being present
set.seed(99)
input_suspects <-  example_input_suspects_raw %>%
  dplyr::group_by(node_type) %>%
  dplyr::slice_sample(n=100)  %>%
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

output_defaults <- fn_gpsmart(input_crime = fn_prepare_input_crime(example_input_crime_raw),
                                            input_suspects = input_suspects)

# run tests
test_that("output no error", {

  expect_error(fn_map_gpsmart_output(output_default_weights_node_preds), NA)
})

test_that("output no warning", {

  expect_warning(fn_map_gpsmart_output(output_default_weights_node_preds), NA)

})

test_that("error if no node predictions in input", {

  expect_error(fn_map_gpsmart_output(output_defaults),
               "No suspect node predictions to map",
               fixed = TRUE)

})

# run snapshot comparison to prompt user to accept changes as intended
test_that("output matches expected output for test input using defaults", {

  expect_snapshot_output(fn_map_gpsmart_output(output_default_weights_node_preds))

})

