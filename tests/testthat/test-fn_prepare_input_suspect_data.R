Sys.setenv("R_TESTS" = "")

# load the example data
data("example_input_suspects_raw")

# select a random sample to test ensuring representation of example input crime offender and 5 records of each node_type for other suspects
set.seed(99)
input <-  example_input_suspects_raw %>%
  dplyr::group_by(node_type) %>%
  dplyr::slice_sample(n=5)  %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(
    example_input_suspects_raw %>%
      dplyr::filter(stringr::str_detect(person_id,
                                        stringr::str_sub(example_input_crime_raw$case_id[[1]], start = 1L, end = 7L)))
  )

# create test output
output <- fn_prepare_suspect_data(input)
new_cols <- setdiff(names(output),c(names(input),"event_date_time_random", "daypart"))

# run tests
test_that("output no NAs", {

  # no NAs in the newly created columns (NAs are allowed in the input and random date columns due to dates being different for event and span nodes)
  expect_false(any(is.na(output[,c(dplyr::all_of(new_cols))])))
  # n NAs equals n police_other nodes for daypart column
  expect_equal(sum(is.na(output$daypart)), sum(input$node_type == "police_other"))

})

test_that("output cols", {

  # number of columns
  expect_equal(ncol(output), 21)
  # column names
  n_problem_cols <- 21 - sum(unlist(lapply(
    names(output),
    function(x) {
      x %in% c("person_id", "death_date", "x", "y", "node_category", "node_type", "prior_offence_type", "span_start_date",
               "span_end_date", "event_start_date", "event_end_date", "event_start_time", "event_end_time", "location_type",
               "event_date_time_random", "daypart", "weekpart", "spring", "summer", "autumn", "winter")
    })))
  expect_equal(n_problem_cols, 0)
})

test_that("error if input wrong colnames", {

  expect_error(fn_prepare_input_crime(input[,1:5]))

})

# run snapshot comparison to prompt user to accept changes as intended
test_that("output matches expected output for example input", {

  expect_snapshot_output(output)

})


