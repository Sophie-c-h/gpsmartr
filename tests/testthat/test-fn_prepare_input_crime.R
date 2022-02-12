Sys.setenv("R_TESTS" = "")

test_that("fn_prepare_input_crime works", {

# load the example data
data("example_input_crime_raw")

# create test output
output <- fn_prepare_input_crime(example_input_crime_raw)

# run tests

  # no NAs in the output
  expect_false(any(is.na(output[1,])))

  # number of columns
  expect_equal(ncol(output), 16)

  # column names
  n_problem_cols <- 16 - sum(unlist(lapply(
    names(output),
    function(x) {
      x %in% c("case_id_ic", "offence_type_ic" , "x_ic", "y_ic", "start_date_ic", "end_date_ic", "start_time_ic",  "end_time_ic" , "location_type_ic",
               "offence_subtype_ic", "daypart_ic", "weekpart_ic", "spring_ic", "summer_ic", "autumn_ic", "winter_ic")
    })))
  expect_equal(n_problem_cols, 0)

  # error if input wrong column names
  expect_error(fn_prepare_input_crime(example_input_crime_raw[,1:5]))

  # run snapshot comparison to prompt user to accept changes as intended
  expect_snapshot_output(output)

})

