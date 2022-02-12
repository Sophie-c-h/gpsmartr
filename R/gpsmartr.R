#'
#'
#'
#' @details
#' gpsmartr.private is a collection of functions to support geographic profiling in crime investigations.
#' It operationalises GP-SMART (Geographic Profiling: Suspect Mapping and Ranking Technique),
#' the method described in Curtis-Ham et al (under review) to map and rank suspects for an input crime,
#' based on the location and attributes of the suspects' activity locations (nodes).
#' The functions require the user to have a) an input crime and b) a dataset of suspect activity nodes.
#' These files need to include specific variables describing the location, time and other attributes of both the input crime and suspect activity nodes.
#' Examples of both files are provided as package data.
#' The package includes the following functions, to be run in order:
#' * [fn_prepare_input_crime()] checks that the minimum necessary variables are present in the user's input crime file and creates the `input_crime` data frame for use in [fn_gpsmart()].
#' * [fn_prepare_suspect_data()] checks that the minimum necessary variables are present in the user's suspect activity nodes file and creates the `input_suspects` data frame for use in [fn_gpsmart()].
#' * [fn_gpsmart()] filters and ranks `input_suspects` based on their probability of committing the `input_crime`.
#' * [fn_map_gpsmart_output()] creates an interactive map visualising the output of [fn_gpsmart()].
#'
#' @references
#' Curtis-Ham S., Bernasco, W., Medvedev, O. N., & Polaschek, D. L. L (under review).
#'   'A new geographic profiling method for mapping and ranking suspects in crime investigations: GP-SMART'.

"_PACKAGE"
