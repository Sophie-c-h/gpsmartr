
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpsmartr

<!-- badges: start -->
<!-- badges: end -->

The goal of gpsmartr is to illustrate the geographic profiling method
described in Curtis-Ham et al (2022), the Geographic Profiling: Suspect
Mapping and Ranking Technique (GP-SMART). The goal of GP-SMART is to
support geographic profiling in police investigations, by mapping and
ranking suspects for an input crime, based on the location and
attributes of the suspects’ activity locations (nodes). Users should
read that paper before using gpsmartr. Its operational use is at the
user’s own risk; accuracy outside the test data used to develop the
method (as described in the paper) is not guaranteed.

The functions in gpsmartr require the user to have a) an input crime and
b) a dataset of suspect activity nodes. These files need to include
specific variables describing the location, time and other attributes of
both the input crime and suspect activity nodes. Examples of both files
are provided as package data.

The functions should be run in the order shown in the example below.
They prepare the input data, run the GP-SMART process, then map the
output.

Reference

Curtis-Ham S., Bernasco, W., Medvedev, O. N., & Polaschek, D. L. L
(2022). ‘A new geographic profiling method for mapping and ranking
suspects in crime investigations: GP-SMART’. Journal of Investigative
Psychology and Offender Profiling. <https://doi.org/10.1002/jip.1585>

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Sophie-c-h/gpsmartr")
```

## Example

This example steps through the gpsmartr workflow:

First prepare the input crime and input suspect activity location (node)
data. These functions will return an error if the input datasets do not
include the necessary columns in the necessary format. This example uses
the built in package data, which is fictional data that approximates the
real crime and suspect data used to develop and test GP-SMART.

``` r
library(gpsmartr)

data(example_input_crime_raw)
data(example_input_suspects_raw)
input_crime <- fn_prepare_input_crime(example_input_crime_raw)
input_suspects <- fn_prepare_suspect_data(example_input_suspects_raw)
```

Then run the GP-SMART process to generate a shortlist of ranked suspects
for the input crime, based on the suspects’ activity locations.

``` r
gpsmart_output <- fn_gpsmart(
   input_crime = input_crime,
   input_suspects = input_suspects,
   search_radius = 10,
   return_node_predictions = TRUE # set this to false if not wanting to map the output (next step)
 )
```

Then map the output to explore it interactively.

``` r
map <- fn_map_gpsmart_output(
   gpsmart_output,
   coordinate_system = 2193
 )
```
