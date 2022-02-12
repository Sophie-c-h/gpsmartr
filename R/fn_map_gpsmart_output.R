#' Map the output of GP-SMART for an input crime and shortlisted suspects
#'
#' @description
#' Creates an interactive map showing shortlisted suspects' activity locations (nodes) within the user's set search radius of an input crime.
#' Use [htmltools::save_html()] to save the output map as an html file.
#'
#' @param gpsmart_output A list with four elements, returned by running the function [fn_gpsmart()] with `return_node_predictions` set to `TRUE`:
#' \describe{
#'  \item{`suspects_ranked`}{A data frame with n rows = n suspects with nodes within the `search_radius` of the input crime,
#'    ranked by their maximum predicted crime probability.}
#'  \item{`node_predictions`}{A data frame with n rows = n suspect nodes within `search_radius`,
#'     including all node attributes and predicted crime probability per node.}
#'  \item{`input_crime`}{A data frame with 1 row and 16 columns created by [fn_prepare_input_crime()],
#'      entered as input for [fn_gpsmart()].}
#'  \item{`search_radius`}{The search radius (in kilometres), entered as input for [fn_gpsmart()].}
#' }
#'
#' @param coordinate_system CRS number for the coordinate system for the X and Y coordinates of the input crime and suspect node data.
#'   The default is New Zealand Transverse Mercator (2193).
#'
#' @author Sophie Curtis-Ham
#'
#' @importFrom rlang .data
#' @importFrom utils data
#'
#' @return An interactive map object that can be opened in the RStudio Viewer or an internet browser and saved as an html file for sharing.
#'   The map displays:
#' \itemize{
#'  \item{the location of the input crime (IC);}
#'  \item{the activity nodes of all shortlisted suspects with nodes within the `search_radius` when running [fn_gpsmart()], coloured by their predicted probability (as a percentile);}
#'  \item{the search radius.}
#'  }
#'   The user can:
#' \itemize{
#'  \item{pan and zoom to specific locations;}
#'  \item{search (enter text or use drop-down menu) to filter nodes to a particular suspect(s) based on their identifier;}
#'  \item{use a slider to limit the displayed nodes to those of the top x ranked suspects;}
#'  \item{use tick boxes to limit the displayed nodes according to `node_type` and other node attributes used in [fn_gpsmart()] to calculate the predicted probability;}
#'  \item{hover over a node to show the rank of the suspect;}
#'  \item{click on a node to show a popup box with the suspect's rank and identifier, the `node_type`, and the node's values on the GP-SMART variables;}
#'  \item{search for an address or place name.}
#'  }
#'
#' @export
#'
#' @details
#' \itemize{
#'  \item{The function will return an error if there are no node predictions in `gpsmart_output` to map.}
#'  \item{Requires an internet connection.}
#'  }
#'
#' @seealso
#' * [fn_gpsmart()] creates the input object for this function.
#'
#' @examples
#' \dontrun{
#' data(example_input_crime_raw)
#' data(example_input_suspects_raw)
#' gpsmart_output <- fn_gpsmart(
#'   input_crime = fn_prepare_input_crime(example_input_crime_raw),
#'   input_suspects = fn_prepare_suspect_data(example_input_suspects_raw),
#'   search_radius = 10,
#'   return_node_predictions = FALSE
#' )
#' fn_map_gpsmart_output(
#'   gpsmart_output,
#'   coordinate_system = 2193,
#'   search_radius = 10
#' )
#' }
#'
fn_map_gpsmart_output <- function(gpsmart_output, coordinate_system = 2193) {

  # check there are suspect nodes to map and end the function with an error if there are no suspect node predictions in the input
  if (class(gpsmart_output$node_predictions[[1]]) == "character") {
    stop("No suspect node predictions to map. Ensure return_node_predictions is set to TRUE when running fn_gpsmart.")
  }

  # create spatial files for input crime and suspect nodes and ensure all are in leaflet coordinate system
  input_crime_spatial <- sf::st_as_sf(gpsmart_output$input_crime, coords = c("x_ic", "y_ic"), crs = coordinate_system)
  input_crime_wgs <- input_crime_spatial %>%
    sf::st_transform(crs = 4326)
  suspect_nodes_wgs <- sf::st_as_sf(gpsmart_output$node_predictions %>%
    dplyr::mutate(
      ppp = dplyr::percent_rank(.data$predicted_probability),
      frequency = factor(.data$frequency, levels = c("freq_weekly", "freq_monthly", "freq_yearly")),
      recency = factor(.data$recency, levels = c("rec_1to2d", "rec_3to30d", "rec_1to12m", "rec_1to5y", "rec_over5y")),
      duration = factor(.data$duration, levels = c("dur_1to2d", "dur_3to30d", "dur_1to12m", "dur_1to5y", "dur_over5y"))
    ),
  coords = c("x", "y"), crs = coordinate_system
  ) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_jitter(factor = 0.0003) # adjusts the points so markers at the same coordinates do not overlap

  search_area_wgs <- sf::st_buffer(
    input_crime_spatial,
    gpsmart_output$search_radius * 1000
  ) %>%
    sf::st_transform(crs = 4326)

  # create prediction colour palette and offence icon
  pal_predprob <- leaflet::colorNumeric(
    palette = "Reds",
    domain = suspect_nodes_wgs$ppp,
    reverse = FALSE
  )
  offence_icon <- leaflet::makeAwesomeIcon(
    text = "IC",
    markerColor = "black",
    iconColor = "white"
  )

  # create map
  shared_data <- crosstalk::SharedData$new(suspect_nodes_wgs)

  output_map <- crosstalk::bscols(
    widths = c(4, 8),
    device = "lg", # wraps the slider and tickboxes when screen dimensions are portrait

    list(
      crosstalk::filter_select("person_id", "Person Identifier", sharedData = shared_data, group = ~person_id),
      crosstalk::filter_slider("suspect_rank", "Suspect Rank", sharedData = shared_data, column = ~suspect_rank, step = 1, width = "100%"),
      crosstalk::filter_checkbox("node_type", "Node Type", sharedData = shared_data, group = ~node_type, inline = TRUE),
      crosstalk::filter_checkbox("frequency", "Frequency", sharedData = shared_data, group = ~frequency, inline = TRUE),
      crosstalk::filter_checkbox("recency", "Recency", sharedData = shared_data, group = ~recency, inline = TRUE),
      crosstalk::filter_checkbox("duration", "Duraction", sharedData = shared_data, group = ~duration, inline = TRUE),
      crosstalk::filter_checkbox("beh_sim", "Same crime", sharedData = shared_data, group = ~beh_sim, inline = TRUE),
      crosstalk::filter_checkbox("loc_sim", "Same location type", sharedData = shared_data, group = ~loc_sim, inline = TRUE),
      crosstalk::filter_checkbox("daypart_sim", "Same daypart", sharedData = shared_data, group = ~daypart_sim, inline = TRUE),
      crosstalk::filter_checkbox("weekpart_sim", "Same_weekpart", sharedData = shared_data, group = ~weekpart_sim, inline = TRUE),
      crosstalk::filter_checkbox("season_sim", "Same season", sharedData = shared_data, group = ~season_sim, inline = TRUE)
    ),
    leaflet::leaflet(
      data = shared_data,
      width = "100%",
      height = 800,
      options = leaflet::leafletOptions(
        dragging = TRUE,
        minzoom = 10, # limits zoom range for user
        maxzoom = 20
      )
    ) %>%
      leaflet::addProviderTiles(provider = "CartoDB", group = "CartoDB") %>% # opens a specified basemap, group is needed for layer control later
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
      leaflet::addProviderTiles(provider = "Esri", group = "Esri") %>%
      leaflet::addScaleBar(
        position = "bottomleft",
        options = leaflet::scaleBarOptions(
          maxWidth = 100,
          metric = TRUE,
          updateWhenIdle = TRUE
        )
      ) %>%
      leaflet::addLayersControl(baseGroups = c("CartoDB", "OSM", "Esri"), position = "topleft") %>% # lets users toggle on and off different basemaps
      leaflet.extras::addSearchOSM() %>% # adds a magnifying glass icon that user can click on to search for an address/place name
      leaflet.extras::addResetMapButton() %>%
      leaflet::addPolygons(
        data = search_area_wgs,
        fillOpacity = 0,
        color = "black",
        weight = 2
      ) %>%
      leaflet::addAwesomeMarkers(
        data = input_crime_wgs,
        icon = offence_icon
      ) %>%
      leaflet::addCircleMarkers(
        color = ~ pal_predprob(ppp),
        radius = 4,
        opacity = 1,
        weight = 1.5,
        label = ~suspect_rank,
        popup = ~ paste0(
          "Rank: ", suspect_rank,
          "<br>ID:", person_id,
          "<br>PPP: ", round(ppp, 3),
          "<br>Node: ", node_type,
          "<br>Frequency: ", frequency,
          "<br>Recency: ", recency,
          "<br>Duration: ", duration,
          "<br>Same crime: ", beh_sim,
          "<br>Same location type: ", loc_sim,
          "<br>Same daypart: ", daypart_sim,
          "<br>Same weekpart: ", weekpart_sim,
          "<br>Same season: ", season_sim
        )
      ) %>%
      leaflet::addLegend(
        title = "Predicted <br> probability <br> percentile",
        position = "bottomright",
        pal = pal_predprob,
        values = ~ppp
      )
  )

  return(output_map)
}
