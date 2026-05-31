#' Plot Countries
#'
#' Produce a map of countries with lowest measles case rate and highest GDP in a given year
#'
#' @param measles_data a dataframe of measles case data
#' @param year_chosen a year from 2012 to 2024
#' @param top_n number of countries to be displayed
#'
#' @returns a Leaflet map
#' @importFrom dplyr group_by slice_min mutate bind_rows filter summarise
#' @importFrom leaflet leaflet addTiles setView setMaxBounds addCircleMarkers addLegend
#' @export
#'
#' @examples
#' plot_gdp_cases_map(measles_data, 2024, 20)
plot_gdp_cases_map <- function(measles_data, year_chosen = 2024, top_n = 20){

  if(!(year_chosen %in% 2012:2024)){
    warning("Please enter a valid year between 2012 and 2024.")
  }

  top_measles <- measles_data |>
    filter(year == {{year_chosen}})
    slice_min(n={{top_n}},
              order_by = measles_incidence_rate_per_1000000_total_population) |>
    mutate(type = "cases")

  measles_data |>
      filter(year == {{year_chosen}}) |>
      slice_max(n={{top_n}}, order_by = gdp_per_capita) |>
      mutate(type = "gdp") |>
      bind_rows(top_measles) |>
      group_by(country, Longitude, Latitude) |>
      summarise(both = n() == 2,
                type = if_else(both, "both", first(type)),
                gdp = first(gdp_per_capita),
                measles_cases = first(measles_incidence_rate_per_1000000_total_population),
                .groups = "drop") |>
      mutate(circle_label =
               glue::glue(
                 "{country}<br>
             GDP per capita: {scales::comma(gdp)}<br>
             Measles incidence rate: {measles_cases}")) |>
      leaflet(
        options = leafletOptions(
          minZoom = 2,
          maxBoundsViscosity = 1
        )
      ) |>
      addTiles(
        options = tileOptions(noWrap = TRUE)
      ) |>
      setView(
        lng = 0,
        lat = 20,
        zoom = 2
      ) |>
      setMaxBounds(
        lng1 = -180,
        lat1 = -60,
        lng2 = 180,
        lat2 = 85
      ) |>
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~lapply(circle_label, htmltools::HTML),
        color = ~case_when(
          type == "both" ~ "#8B3A3A",
          type == "gdp" ~ "#325D88",
          type == "cases" ~ "#968777"
        ),
        radius = 6,
        stroke = TRUE,
        fillOpacity = 1
        # clusterOptions = markerClusterOptions
      ) |>
      addLegend(
        position = "bottomleft",
        colors = c("#325D88", "#968777", "#8B3A3A"),
        labels = c("Highest GDP per capita",
                   "Lowest Measles Cases per 1,000,000 population",
                   "Both in highest GDP per capita and lowest cases per 1,000,000 people"),
        title = "Top Countries (2024)"
      )

}
