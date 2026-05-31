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
plot_gdp_cases_map <- function(measles_data, year_chosen = 2024, top_n = 20) {

  year_chosen <- check_year(measles_data, year_chosen)

  top_measles <- measles_data |>
    dplyr::filter(year == year_chosen) |>
    dplyr::slice_min(
      n = top_n,
      order_by = measles_incidence_rate_per_1000000_total_population
    ) |>
    dplyr::mutate(type = "cases")

  measles_data |>
    dplyr::filter(year == year_chosen) |>
    dplyr::slice_max(
      n = top_n,
      order_by = gdp_per_capita
    ) |>
    dplyr::mutate(type = "gdp") |>
    dplyr::bind_rows(top_measles) |>
    dplyr::group_by(country, Longitude, Latitude) |>
    dplyr::summarise(
      both = dplyr::n() == 2,
      type = dplyr::if_else(both, "both", dplyr::first(type)),
      gdp = dplyr::first(gdp_per_capita),
      measles_cases = dplyr::first(measles_incidence_rate_per_1000000_total_population),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      circle_label = glue::glue(
        "{country}<br>
        GDP per capita: {scales::comma(gdp)}<br>
        Measles incidence rate: {measles_cases}"
      )
    ) |>
    leaflet::leaflet(
      options = leaflet::leafletOptions(
        minZoom = 2,
        maxBoundsViscosity = 1
      )
    ) |>
    leaflet::addTiles(
      options = leaflet::tileOptions(noWrap = TRUE)
    ) |>
    leaflet::setView(
      lng = 0,
      lat = 20,
      zoom = 2
    ) |>
    leaflet::setMaxBounds(
      lng1 = -180,
      lat1 = -60,
      lng2 = 180,
      lat2 = 85
    ) |>
    leaflet::addCircleMarkers(
      lng = ~Longitude,
      lat = ~Latitude,
      label = ~lapply(circle_label, htmltools::HTML),
      color = ~dplyr::case_when(
        type == "both" ~ "#8B3A3A",
        type == "gdp" ~ "#325D88",
        type == "cases" ~ "#968777"
      ),
      radius = 6,
      stroke = TRUE,
      fillOpacity = 1
    ) |>
    leaflet::addLegend(
      position = "bottomleft",
      colors = c("#325D88", "#968777", "#8B3A3A"),
      labels = c(
        "Highest GDP per capita",
        "Lowest measles cases per 1,000,000 population",
        "Both highest GDP per capita and lowest cases per 1,000,000 people"
      ),
      title = glue::glue("Top Countries ({year_chosen})")
    )
}
