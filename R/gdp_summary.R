#' Summarise measles incidence rates by GDP group and region
#'
#' @param measles_data A data frame of cleaned measles case and GDP data.
#' @param start_year First year to include. Defaults to 2012.
#' @param end_year Last year to include. Defaults to 2024.
#'
#' @returns A tibble with median measles incidence rates by region and GDP group.
#' @importFrom dplyr filter mutate group_by summarise select recode ntile
#' @importFrom tidyr complete pivot_wider
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' gdp_summary_table(load_data())
#' gdp_summary_table(load_data(), start_year = 2020, end_year = 2024)

gdp_summary_table <- function(measles_data, start_year = 2012, end_year = 2024) {

  measles_data |>
    dplyr::filter(
      .data$year >= start_year,
      .data$year <= end_year
    ) |>
    dplyr::mutate(
      gdp_group = dplyr::ntile(.data$gdp_per_capita, 5),
      gdp_group = dplyr::recode(
        as.character(.data$gdp_group),
        "1" = "Low GDP",
        "2" = "Lower-Middle GDP",
        "3" = "Middle GDP",
        "4" = "Upper-Middle GDP",
        "5" = "High GDP"
      ),
      gdp_group = factor(
        .data$gdp_group,
        levels = c(
          "Low GDP",
          "Lower-Middle GDP",
          "Middle GDP",
          "Upper-Middle GDP",
          "High GDP"
        )
      )
    ) |>
    dplyr::group_by(.data$region, .data$gdp_group) |>
    dplyr::summarise(
      median_incidence_rate = median(
        .data$measles_incidence_rate_per_1000000_total_population,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    tidyr::complete(
      .data$region,
      .data$gdp_group,
      fill = list(median_incidence_rate = 0)
    ) |>
    dplyr::select(region, gdp_group, median_incidence_rate) |>
    tidyr::pivot_wider(
      names_from = gdp_group,
      values_from = median_incidence_rate
    )
}
