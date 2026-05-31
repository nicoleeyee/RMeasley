#' Summarise measles cases for a region over time
#'
#' Produce a summary output of the proportion of measles cases that are
#' epidemiologically linked for a specified region over time.
#'
#' @param measles_data A data frame of measles case data.
#' @param region A string input of a region name.
#'
#' @returns A tibble with year and proportion of epidemiologically linked measles cases.
#' @importFrom dplyr recode filter group_by summarise mutate select
#' @importFrom stringr str_to_title
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' epi_linked_cases(measles_data, "Africa")
epi_linked_cases <- function(measles_data, region = "Africa") {

  region_input <- check_region(region)

  measles_data |>
    dplyr::mutate(
      region_name = dplyr::recode(
        .data$region,
        "AFR" = "Africa",
        "AMR" = "Americas",
        "EMR" = "Eastern Mediterranean",
        "EUR" = "Europe",
        "SEAR" = "South-East Asia",
        "WPR" = "Western Pacific"
      )
    ) |>
    dplyr::filter(.data$region_name == region_input) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      measles_epi_linked = sum(.data$measles_epi_linked, na.rm = TRUE),
      measles_lab_confirmed = sum(.data$measles_lab_confirmed, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prop_epi_measles = .data$measles_epi_linked /
        (.data$measles_epi_linked + .data$measles_lab_confirmed)
    ) |>
    dplyr::select(.data$year, .data$prop_epi_measles)
}
