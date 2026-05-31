#' Summarise measles cases for a region over time
#'
#' Produce a summary output of the proportion of measles cases that are epidemiologically linked for a specified region over time
#'
#' @param measles_data a dataframe of measles case data
#' @param region a string input of a region name
#'
#' @returns
#' @importFrom dplyr recode filter group_by summarise mutate select
#' @importFrom stringr str_to_title
#' @export
#'
#' @examples
#' epi_linked_cases(measles_data, "Africa")
epi_linked_cases <- function(measles_data, region = "Africa") {

  if(!(str_to_title({{region}})) %in% c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific")){
    warning("Please enter a valid region.")
    break
  }

  measles_data |>
    mutate(region_name = recode(region,
                         "AFR" = "Africa",
                         "AMR" = "Americas",
                         "EMR" = "Eastern Mediterranean",
                         "EUR" = "Europe",
                         "SEAR" = "South-East Asia",
                         "WPR" = "Western Pacific")) |>
    filter(region == str_to_title({{region}})) |>
      group_by(year) |>
      # get total counts for each year
      summarise(
        # measles total epidemiologically-linked and lab confirmed
        measles_epi_linked = sum(measles_epi_linked, na.rm = TRUE),
        measles_lab_confirmed = sum(measles_lab_confirmed, na.rm = TRUE),
        .groups = "drop"
      ) |>
      # modify columns by calculating proportions
      mutate(
        prop_epi_measles = measles_epi_linked /
          (measles_epi_linked + measles_lab_confirmed)
      ) |>
      select(year, prop_epi_measles)
}
