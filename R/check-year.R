#' Check that a year exists in the data
#'
#' @param measles_data A data frame of measles case data.
#' @param year_chosen A year to check.
#'
#' @returns The year if it exists in the data.
#'
#' @examples
#' check_year(measles_data, 2024)

check_year <- function(measles_data, year_chosen) {
  if (!year_chosen %in% measles_data$year) {
    stop("year_chosen must be a year in the data.")
  }

  year_chosen
}
