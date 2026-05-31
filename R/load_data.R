#' Load data
#'
#' @returns A dataframe of measles data by month
#'
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' load_data()
load_data <- function(){
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')
}
