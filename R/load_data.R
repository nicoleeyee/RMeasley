#' Load data
#'
#' @returns A dataframe of measles data by month
#'
#' @importFrom arrow read_parquet
#' @export
#'
#' @examples
#' load_data()
load_data <- function(){


  path <- system.file("extdata",
                      "cases_gdp_year_new.parquet",
                      package = "RMeasley")
  arrow::read_parquet(path) |>
    clean_data()
}

#' Helper: Change region names ad join country coordinates
#'
#' @param data measles dataset from load_data()
#'
#' @returns A dataframe of measles data with regions renamed
#' @importFrom dplyr mutate recode left_join recode_values join_by
#' @importFrom readr read_csv
clean_data <- function(data){

  coord_path <- system.file("extdata",
                            "world-data-2023.csv",
                            package = "RMeasley")

  lat_lon <- readr::read_csv(coord_path,
                              show_col_types = FALSE) |>
    mutate(Country = recode(Country,
                           "Republic of the Congo" ~ "Congo",
                           "The Gambia" ~ "Gambia",
                           "The Bahamas" ~ "Bahamas",
                           "Czech Republic" ~ "Czechia",
                           "Republic of Ireland" ~ "Ireland",
                           .default = Country))
  data |>
    mutate(
      region = recode(region,
                      "AFRO" = "Africa",
                      "AMRO" = "Americas",
                      "EMRO" = "Eastern Mediterranean",
                      "EURO" = "Europe",
                      "SEARO" = "South-East Asia",
                      "WPRO" = "Western Pacific"
      )) |>
    left_join(lat_lon, by = join_by("country" == "Country"))
}
