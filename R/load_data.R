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
                      "cases_gdp_year.parquet",
                      package = "RMeasley")
  arrow::read_parquet(path,
                      show_col_types = FALSE)
}
