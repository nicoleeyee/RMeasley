test_that("plot_gdp_cases_map returns leaflet maps and validates inputs", {

  dat <- load_data()

  map_default <- plot_gdp_cases_map(dat)

  map_year <- plot_gdp_cases_map(
    dat,
    year_chosen = 2024
  )

  map_top_n <- plot_gdp_cases_map(
    dat,
    top_n = 10
  )

  expect_s3_class(map_default, "leaflet")
  expect_s3_class(map_year, "leaflet")
  expect_s3_class(map_top_n, "leaflet")

  expect_error(
    plot_gdp_cases_map(
      dat,
      top_n = "cat"
    ),
    "Please enter a valid number of countries to check"
  )

  expect_error(
    plot_gdp_cases_map(
      dat,
      year_chosen = "dog"
    ),
    "Please enter an integer as a numeric value from 2012 to 2024"
  )

})
