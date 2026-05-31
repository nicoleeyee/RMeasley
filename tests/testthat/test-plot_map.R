test_that("plot_gdp_cases_map returns a leaflet map", {
  test_data <- tibble::tibble(
    year = c(2024, 2024, 2023),
    country = c("A", "B", "C"),
    Longitude = c(0, 10, 20),
    Latitude = c(0, 10, 20),
    gdp_per_capita = c(1000, 2000, 3000),
    measles_incidence_rate_per_1000000_total_population = c(5, 2, 8)
  )

  map_default <- plot_gdp_cases_map(test_data)

  map_year <- plot_gdp_cases_map(
    test_data,
    year_chosen = 2023
  )

  map_top_n <- plot_gdp_cases_map(
    test_data,
    top_n = 10
  )

  expect_s3_class(map_default, "leaflet")
  expect_s3_class(map_year, "leaflet")
  expect_s3_class(map_top_n, "leaflet")
})
