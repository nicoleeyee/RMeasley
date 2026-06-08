test_that("gdp_summary_table works", {
  dat <- load_data()

  result <- gdp_summary_table(dat, start_year = 2020, end_year = 2024)

  expect_s3_class(result, "tbl_df")

  expect_equal(
    names(result),
    c(
      "region",
      "Low GDP",
      "Lower-Middle GDP",
      "Middle GDP",
      "Upper-Middle GDP",
      "High GDP"
    )
  )

  expect_true("Africa" %in% result$region)

  expected_africa_low <- dat |>
    dplyr::filter(year >= 2020, year <= 2024) |>
    dplyr::mutate(
      gdp_group = dplyr::ntile(gdp_per_capita, 5),
      gdp_group = dplyr::recode(
        as.character(gdp_group),
        "1" = "Low GDP",
        "2" = "Lower-Middle GDP",
        "3" = "Middle GDP",
        "4" = "Upper-Middle GDP",
        "5" = "High GDP"
      )
    ) |>
    dplyr::filter(region == "Africa", gdp_group == "Low GDP") |>
    dplyr::summarise(
      value = median(
        measles_incidence_rate_per_1000000_total_population,
        na.rm = TRUE
      )
    ) |>
    dplyr::pull(value)

  actual_africa_low <- result |>
    dplyr::filter(region == "Africa") |>
    dplyr::pull(`Low GDP`)

  expect_equal(actual_africa_low, expected_africa_low)
})
