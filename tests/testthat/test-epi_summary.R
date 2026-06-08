test_that("epi_linked_cases_table works", {
  dat <- load_data()

  result <- epi_linked_cases_table(dat, region = "Africa")

  expected <- dat |>
    dplyr::filter(region == "Africa") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      measles_epi_linked = sum(measles_epi_linked, na.rm = TRUE),
      measles_lab_confirmed = sum(measles_lab_confirmed, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prop_epi_measles = measles_epi_linked /
        (measles_epi_linked + measles_lab_confirmed)
    ) |>
    dplyr::select(year, prop_epi_measles)

  expect_s3_class(result, "tbl_df")

  expect_equal(names(result), c("year", "prop_epi_measles"))

  expect_equal(result, expected)
})
