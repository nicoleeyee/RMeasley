test_that("epi_linked_cases works", {
  test_data <- tibble::tibble(
    region = c("AFR", "AFR", "EUR", "AFR"),
    year = c(2023, 2023, 2023, 2024),
    measles_epi_linked = c(10, 20, 5, 30),
    measles_lab_confirmed = c(90, 80, 45, 70)
  )

  result <- epi_linked_cases(test_data, region = "Africa")

  # Check object type
  expect_s3_class(result, "tbl_df")

  # Check expected column names
  expect_equal(names(result), c("year", "prop_epi_measles"))

  # Check expected values
  expect_equal(result$year, c(2023, 2024))
  expect_equal(result$prop_epi_measles, c(0.15, 0.30))
})
