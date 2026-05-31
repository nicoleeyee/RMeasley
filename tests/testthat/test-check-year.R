test_that("check_year works", {
  test_data <- tibble::tibble(
    year = c(2020, 2021, 2024)
  )

  expect_equal(check_year(test_data, 2024), 2024)

  expect_error(
    check_year(test_data, 2019),
    "year_chosen must be a year in the data."
  )
})
