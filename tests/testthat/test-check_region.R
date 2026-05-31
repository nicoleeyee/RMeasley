test_that("check_region works", {
  expect_equal(check_region("Africa"), "Africa")
  expect_equal(check_region("africa"), "Africa")
  expect_equal(check_region("europe"), "Europe")

  expect_error(
    check_region("Not a region"),
    "Please enter a valid region."
  )
})
