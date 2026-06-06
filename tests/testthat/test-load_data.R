test_that("load data returns a dataframe", {
  expect_type(load_data(), "list")
})
