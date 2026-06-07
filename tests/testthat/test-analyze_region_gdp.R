test_that("analyze_region_gdp works", {
  dat <- load_data()

  result <- analyze_region_gdp(dat, start_year = 2020, end_year = 2024)

  expect_s3_class(result, "tbl_df")

  expect_equal(
    names(result),
    c(
      "term",
      "comparison",
      "df",
      "sumsq",
      "rss",
      "f_statistic",
      "p_value",
      "conclusion"
    )
  )

  expect_equal(result$term[1], "Overall region effect")
  expect_equal(result$comparison[1], "GDP-only model vs. GDP + region model")

  expect_true(is.numeric(result$p_value))
  expect_true(result$p_value >= 0)
  expect_true(result$p_value <= 1)

  expect_true(
    result$conclusion %in% c(
      "At least one region differs",
      "No evidence that regions differ overall"
    )
  )
})
