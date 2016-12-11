library(testthat)

test_that("fars_read_years returns a list of tibbles", {
  res <- fars_read_years(c(2013, 2014))
  expect_that(res, is_a("list"))
  expect_that(res[[1]], is_a("tbl_df"))
})

test_that("fars_summarize_years returns a tibble with 12 rows and columns for each year", {
  res <- fars_summarize_years(c(2013, 2014))
  expect_that(res, is_a("tbl_df"))
  expect_that(names(res), equals(c("MONTH", "2013", "2014")))
  expect_that(row.names(res), equals(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")))
})
