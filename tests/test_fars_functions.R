library(testthat)
library(fars)

## Testing fars_read function
test_that("fars_read - Load file", {
  df_fars_file <- fars_read("accident_2013.csv.bz2")
  expect_that(df_fars_file, is_a(c("tbl_df", "tbl", "data.frame")))
  })

test_that("fars_read - Error Message", {
  throws_error(fars_read("accident_2013.csv.bz2"), paste0("file ", system.file("extdata", package = "fars"), "/accidents_2013.csv.bz2 does not exist"))
})

## Testing make_filename function
test_that("make_filename", {
  file_name <- make_filename(2013)
  expect_that(file_name, is_a("character"))
})

test_that("make_filename - Number as string", {
  file_name <- make_filename("2013")
  expect_that(file_name, is_a("character"))
})

test_that("make_filename - String", {
  file_name <- make_filename("abc")
  expect_that(file_name, is_a("character"))
})

test_that("make_filename - warning", {
    gives_warning(make_filename("abc"), "In make_filename('abc') : NAs introduced by coercion")
})

## Testing fars_read_years function
test_that("fars_read_years - 1 year", {
  months_list <- fars_read_years(2013)
  expect_that(months_list, is_a("list"))
})

test_that("fars_read_years - 3 years", {
  months_list <- fars_read_years(c(2013, 2014, 2015))
  expect_that(months_list, is_a("list"))
})

test_that("fars_read_years - 1 year - number of items", {
  months_list <- fars_read_years(2013)
  expect_that(length(months_list), equals(1))
})

test_that("fars_read_years - 3 years - number of items", {
  months_list <- fars_read_years(c(2013, 2014, 2015))
  expect_that(length(months_list), equals(3))
})

test_that("fars_read_years - 1 year - number of rows (item 1)", {
  months_list <- fars_read_years(2013)
  expect_that(nrow(months_list[[1]]), equals(30202))
})

test_that("fars_read_years - Invalid year", {
  gives_warning(months_list(3020), "In value[[3L]](cond) : invalid year: 3020")
})

## Testing fars_summarize_years function
test_that("fars_summarize_years - Summarizing 1 year data (2013)", {
  df_file_summarized <- fars_summarize_years(2013)
  expect_that(df_file_summarized, is_a(c("tbl_df", "tbl", "data.frame")))
})

test_that("fars_summarize_years - 1 year - number of rows", {
  df_file_summarized <- fars_summarize_years(2013)
  expect_that(nrow(df_file_summarized), equals(12))
})

test_that("fars_summarize_years - 1 year - number of collumns", {
  df_file_summarized <- fars_summarize_years(2013)
  expect_that(ncol(df_file_summarized), equals(2))
})

test_that("fars_summarize_years - Summarizing 3 years data (2013, 2014 and 2015)", {
  df_file_summarized <- fars_summarize_years(c(2013, 2014, 2015))
  expect_that(df_file_summarized, is_a(c("tbl_df", "tbl", "data.frame")))
})

test_that("fars_summarize_years - 3 years - number of rows", {
  df_file_summarized <- fars_summarize_years(c(2013, 2014, 2015))
  expect_that(nrow(df_file_summarized), equals(12))
})

test_that("fars_summarize_years - 3 years - number of collumns", {
  df_file_summarized <- fars_summarize_years(c(2013, 2014, 2015))
  expect_that(ncol(df_file_summarized), equals(4))
})

test_that("fars_summarize_years - 1 year - warning message check", {
  gives_warning(fars_summarize_years(3020), "In value[[3L]](cond) : invalid year: 3020")
})

test_that("fars_read - Error Message check", {
  throws_error(fars_summarize_years(3020), "Column `year` is unknown")
})

## Testing fars_map_state function
test_that("fars_map_state - Invalid State", {
  throws_error(fars_map_state(20000, 2013), "Error in fars_map_state(2000, 2013) : invalid STATE number: 2000")
})

test_that("fars_map_state - Invalid year", {
  throws_error(fars_map_state(1, 2012), "file 'accident_2012.csv.bz2' does not exist")
})

test_that("fars_map_state - Nothing to plot", {
  throws_error(fars_map_state(2, 2013), "nothing to draw: all regions out of bounds")
})
