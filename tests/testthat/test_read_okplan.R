# library(OKplan)
library(testthat)


test_that("Errors for read_okplan", {
  linewidth <- options("width")
  options(width = 80)

  # Wrong filename
  expect_error(read_okplan(filename = 1,
                           filepath = tempdir(),
                           column_standards = OKplan::OK_column_standards,
                           dbsource = "okplan"),
               regexp = "Variable 'filename': Must be of type 'string', not 'double'.")

  # Wrong filepath
  expect_error(read_okplan(filename = "okplan_xxxxxx.csv",
                           filepath = "tempdir",
                           column_standards = OKplan::OK_column_standards,
                           dbsource = "okplan"),
               regexp = "Assertion on 'filepath' failed: Directory 'tempdir' does not exist.")

  # Wrong standards table
  expect_error(read_okplan(filename = "okplan_xxxxxx.csv",
                           filepath = tempdir(),
                           column_standards = "OK_column_standards",
                           dbsource = "okplan"),
               regexp = "Variable 'column_standards': File does not exist:")

  # Wrong dbsource
  expect_error(read_okplan(filename = "okplan_xxxxxx.csv",
                           filepath = tempdir(),
                           column_standards = OKplan::OK_column_standards,
                           dbsource = "www"),
               regexp = "Variable 'dbsource': Must be element of set")

  options(width = unlist(linewidth))
})
