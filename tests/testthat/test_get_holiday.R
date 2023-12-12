library(OKplan)
library(testthat)

test_that("Output from get_holiday", {

  # Selects the public holidays
   public_holidays <- get_holiday(year = 2000,
                                  type = "public_holiday")

  expect_identical(public_holidays$date,
                   as.Date(c("2000-01-01", "2000-04-16", "2000-04-20",
                     "2000-04-21", "2000-04-23", "2000-04-24",
                     "2000-05-01", "2000-05-17", "2000-06-01",
                     "2000-06-11", "2000-06-12", "2000-12-25",
                     "2000-12-26"), format = "%Y-%m-%d"))


     public_holidays <- get_holiday(year = 2024,
                                  type = "public_holiday")
  expect_identical(public_holidays$date,
                   as.Date(c("2024-01-01", "2024-03-24", "2024-03-28",
                             "2024-03-29", "2024-03-31", "2024-04-01",
                             "2024-05-01", "2024-05-09", "2024-05-17",
                             "2024-05-19", "2024-05-20", "2024-12-25",
                             "2024-12-26"), format = "%Y-%m-%d"))

   public_holidays <- get_holiday(year = 2025,
                                  type = "public_holiday")
  expect_identical(public_holidays$date,
                   as.Date(c("2025-01-01", "2025-04-13", "2025-04-17",
                             "2025-04-18", "2025-04-20", "2025-04-21",
                             "2025-05-01", "2025-05-17", "2025-05-29",
                             "2025-06-08", "2025-06-09", "2025-12-25",
                             "2025-12-26"), format = "%Y-%m-%d"))

  # Selects workdays except the trapped days
   workdays <- get_holiday(year = 2024,
                           type = "workday",
                           exclude_trapped_days = TRUE)

  # Selects workdays except days in Easter and Christmas week
   workdays <- get_holiday(year = 2024,
                           type = "workday",
                           exclude_trapped_days = c("easter", "xmas"))


})


test_that("Errors for get_holiday ", {

  total_budget <- 150
  # Add data frame with sample number to adjust
  x <- as.data.frame(cbind(c(1:10),
                           c(24, 30, 36, 12, 6, 18, 6, 0, 0, 0)))
  colnames(x) <- c("id", "sample")

  expect_error(
    get_holiday(year = 2023,
                type = "weekend",
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "Variable \'data\': Must be of type \'data.frame\'")

  expect_error(
    get_holiday(year = 2023,
                type = c("non_workday", "public_holiday",
                            "sat_to_sun", "workday"),
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "Variable \'budget\': Element 1 is not >= 1")

  expect_error(
    get_holiday(year = 2023,
                type = c("non_workday", "public_holiday",
                         "sat_to_sun", "workday"),
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "Variable \'sample_to_adjust\': Must be element of set {\'id\',\'sample\'}, but is \'samples\'")

  expect_error(
    get_holiday(year = 2023,
                type = c("non_workday", "public_holiday",
                         "sat_to_sun", "workday"),
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "Variable \'adjusted_sample\': Must be of type \'character\', not \'double\'")

  expect_error(
    get_holiday(year = 2023,
                type = c("non_workday", "public_holiday",
                         "sat_to_sun", "workday"),
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "Variable \'adjust_by\': Element 1 is not >= 1")

})
