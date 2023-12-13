library(OKplan)
library(testthat)

test_that("Output public_holiday from get_holiday", {
  
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
})

test_that("Output trapped workdays from get_holiday", {
  
  # Selects workdays except the trapped days in 2000
  workdays <- get_holiday(year = 2000,
                          type = "workday",
                          exclude_trapped_days = "trapped",
                          output = "raw")
  trapped <- workdays[which(workdays$trapped == "t"), "date"]
  expect_identical(trapped,
                   as.Date(c("2000-06-02"), format = "%Y-%m-%d"))
  
  # Selects workdays except the trapped days and days off in easter and xmas
  workdays <- get_holiday(year = 2000,
                          type = "workday",
                          exclude_trapped_days = c("trapped", "easter", "xmas"),
                          output = "raw")
  trapped <- workdays[which(!is.na(workdays$trapped)), "date"]
  expect_identical(trapped,
                   as.Date(c("2000-04-17", "2000-04-18", "2000-04-19",
                             "2000-06-02", "2000-12-24", "2000-12-25", 
                             "2000-12-26", "2000-12-27", "2000-12-28", 
                             "2000-12-29", "2000-12-30", "2000-12-31"), 
                           format = "%Y-%m-%d"))
  
  # Selects workdays except the trapped days in 2024
  workdays <- get_holiday(year = 2024,
                          type = "workday",
                          exclude_trapped_days = "trapped",
                          output = "raw")
  trapped <- workdays[which(workdays$trapped == "t"), "date"]
  expect_identical(trapped,
                   as.Date(c("2024-05-10", "2024-12-27"), format = "%Y-%m-%d"))
  
  # Selects workdays except the trapped days and days off in easter and xmas
  workdays <- get_holiday(year = 2024,
                          type = "workday",
                          exclude_trapped_days = c("trapped", "easter", "xmas"),
                          output = "raw")
  trapped <- workdays[which(!is.na(workdays$trapped)), "date"]
  expect_identical(trapped,
                   as.Date(c("2024-03-25", "2024-03-26", "2024-03-27",
                             "2024-05-10", "2024-12-22", "2024-12-23", 
                             "2024-12-24", "2024-12-25", "2024-12-26", 
                             "2024-12-27", "2024-12-28", "2024-12-29", 
                             "2024-12-30", "2024-12-31"), format = "%Y-%m-%d"))
  
  # Selects workdays except the trapped days in 2025
  workdays <- get_holiday(year = 2025,
                          type = "workday",
                          exclude_trapped_days = "trapped",
                          output = "raw")
  trapped <- workdays[which(workdays$trapped == "t"), "date"]
  expect_identical(trapped,
                   as.Date(c("2025-05-02", "2025-05-30"), format = "%Y-%m-%d"))
  
  # Selects workdays except the trapped days and days off in easter and xmas
  workdays <- get_holiday(year = 2025,
                          type = "workday",
                          exclude_trapped_days = c("trapped", "easter", "xmas"),
                          output = "raw")
  trapped <- workdays[which(!is.na(workdays$trapped)), "date"]
  expect_identical(trapped,
                   as.Date(c("2025-04-14", "2025-04-15", "2025-04-16",
                             "2025-05-02", "2025-05-30", "2025-12-21", 
                             "2025-12-22", "2025-12-23", "2025-12-24", 
                             "2025-12-25", "2025-12-26", "2025-12-27", 
                             "2025-12-28", "2025-12-29", "2025-12-30", 
                             "2025-12-31"), format = "%Y-%m-%d"))
  
})


test_that("Errors for get_holiday ", {
  
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(
    get_holiday(year = 2023,
                type = "weekend",
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "Variable 'type': Must be element of set",
    fixed = TRUE)
  
  expect_error(
    get_holiday(year = 2023,
                type = c("non_workday", "public_holiday",
                         "sat_to_sun", "workday"),
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "Variable 'type': Must have length 1, but has length 4",
    fixed = TRUE)
  
  expect_error(
    get_holiday(year = "2023",
                type = c("non_workday"),
                exclude_trapped_days = "easter",
                output = "selected"),
    regexpr = "'year' failed: Must be of type 'integerish', not 'character'.",
    fixed = TRUE)
  
  expect_error(
    get_holiday(year = 2023,
                type = c("public_holiday"),
                exclude_trapped_days = "exclude",
                output = "selected"),
    regexpr = "{'easter','trapped','xmas'}, but has additional elements",
    fixed = TRUE)
  
  expect_error(
    get_holiday(year = 2023,
                type = c("sat_to_sun"),
                exclude_trapped_days = "easter",
                output = c("raw", "fhi")),
    regexpr = "argument 'pattern' has length > 1",
    fixed = TRUE)
  
  expect_error(
    get_holiday(year = 2023,
                type = c("sat_to_sun"),
                exclude_trapped_days = "easter",
                output = "csdata"),
    regexpr = "{'cstime','fhi','raw','selected'}, but is 'csdata'",
    fixed = TRUE)
  
  options(width = unlist(linewidth))
})
