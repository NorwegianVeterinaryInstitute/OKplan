library(OKplan)
library(testthat)

test_that("Adjusting sample number", {

  total_budget <- 150
  # Add data frame with sample number to adjust
  x <- as.data.frame(cbind(c(1:10),
                           c(24, 30, 36, 12, 6, 18, 6, 0, 0, 0)))
  colnames(x) <- c("id", "sample")

  x2 <- adjust_samples_to_budget(data = x,
                                budget = total_budget,
                                sample_to_adjust = "sample",
                                adjusted_sample = "new_sample",
                                adjust_by = 6)
  expect_identical(x2$new_sample,
                   c(42, 36, 30, 18, 12, 6, 6, 0, 0, 0))

  x2 <- adjust_samples_to_budget(data = x,
                                budget = total_budget,
                                sample_to_adjust = "sample",
                                adjusted_sample = "new_sample",
                                adjust_by = 4)
  expect_identical(x2$new_sample,
                   c(40, 34, 28, 22, 13, 7, 6, 0, 0, 0))

  x2 <- adjust_samples_to_budget(data = x,
                             budget = total_budget,
                             sample_to_adjust = "sample",
                             adjusted_sample = "new_sample",
                             adjust_by = 10)
  expect_identical(x2$new_sample,
                   c(46, 32, 26, 20, 13, 7, 6, 0, 0, 0))

  total_budget <- 130

  x2 <- adjust_samples_to_budget(data = x,
                                budget = total_budget,
                                sample_to_adjust = "sample",
                                adjusted_sample = "new_sample",
                                adjust_by = 6)
  expect_identical(x2$new_sample,
                   c(35, 29, 24, 18, 12, 6, 6, 0, 0, 0))

  x2 <- adjust_samples_to_budget(data = x,
                                budget = total_budget,
                                sample_to_adjust = "sample",
                                adjusted_sample = "new_sample",
                                adjust_by = 2)
  expect_identical(x2$new_sample,
                   c(34, 30, 24, 18, 12, 6, 6, 0, 0, 0))
})


test_that("Errors for adjust_sample_number", {

  total_budget <- 150
  # Add data frame with sample number to adjust
  x <- as.data.frame(cbind(c(1:10),
                           c(24, 30, 36, 12, 6, 18, 6, 0, 0, 0)))
  colnames(x) <- c("id", "sample")

  expect_error(
    adjust_samples_to_budget(data = total_budget,
                            budget = total_budget,
                            sample_to_adjust = "sample",
                            adjusted_sample = "new_sample",
                            adjust_by = 6),
    regexpr = "Variable \'data\': Must be of type \'data.frame\'")

  expect_error(
    adjust_samples_to_budget(data = x,
                            budget = 0,
                            sample_to_adjust = "sample",
                            adjusted_sample = "new_sample",
                            adjust_by = 6),
    regexpr = "Variable \'budget\': Element 1 is not >= 1")

  expect_error(
    adjust_samples_to_budget(data = x,
                            budget = total_budget,
                            sample_to_adjust = "samples",
                            adjusted_sample = "new_sample",
                            adjust_by = 6),
    regexpr = "Variable \'sample_to_adjust\': Must be element of set {\'id\',\'sample\'}, but is \'samples\'")

  expect_error(
    adjust_samples_to_budget(data = x,
                            budget = total_budget,
                            sample_to_adjust = "sample",
                            adjusted_sample = total_budget,
                            adjust_by = 6),
    regexpr = "Variable \'adjusted_sample\': Must be of type \'character\', not \'double\'")

  expect_error(
    adjust_samples_to_budget(data = x,
                            budget = total_budget,
                            sample_to_adjust = "sample",
                            adjusted_sample = "new_sample",
                            adjust_by = 0),
    regexpr = "Variable \'adjust_by\': Element 1 is not >= 1")

})
