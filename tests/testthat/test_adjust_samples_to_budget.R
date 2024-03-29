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
                   c(30, 36, 42, 12, 6, 18, 6, 0, 0, 0))

  x2 <- adjust_samples_to_budget(data = x,
                                 budget = total_budget,
                                 sample_to_adjust = "sample",
                                 adjusted_sample = "new_sample",
                                 adjust_by = 4)
  expect_identical(x2$new_sample,
                   c(28, 34, 40, 13, 6, 22, 7, 0, 0, 0))

  x2 <- adjust_samples_to_budget(data = x,
                                 budget = total_budget,
                                 sample_to_adjust = "sample",
                                 adjusted_sample = "new_sample",
                                 adjust_by = 10)
  expect_identical(x2$new_sample,
                   c(26, 32, 46, 13, 7, 19, 7, 0, 0, 0))


  total_budget <- 130

  x2 <- adjust_samples_to_budget(data = x,
                                 budget = total_budget,
                                 sample_to_adjust = "sample",
                                 adjusted_sample = "new_sample",
                                 adjust_by = 6)
  expect_identical(x2$new_sample,
                   c(24, 29, 35, 12, 6, 18, 6, 0, 0, 0))

  x2 <- adjust_samples_to_budget(data = x,
                                 budget = total_budget,
                                 sample_to_adjust = "sample",
                                 adjusted_sample = "new_sample",
                                 adjust_by = 2)
  expect_identical(x2$new_sample,
                   c(24, 30, 34, 12, 6, 18, 6, 0, 0, 0))


  total_budget <- 60
  # Add data frame with sample number to adjust
  x <- as.data.frame(cbind(c(1:10),
                           c(rep("x", 5), rep("y", 5)),
                           c(24, 18, 6, 0, 30, 36, 12, 6, 0, 0)))
  colnames(x) <- c("id", "xy", "sample")

  x2 <- adjust_samples_to_budget(data = x,
                                 group = "xy",
                                 budget = total_budget,
                                 sample_to_adjust = "sample",
                                 adjusted_sample = "new_sample",
                                 adjust_by = 6)
  expect_identical(x2$new_sample,
                   c(18, 12, 6, 0, 24, 42, 12, 6, 0, 0))



})


test_that("Errors for adjust_sample_number", {

  linewidth <- options("width")
  options(width = 80)

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
    regexp = "Variable \'data\': Must be of type \'data.frame\'")

  expect_error(
    adjust_samples_to_budget(data = x,
                             budget = 0,
                             sample_to_adjust = "sample",
                             adjusted_sample = "new_sample",
                             adjust_by = 6),
    regexp = "Element 1 is not >= 1")

  expect_error(
    adjust_samples_to_budget(data = x,
                             budget = total_budget,
                             sample_to_adjust = "samples",
                             adjusted_sample = "new_sample",
                             adjust_by = 6),
    regexp = "Variable 'sample_to_adjust': Must be element of set {'id','sample'},",
    fixed = TRUE)

  expect_error(
    adjust_samples_to_budget(data = x,
                             budget = total_budget,
                             sample_to_adjust = "sample",
                             adjusted_sample = total_budget,
                             adjust_by = 6),
    regexp = "Variable \'adjusted_sample\': Must be of type \'character\', not")

  expect_error(
    adjust_samples_to_budget(data = x,
                             budget = total_budget,
                             sample_to_adjust = "sample",
                             adjusted_sample = "new_sample",
                             adjust_by = 0),
    regexp = "Variable \'adjust_by\': Element 1 is not >= 1")

  options(width = unlist(linewidth))
})
