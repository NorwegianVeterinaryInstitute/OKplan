# library(OKplan)
library(testthat)


test_that("Errors for read_okplan", {
  linewidth <- options("width")
  options(width = 80)

  x <- data.frame(list("ok_hensiktkode" = "02001",
                       "ok_driftsformkode" = "10101",
                       "statuskode" = 1,
                       "prioritet_av_reserve" = 1,
                       "eier_lokalitetnr" = "4601012345"))

  # Wrong data
  expect_error(save_okplan(data = "x",
                           filename = "okplan_xxxxxx.csv",
                           filepath = tempdir(),
                           sortvar = c("ok_hensiktkode", "ok_driftsformkode",
                                       "statuskode")),
               regexp = "Variable 'data': Must be of type 'data.frame'")

  # Wrong filename
  expect_error(save_okplan(data = x,
                           filename = 1,
                           filepath = tempdir(),
                           sortvar = c("ok_hensiktkode", "ok_driftsformkode",
                                       "statuskode")),
               regexp = "Variable 'filename': Must be of type 'string', not")

  # Wrong filepath
  expect_error(save_okplan(data = "x",
                           filename = "okplan_xxxxxx.csv",
                           filepath = "tempdir",
                           sortvar = c("ok_hensiktkode", "ok_driftsformkode",
                                       "statuskode")),
               regexp = "Assertion on 'filepath' failed: Directory 'tempdir' does not exist.")

  # Wrong sortvar
  expect_error(save_okplan(data = x,
                           filename = "okplan_xxxxxx.csv",
                           filepath = tempdir(),
                           sortvar = c("ok_hensikt", "ok_driftsform",
                                       "status")),
               regexp = "Variable 'sortvar': Must be a subset of")

  options(width = unlist(linewidth))
})
