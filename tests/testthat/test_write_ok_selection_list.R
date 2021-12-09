library(OKplan)
library(testthat)

test_that("write_ok_selection_list", {

  td <- tempdir()

  # Make example data
  x <- as.data.frame(cbind("ok_aar" = 2021, "rapport" = "Brucellose hos geit, utvalgsliste",
                           "mt_regionnr" = "M21000", "mt_ region" = "Region Stor-Oslo",
                           "mt_avdelingnr" = "M21150", "mt_avdeling" = "Romerike",
                           "eier_lokalitetnr" = "30303030", "eier_lokalitet" = "XXX XXXXX",
                           "postnr" = "0468", "poststed" = "OSLO", "ant_prover" = 26))
  # Probably first makes a matrix, therefore "Antall prøver" is character.
  x$ant_prover <- as.numeric(x$ant_prover)

  write_ok_selection_list(data = x,
                          sheet = "ok_test_data",
                          filename = "oktest.xlsx",
                          filepath = td,
                          calculate_sum = TRUE,
                          dbsource = "ok_avlsgris")

  expect_true(checkmate::check_file_exists(paste0(td, "/oktest.xlsx")))

  expect_identical(openxlsx::getSheetNames(paste0(td, "/oktest.xlsx")),
                   "ok_test_data")

  # testwb <- openxlsx::loadWorkbook(xlsxFile = paste0(td, "/oktest.xlsx"))

})


test_that("Errors for write_ok_selection_list", {
  td <- tempdir()

  # Make example data
  x <- as.data.frame(cbind("ok_aar" = 2021, "rapport" = "Brucellose hos geit, utvalgsliste",
                           "mt_regionnr" = "M21000", "mt_ region" = "Region Stor-Oslo",
                           "mt_avdelingnr" = "M21150", "mt_avdeling" = "Romerike",
                           "eier_lokalitetnr" = "30303030", "eier_lokalitet" = "XXX XXXXX",
                           "postnr" = "0468", "poststed" = "OSLO", "ant_prover" = 26))
  # Probably first makes a matrix, therefore "Antall prøver" is character.
  x$ant_prover <- as.numeric(x$ant_prover)


  # x is not a data frame
  expect_error(write_ok_selection_list(data = "x",
                                       sheet = "avlsgris",
                                       filename = "oktest.xlsx",
                                       filepath = td,
                                       calculate_sum = TRUE,
                                       dbsource = "ok_avlsgris"),
               regexp = "Variable \'data\': Must be of type \'data.frame\'")

  # Wrong sheet name
  expect_error(write_ok_selection_list(data = x,
                                       sheet = "",
                                       filename = "oktest.xlsx",
                                       filepath = td,
                                       calculate_sum = TRUE,
                                       dbsource = "ok_avlsgris"),
               regexp = "Variable \'sheet\': All elements must have at least 1 characters")

  # Wrong path
  expect_error(write_ok_selection_list(data = x,
                                       sheet = "avlsgris",
                                       filename = "oktest.xlsx",
                                       filepath = paste0(td, "/wrongpath"),
                                       calculate_sum = TRUE,
                                       dbsource = "ok_avlsgris"),
               regexp = "Variable \'filepath\': Directory")

  # Wrong dbsource
  expect_error(write_ok_selection_list(data = x,
                                       sheet = "avlsgris",
                                       filename = "oktest.xlsx",
                                       filepath = td,
                                       calculate_sum = TRUE,
                                       dbsource = "x_avlsgris"),
               regexp = "Variable \'dbsource\': Must be element of set")

})
