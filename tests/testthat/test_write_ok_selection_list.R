# library(OKplan)
library(NVIdb)
library(testthat)

test_that("write_ok_selection_list", {

  td <- tempdir()

  # Make example data
  x <- as.data.frame(cbind("ok_aar" = 2021, "rapport" = "Brucellose hos geit, utvalgsliste",
                           "mt_regionnr" = "M21000", "mt_region" = "Region Stor-Oslo",
                           "mt_avdelingnr" = "M21150", "mt_avdeling" = "Romerike",
                           "eier_lokalitetnr" = "30303030", "orgnr" = "999999999", "eier_lokalitet" = "XXX XXXXX",
                           "postnr" = "0468", "poststed" = "OSLO", "ant_prover" = 26))
  # Probably first makes a matrix, therefore "Antall prøver" is character.
  x$ant_prover <- as.numeric(x$ant_prover)

  write_ok_selection_list(data = x,
                          sheet = "ok_test_data",
                          filename = "oktest.xlsx",
                          filepath = td,
                          calculate_sum = TRUE,
                          dbsource = "ok_avlsgris")

  expect_true(checkmate::check_file_exists(file.path(td, "oktest.xlsx")))

  expect_identical(openxlsx::getSheetNames(file.path(td, "oktest.xlsx")),
                   "ok_test_data")

  y <- openxlsx::read.xlsx(xlsxFile = file.path(td, "oktest.xlsx"))
  expect_identical(colnames(y),
                   c("År", "Rapport", "MT.regionnr", "MT.region",
                     "MT.avdelingsnr", "MT.avdeling", "Produsentnr",
                     "Foretaksnr", "Virksomhet", "Postnr", "Poststed",
                     "Antall.prøver"))
  expect_identical(y[2, "Antall.prøver"], "26")
  expect_identical(y[3, 1], paste("Datauttrekket er gjort", format(Sys.Date(), "%d/%m/%Y")))

})

test_that("write_ok_selection_list, column_standards is list", {

  td <- tempdir()

  # Make example data
  x <- as.data.frame(cbind("ok_aar" = 2021, "rapport" = "Brucellose hos geit, utvalgsliste",
                           "mt_regionnr" = "M21000", "mt_region" = "Region Stor-Oslo",
                           "mt_avdelingnr" = "M21150", "mt_avdeling" = "Romerike",
                           "eier_lokalitetnr" = "30303030", "orgnr" = "999999999", "eier_lokalitet" = "XXX XXXXX",
                           "postnr" = "0468", "poststed" = "OSLO", "ant_prover" = 26))
  # Probably first makes a matrix, therefore "Antall prøver" is character.
  x$ant_prover <- as.numeric(x$ant_prover)

  write_ok_selection_list(data = x,
                          sheet = "ok_test_data",
                          filename = "oktest.xlsx",
                          filepath = td,
                          column_standards =
                            list("colname" = c("ok_aar", "rapport", "mt_regionnr", "mt_region", "mt_avdelingnr",
                                               "mt_avdeling", "eier_lokalitetnr", "orgnr", "eier_lokalitet",
                                               "postnr", "poststed",
                                               "ant_prover"),
                                 "collabel" = c("År", "Rapport", "MT regionnr", "MT region", "MT avdelingsnr",
                                                "MT avdeling", "Produsentnr", "Foretaksnr", "Virksomhet", "Postnr", "Poststed",
                                                "Antall prøver"),
                                 "colwidth" = c(5, 9, 12.5, 16, 13, 30, 12, 12, 30, 8, 15, 8.5)),
                          calculate_sum = TRUE,
                          dbsource = "ok_brucella")

  expect_true(checkmate::check_file_exists(paste0(td, "/oktest.xlsx")))

  expect_identical(openxlsx::getSheetNames(paste0(td, "/oktest.xlsx")),
                   "ok_test_data")
  y <- openxlsx::read.xlsx(xlsxFile = file.path(td, "oktest.xlsx"))
  expect_identical(colnames(y),
                   c("År", "Rapport", "MT.regionnr", "MT.region",
                     "MT.avdelingsnr", "MT.avdeling", "Produsentnr",
                     "Foretaksnr", "Virksomhet", "Postnr", "Poststed",
                     "Antall.prøver"))
  expect_identical(y[2, "Antall.prøver"], "26")
  expect_identical(y[3, 1], paste("Datauttrekket er gjort", format(Sys.Date(), "%d/%m/%Y")))

})

test_that("write_ok_selection_list, column_standards is file", {

  td <- tempdir()

  # Make example data
  x <- as.data.frame(cbind("ok_aar" = 2021, "rapport" = "Brucellose hos geit, utvalgsliste",
                           "mt_regionnr" = "M21000", "mt_region" = "Region Stor-Oslo",
                           "mt_avdelingnr" = "M21150", "mt_avdeling" = "Romerike",
                           "eier_lokalitetnr" = "30303030", "orgnr" = "999999999", "eier_lokalitet" = "XXX XXXXX",
                           "postnr" = "0468", "poststed" = "OSLO", "ant_prover" = 26))
  # Probably first makes a matrix, therefore "Antall prøver" is character.
  x$ant_prover <- as.numeric(x$ant_prover)

  write_ok_selection_list(data = x,
                          sheet = "ok_test_data",
                          filename = "oktest.xlsx",
                          filepath = td,
                          column_standards = file.path(NVIdb::set_dir_NVI("programmeringR", slash = FALSE),
                                                       "standardization", "colnames", "OK_column_standards.csv"),
                          calculate_sum = TRUE,
                          dbsource = "ok_avlsgris")

  expect_true(checkmate::check_file_exists(paste0(td, "/oktest.xlsx")))

  expect_identical(openxlsx::getSheetNames(paste0(td, "/oktest.xlsx")),
                   "ok_test_data")
  y <- openxlsx::read.xlsx(xlsxFile = file.path(td, "oktest.xlsx"))
  expect_identical(colnames(y),
                   c("År", "Rapport", "MT.regionnr", "MT.region",
                     "MT.avdelingsnr", "MT.avdeling", "Produsentnr",
                     "Foretaksnr", "Virksomhet", "Postnr", "Poststed",
                     "Antall.prøver"))
  expect_identical(y[2, "Antall.prøver"], "26")
  expect_identical(y[3, 1], paste("Datauttrekket er gjort", format(Sys.Date(), "%d/%m/%Y")))

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
