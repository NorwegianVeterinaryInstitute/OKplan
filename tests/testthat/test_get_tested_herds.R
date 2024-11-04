library(NVIdb)
library(OKplan)
library(testthat)
library(checkmate)


test_that("get tested herds for virus swine", {
  # skip if no connection to 'EOS' have been established
  skip_if_not(dir.exists(set_dir_NVI("EOS")))

  tested <- get_tested_herds(eos_table = "proveresultat_virusinfeksjoner",
                             year = 2022,
                             tested = FALSE)
  expect_data_frame(tested, nrow = 1065, ncol = 31)

  tested <- get_tested_herds(eos_table = "proveresultat_virusinfeksjoner",
                             year = 2022,
                             min_prover = 8,
                             tested = FALSE)
  expect_data_frame(tested, nrow = 565, ncol = 31)

  tested <- get_tested_herds(eos_table = "proveresultat_virusinfeksjoner",
                             year = 2022,
                             disease = "AD",
                             min_prover = 8,
                             tested = TRUE)
  expect_data_frame(tested, nrow = 565, ncol = 31)

})

test_that("get tested herds for scrapie", {
  # skip if no connection to 'EOS' have been established
  skip_if_not(dir.exists(set_dir_NVI("EOS")))

  tested <- get_tested_herds(eos_table = "proveresultat_scrapie",
                             year = 2022,
                             tested = FALSE)
  expect_data_frame(tested, nrow = 20623, ncol = 28)

  tested <- expect_warning(get_tested_herds(eos_table = "proveresultat_scrapie",
                                            year = 2022,
                                            min_prover = 2,
                                            tested = FALSE),
                           regexp = "The number of received samples could not be calculated")
  expect_data_frame(tested, nrow = 18481, ncol = 28)

  tested <- get_tested_herds(eos_table = "proveresultat_scrapie",
                             year = 2022,
                             species = "Sau",
                             min_prover = 2,
                             tested = TRUE)
  expect_data_frame(tested, nrow = 17915, ncol = 28)

})

test_that("get tested herds for virus in cattle", {
  # skip if no connection to 'EOS' have been established
  skip_if_not(dir.exists(set_dir_NVI("EOS")))

  tested <- get_tested_herds(eos_table = "proveresultat_bvd_ebl_ibr",
                             year = 2022,
                             tested = FALSE)
  expect_data_frame(tested, nrow = 3010, ncol = 22)

  tested <- get_tested_herds(eos_table = "proveresultat_bvd_ebl_ibr",
                             year = 2022,
                             production = "Melkeproduksjon",
                             tested = FALSE)
  expect_data_frame(tested, nrow = 1211, ncol = 22)

  tested <- get_tested_herds(eos_table = "proveresultat_bvd_ebl_ibr",
                             year = 2022,
                             min_prover = 2,
                             production = "Melkeproduksjon",
                             tested = FALSE)
  expect_data_frame(tested, nrow = 234, ncol = 22)

  tested <- get_tested_herds(eos_table = "proveresultat_bvd_ebl_ibr",
                             year = 2022,
                             disease = "ibr",
                             min_prover = 5,
                             tested = TRUE)
  expect_data_frame(tested, nrow = 577, ncol = 22)

  tested <- expect_warning(get_tested_herds(eos_table = "proveresultat_bvd_ebl_ibr",
                                            year = 2022,
                                            production = "Melk",
                                            tested = FALSE),
                           regexp = "There where no saker fulfilling the selection criterea")
  expect_data_frame(tested, nrow = 0, ncol = 18)

})

test_that("get tested herds for paratuberculosis in ruminants", {
  # skip if no connection to 'EOS' have been established
  skip_if_not(dir.exists(set_dir_NVI("EOS")))

  tested <- get_tested_herds(eos_table = "proveresultat_paratuberkulose",
                             year = 2022,
                             tested = FALSE)
  expect_data_frame(tested, nrow = 279, ncol = 18)

  tested <- get_tested_herds(eos_table = "proveresultat_paratuberkulose",
                             year = 2022,
                             min_prover = 5,
                             tested = FALSE)
  expect_data_frame(tested, nrow = 273, ncol = 18)

  tested <- expect_warning(get_tested_herds(eos_table = "proveresultat_paratuberkulose",
                                            year = 2022,
                                            species = "Alpakka",
                                            min_prover = 1,
                                            tested = TRUE),
                           regexp = "The number of tested samples were calculated using ant_prover")
  expect_data_frame(tested, nrow = 1, ncol = 18)

  tested <- expect_warning(get_tested_herds(eos_table = "proveresultat_paratuberkulose",
                                            year = 2022,
                                            species = "Alpakka",
                                            min_prover = 2,
                                            tested = TRUE),
                           regexp = "There where no saker fulfilling the selection criterea")
  expect_data_frame(tested, nrow = 0, ncol = 18)

})


test_that("Errors for get_tested_herds", {
  # skip if no connection to 'EOS' have been established
  skip_if_not(dir.exists(set_dir_NVI("EOS")))

  linewidth <- options("width")
  options(width = 80)

  expect_error(
    get_tested_herds(eos_table = "proveresultat_rubish",
                     year = 2022,
                     tested = FALSE),
    regexp = "Variable 'file.path(from_path, paste0(eos_table, ",
    fixed = TRUE)

  expect_error(
    get_tested_herds(eos_table = "proveresultat_paratuberkulose",
                     year = 1990,
                     tested = FALSE),
    regexp = "Element 1 is not >= 1995.",
    fixed = TRUE)

  expect_error(
    get_tested_herds(eos_table = "proveresultat_paratuberkulose",
                     year = 2022,
                     tested = "FALSE"),
    regexp = "Must be of type 'logical flag'",
    fixed = TRUE)

  expect_error(
    get_tested_herds(eos_table = "proveresultat_virusinfeksjoner",
                     year = 2022,
                     disease = "ebl",
                     tested = TRUE,
                     min_prover = 100),
    regexp = "You need to specify the disease",
    fixed = TRUE)

  expect_error(
    get_tested_herds(eos_table = "proveresultat_virusinfeksjoner",
                     year = 2022,
                     disease = "ad",
                     tested = TRUE,
                     min_prover = "100"),
    regexp = "Must be of type 'integerish'",
    fixed = TRUE)

  expect_error(
    get_tested_herds(eos_table = "proveresultat_scrapie",
                     year = 2022,
                     species = FALSE,
                     tested = TRUE,
                     min_prover = 1),
    regexp = "Must be of type 'character'",
    fixed = TRUE)

  expect_error(
    get_tested_herds(eos_table = "proveresultat_bvd_ebl_ibr",
                     year = 2022,
                     production = FALSE,
                     tested = TRUE,
                     min_prover = 1),
    regexp = "Must be of type 'character'",
    fixed = TRUE)


  options(width = unlist(linewidth))
})
