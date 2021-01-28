library(NVIdb)
library(OKplan)
library(testthat)
context("OK_column_standards")

test_that("Standard colwidths in Excel for OK-selections", {
  # Generate data frame with column names for table that should be exported to Excel
  # Example with selection of samples collected in herds
  df <- cbind("ok_aar" = "2021", "rapport" = "Brucellose hos geit, utvalgsliste",
              "mt_regionnr" = "M22000", "mt_region" = " Region Øst ",
              "mt_avdelingnr" = " M22110", "mt_avdeling" = " Glåmdal og Østerdal ",
              "eier_lokalitetnr" = "34343434", "eier_lokalitet" = "Gårdsbruk", "postnr" = "2560", "poststed" = " ALVDAL ",
              "ant_prover" = 30)

  # Make a vector with correct column names after translation
  correct_result <- c(5, 10.78, 12.5, 16, 13, 33, 12, 30, 8, 15, 8.5)

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_equal(standardize_columns(data = df, dbsource = "geit_brucella_utvalg",
                                       standards = OK_column_standards,
                                   property = "colwidths_Excel"),
               correct_result)

  # Generate data frame with column names for table that should be exported to Excel
  # Example with selection of samples collected at slaughterhouses
  df <- cbind("mt_regionnr" = "M25000", "mt_region" = "Region Nord",
              "mt_avdelingnr" = "M25150", "mt_avdeling" = "Finnmark",
              "eier_lokalitetnr" = "802", "eier_lokalitet" = "NORTURA SA AVD. FINNMARK/KARASJOK",
              "ant_prover" = 30)


  # Make a vector with correct column names after translation
  correct_result <- c(12.5, 16, 13, 33, 7, 35, 8.5)

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(standardize_columns(data = df,
                                       dbsource = "sau_brucella_slakteri",
                                       standards = OK_column_standards,
                                       property = "colwidths_Excel"),
                   correct_result)

})


test_that("Standard collabels for OK selections", {
  # Example with selection of samples collected in herds
  df <- cbind("aar" = "2021", "rapport" = "Paratuberkulose hos storfe, utvalgsliste",
              "mt_regionnr" = "M21000", "mt_region" = " Region Stor-Oslo",
              "mt_avdelingnr" = "M21130", "mt_avdeling" = "Østfold og Follo",
              "eier_lokalitetnr" = "3030303030", "eier_lokalitet" = "Gårdsbruk", "postnr" = "1747", "poststed" = "SKJEBERG",
              "ant_prover" = 5, "provetakingstidspunkt" = 2)

  # Make a vector with correct column names after translation
  correct_result <- c("År", "Rapport", "MT regionnr", "MT region", "MT avdelingsnr", "MT avdeling", "Produsentnr",
                      "Viksomhet", "Postnr", "Poststed", "Antall prøver", "Prøvetakingsuke")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_equal(standardize_columns(data = df,
                                   dbsource = "storfe_paratuberkulose_utvalg",
                                   standards = OK_column_standards,
                                   property = "collabels"),
               correct_result)

  # Generate column labels
  # Example with selection of samples collected at slaughterhouses
  df <- cbind("mt_regionnr" = "M25000", "mt_region" = "Region Nord",
              "mt_avdelingnr" = "M25150", "mt_avdeling" = "Finnmark",
              "eier_lokalitetnr" = "802", "eier_lokalitet" = "NORTURA SA AVD. FINNMARK/KARASJOK",
              "ant_prover" = 30)


  # Make a vector with correct column names after translation
  correct_result <- c("MT regionnr", "MT region", "MT avdelingsnr", "MT avdeling", "Autnr", "Slakteri", "Antall prøver")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(standardize_columns(data = df,
                                       dbsource = "sau_brucella_slakteri",
                                       standards = OK_column_standards,
                                       property = "collabels"),
                   correct_result)

})
