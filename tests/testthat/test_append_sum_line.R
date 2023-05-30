library(OKplan)
library(testthat)

test_that("Append sum in last row", {

  # Make example data
  x <- as.data.frame(cbind("\u00C5r" = 2021, "Rapport" = "Brucellose hos geit, utvalgsliste",
                           "MT regionnr" = "M21000", "MT region" = "Region Stor-Oslo",
                           "MT avdelingsnr" = "M21150", "MT avdeling" = "Romerike",
                           "Produsentnr" = "30303030", "Foretak" = "XXX XXXXX",
                           "Postnr" = "0468", "Poststed" = "OSLO", "Antall prover" = 26, "Antall prover2" = 13))
  # Probably first makes a matrix, therefore "Antall prover" is character.
  x$`Antall prover` <- as.numeric(x$`Antall prover`)
  x$`Antall prover2` <- as.numeric(x$`Antall prover2`)

  # Include row with generated date, position = "left"
  y <- append_sum_line(data = x, column = c("Antall prover"), pretext = "Sum", position = "left")

  expect_equal(y[dim(y)[1], "Poststed"], "Sum")
  expect_equal(y[dim(y)[1], "Antall prover"], "26")


  # Include row with generated date, sum two columns, position = "left"
  y <- append_sum_line(data = x, column = c("Antall prover", "Antall prover2"), pretext = "Sum", position = "left")

  expect_equal(y[dim(y)[1], "Poststed"], "Sum")
  expect_equal(y[dim(y)[1], "Antall prover"], "26")
  expect_equal(y[dim(y)[1], "Antall prover2"], "13")


  # Include row with generated date, position = "first"
  y <- append_sum_line(data = x, column = c("Antall prover"), pretext = "Sum", position = "first")

  expect_equal(y[dim(y)[1], 1], "Sum")
  expect_equal(y[dim(y)[1], "Antall prover"], "26")


  # Include row with generated date, pretext = "Total"
  y <- append_sum_line(data = x, column = c("Antall prover"), pretext = "Total", position = "first")

  expect_equal(y[dim(y)[1], 1], "Total")
  expect_equal(y[dim(y)[1], "Antall prover"], "26")

})


test_that("Errors for append_sum_line", {

  # Make example data
  x <- as.data.frame(cbind("\u00C5r" = 2021, "Rapport" = "Brucellose hos geit, utvalgsliste",
                           "MT regionnr" = "M21000", "MT region" = "Region Stor-Oslo",
                           "MT avdelingsnr" = "M21150", "MT avdeling" = "Romerike",
                           "Produsentnr" = "30303030", "Foretak" = "XXX XXXXX",
                           "Postnr" = "0468", "Poststed" = "OSLO", "Antall prover" = 26))
  # Probably first makes a matrix, therefore "Antall prøver" is character and must be transformed.
  x$`Antall prover` <- as.numeric(x$`Antall prover`)

  # Wrong column name
  expect_error(append_sum_line(data = x, column = c("Antall_prover"), pretext = "Sum", position = "left"),
               regexp = "Variable \'column\': Names must be a subset of")

  # position not in c("first", "left")
  expect_error(append_sum_line(data = x, column = c("Antall prover"), pretext = "Sum", position = "last"),
               regexp = "Variable \'position\': Must be element of set ")

})
