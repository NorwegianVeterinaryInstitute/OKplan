context("include_generated_date")
library(OKplan)
library(testthat)

test_that("Including generated date in last row", {


# Make example data
x <- as.data.frame(cbind("År" = 2021, "Rapport" = "Brucellose hos geit, utvalgsliste",
                         "MT regionnr" = "M21000", "MT region" = "Region Stor-Oslo", "MT avdelingsnr" = "M21150", "MT avdeling" = "Romerike",
                         "Produsentnr" = "30303030", "Foretak" = "XXX XXXXX", "Postnr" = "0468", "Poststed" = "OSLO", "Antall prøver" = 26))

# Include row with generated date
y <- include_generated_date(x)

expect_equal(dim(y)[1], dim(x)[1] + 2)
expect_identical(y[dim(y)[1], 1], paste("Datauttrekket er gjort", format(Sys.Date(),"%d/%m/%Y")))


# Include row with generated date
today <- format(Sys.Date(),"%d/%m/%Y")
y <- include_generated_date(x, pretext = "Data was generated", date = today)

expect_equal(dim(y)[1], dim(x)[1] + 2)
expect_identical(y[dim(y)[1], 1], paste("Data was generated", format(Sys.Date(),"%d/%m/%Y")))

})

