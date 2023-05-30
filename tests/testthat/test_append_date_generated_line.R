library(OKplan)
library(testthat)

test_that("Append date generated in last row", {


# Make example data
x <- as.data.frame(cbind("\u00C5r" = 2021, "Rapport" = "Brucellose hos geit, utvalgsliste",
                         "MT regionnr" = "M21000", "MT region" = "Region Stor-Oslo", "MT avdelingsnr" = "M21150", "MT avdeling" = "Romerike",
                         "Produsentnr" = "30303030", "Foretak" = "XXX XXXXX", "Postnr" = "0468", "Poststed" = "OSLO", "Antall pr\u00F8ver" = 26))

# Include row with generated date
y <- append_date_generated_line(x)

expect_equal(dim(y)[1], dim(x)[1] + 2)
expect_identical(y[dim(y)[1], 1], paste("Datauttrekket er gjort", format(Sys.Date(), "%d/%m/%Y")))


# Include row with generated date
today <- format(Sys.Date(), "%d/%m/%Y")
y <- append_date_generated_line(x, pretext = "Data was generated", date = today)

expect_equal(dim(y)[1], dim(x)[1] + 2)
expect_identical(y[dim(y)[1], 1], paste("Data was generated", format(Sys.Date(), "%d/%m/%Y")))

})
