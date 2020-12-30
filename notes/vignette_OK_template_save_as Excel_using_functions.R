library(openxlsx)
library(dplyr)
library(NVIdb)
library(OKplan)
library(NVIpretty)


plan_aar <- 2021

colclasses <- standardize_columns(csvfile = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/resultater/utvalgslister/OKWEB_alle_gris.csv"),
                                  dbsource = "okplan",
                                  property = "colclasses")
okplan <- read.csv2(file = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/resultater/utvalgslister/OKWEB_alle_gris.csv"),
                    colClasses = colclasses,
                    fileEncoding = "UTF-8")
# data <- gris_virus_slaktegris_utvalg
# dbsource <- "gris_virus_slaktegris_utvalg"

# Slaktegrislister
gris_virus_slaktegris_utvalg <- okplan %>%
  dplyr::filter(ok_hensiktkode == "0200102" & statuskode == 1 & ok_driftsformkode == "10202") %>%
  dplyr::mutate(rapport = "Spesifikke virusinfeksjoner hos slaktegrisbesetninger, utvalgsliste") %>%
  dplyr::arrange(mt_regionnr, mt_avdelingnr, eier_lokalitetnr)

# Standardiserer kolonnenavn
gris_virus_slaktegris_utvalg <- standardize_columns(gris_virus_slaktegris_utvalg, property = "colnames")

# Plasserer kolonnene i fastsatt rekkefølge og fjerner overflødige kolonner
gris_virus_slaktegris_utvalg  <- standardize_columns(gris_virus_slaktegris_utvalg, property = "colorder", exclude = TRUE)


# Inkludere en tom rad og en rad som angir når datauttrekket ble gjort
gris_virus_slaktegris_utvalg <- include_generated_date(gris_virus_slaktegris_utvalg)


# Generere Excel-ark
OK_wb <- createWorkbook()
# colnames(antprover_slakteri) <- standardize_columns(data = antprover_slakteri, property = "collabels")


add_formatted_worksheet(gris_virus_slaktegris_utvalg,
                        OK_wb,
                        sheet = paste0("Slaktegris_utvalgt_", plan_aar),
                        wrapHeadlineText = TRUE,
                        collabels = TRUE,
                        colwidths = TRUE)


saveWorkbook(wb = OK_wb,
             file = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/Utvalgslister/Test ", plan_aar, " utvalgte besetninger.xlsx"),
             overwrite = TRUE)


