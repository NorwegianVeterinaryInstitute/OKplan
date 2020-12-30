#### UTVALG GRISEBESETNINGER SOM SKAL UNDERSØKES FOR VIRUS OG SALMONELLA ####

### BESKRIVELSE AV SCRIPT ----
# PRINSIPP
#   1. Genererer geitepopulasjonen
#      a. Leser inndata fra husdyrregisteret småfe
#      b. Leser inndata fra søknad om produksjonstilskudd
#      c. Kombinerer registrene
#   2. Ekskluderer besetninger
#      a. Ekskluderer geitebesetninger testet for brucella siste 2 år
#      b. Elkskluderer geitebesetninger teste for paratuberkulose siste to år
#   3.


# AVHENGIGHETER
# Pakker

# Filer
#

# MANGLER
#

### OPPSETT AV R-MILJØ ----
# rm(list = ls())    # Benyttes for å tømme R-environment ved behov

# AKTIVERE R-PAKKER
library(dplyr)
library(tidyr)
# library(janitor)
library(openxlsx)
library(NVIdb)
library(haven)
library(RODBC)

# GLOBALE VARIABLER
# today: Dagens dato i formatet yyyymmdd for bruk i filnavn
today<-format(Sys.Date(),"%Y%m%d")

# Variables defining the dataset with counting date and extracting date from the central database
# source("config data source for prodtilskudd.R", encoding = "UTF-8")

plan_aar <- 2021
ant_store_slakterier <- 7
min_slaktegris <- 400
n_selektert <- 21  # Tilfeldig utvalg av 21 besetninger fra hvert av slakteriene i 2021
# n_reserve <- 15    # 15 per MT region;

seed <- runif(n = 1, min = 1, max = 1000000000)
format(seed, scientific = FALSE)
seed <- 925911551 #Bestemt 27.11.2020 for 2021

# Kataloger og filnavn
# Working directory er katalogen for denne filen
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

fil_avlsgris <- "Avlsgris_202010.csv"
fil_purkeringer <- "Purkering_202011.csv"

### IMPORTERER DATA
# STØTTEREGISTERE
kommune_fylke <- read_kommune_fylke()
slakterier <- read_sas(data_file = paste0(set_dir_NVI("GrunndataLand"), "FormaterteData/SASdata/slakteri.sas7bdat"))
slakterier <- add_kommune_fylke(slakterier, code_column = c("kommunenr" = "komnr"), new_column = "gjeldende_komnr")

anlegg <- read.csv2(file = paste0(set_dir_NVI("LevReg"), "RaData/2020/anlegg.dsv"), colClasses = "character", fileEncoding = "UTF-8", header = FALSE)
colnames(anlegg) <- c("anleggskode", "anleggsnavn")

komnr_2_MT_omrader <- read_MT_omrader()
Pkode_2_text <- read_Pkode_2_text()
prodnr_2_current_prodnr <- read_prodnr_2_current_prodnr()
PJS_codes_2_text <- read_PJS_codes_2_text()
poststed <- read_poststed()

journal_rapp <- login_by_credentials_PJS()
produsenter <- sqlQuery(journal_rapp,
                        "select * from v_registere_alle where kategoritype = 'PROD'",
                        as.is = TRUE,
                        stringsAsFactors = FALSE)
odbcClose(journal_rapp)
# produsenter <- produsenter %>%
#   select(identifikator, navn, postnr, utgatt_dato) %>%
#   mutate(prodnr8 = substr(identifikator, 1, 8)) %>%
#   mutate(utgatt = case_when(is.na(utgatt_dato) ~ as.Date("2021-12-12", format = "%Y-%m-%d"),
#                             TRUE ~ as.Date(substr(utgatt_dato, 1, 10), format = "%Y-%m-%d"))) %>%
#   group_by(prodnr8) %>%
#   mutate(maxutgatt = max(utgatt)) %>%
#   filter(utgatt == maxutgatt) %>%
#   ungroup() %>%
#   select(- identifikator, - utgatt_dato) %>%
#   distinct()

# POPULASJONSDATA
# Leser inn avlsgris
colclasses <- standardize_columns(csvfile =  paste0(set_dir_NVI("FAG"),"EksterneDatakilder/Avlsgris/FormaterteData/", fil_avlsgris), property = "colclasses")
avlsgris <- read.csv2(file = paste0(set_dir_NVI("FAG"),"EksterneDatakilder/Avlsgris/FormaterteData/", fil_avlsgris),
                      colClasses = colclasses,
                      fileEncoding = "UTF-8")

# Leser inn purkeringer
colclasses <- standardize_columns(csvfile =  paste0(set_dir_NVI("FAG"),"EksterneDatakilder/Purkeringer/FormaterteData/", fil_purkeringer), property = "colclasses")
purkeringer <- read.csv2(file = paste0(set_dir_NVI("FAG"),"EksterneDatakilder/Purkeringer/FormaterteData/", fil_purkeringer),
                         colClasses = colclasses,
                         fileEncoding = "UTF-8")

# leser inn produksjonstilskudd
prodtilskudd <- read_Prodtilskudd(Pkode_year = "last", Pkode_month = "last")

# Leser inn leveranseregisteret for slakt
source(paste0("./Leser inn LevReg 201907-202006.R"))


# Besetninger testet siste 2 år
EOS <- login_by_credentials_EOS()
# Testet for brucella
proveresultat_virusinfeksjoner <- sqlQuery(EOS,
                                           "select * from proveresultat_virusinfeksjoner",
                                           as.is = TRUE)
proveresultat_virusinfeksjoner <- standardize_columns(proveresultat_virusinfeksjoner, property = "colnames")

# Fjerner doble registreringer av saker som skyldes at én kommune er knyttet til flere MT avdelinger
proveresultat_virusinfeksjoner <- proveresultat_virusinfeksjoner %>%
  select(- mt_avdelingnr, - mt_avdeling) %>%
  distinct()

virus_gris <- proveresultat_virusinfeksjoner %>%
  mutate(aar = substr(saksnr, 1, 4)) %>%
  mutate(prodnr8 = substr(prodnr, 1, 8)) %>%
  mutate(ant_und_ad = as.numeric(ant_und_ad)) %>%
  add_produsent(translation_table = prodnr_2_current_prodnr, code_column = "prodnr8", new_column = "gjeldende_prodnr8") %>%
  filter(ad == "Negativ") %>%
  select(aar, gjeldende_prodnr8, ant_und_ad) %>%
  group_by(aar, gjeldende_prodnr8) %>%
  mutate(ant_und_ad = sum(ant_und_ad, na.rm = TRUE)) %>%
  distinct() %>%
  filter(ant_und_ad >=8) %>%
  ungroup()



### LAGER LISTER OVER SLAKTEGRISBESETNINGER SOM SKAL TESTES (UTVALGSRAMMEN)

# KLARGJØR DATA FOR SLAKT AV GRIS
# Antall voksne griser slaktet per slakteri
griseslakt <- LevReg %>%
  # Selekterer voksen sau
  filter(varekategorikode %in% c("170","171","172")) %>%
  mutate(besetnings_id = coalesce(produsentorgnr, hovednr)) %>%
  group_by(anleggskode, besetnings_id, varekategorikode) %>%
  # Summerer per slakteri og produsent
  mutate(ant_gris = sum(ant_levert, na.rm = TRUE)) %>%
  select(anleggskode, besetnings_id, varekategorikode, ant_gris) %>%
  ungroup() %>%
  distinct() %>%
  # Setter alle varekategorikoder på en linje
  pivot_wider(id_cols = c(anleggskode, besetnings_id), names_from = varekategorikode, values_from = ant_gris, names_prefix = "v") %>%
  rename(slGris = v170, slPurke = v171, slRane = v172) %>%
  group_by(anleggskode, besetnings_id) %>%
  mutate(slAdSvin = sum(slPurke, slRane, na.rm = TRUE)) %>%
  ungroup()

# FINNER DE 7 STØRSTE SLAKTERIENE

utvalgte_slakterier <- griseslakt %>%
  group_by(anleggskode) %>%
  summarise(anleggskode, ant_slaktegris = sum(slGris, na.rm = TRUE), .groups = "keep") %>%
  ungroup() %>%
  distinct() %>%
  arrange(desc(ant_slaktegris)) %>%
  mutate(radnr = 1:n()) %>%
  filter(radnr <= ant_store_slakterier)

### EKSKLUDERER BESETNINGER ----

# Selekterer besetninger som har
#   levert >=400 gris
#   slakteri og eier som leverer maximum antall gris
#     (for å unngå at de leverer noen få dyr til det slakteriet der prøvene skal tas)
#   leverte råner og purker utgjør mindre enn 1 prosent av leverte slaktegris
#     (mao. godtar at enkelte slaktegris blir klassifisert som råne/purke)
# leverer gris til de seks største slakteriene

store_slaktegris_produsenter <- griseslakt %>%
  filter(!is.na(slGris)) %>%
  group_by(besetnings_id) %>%
  mutate(tot_slaktegris = sum(slGris, na.rm = TRUE), max_slaktegris = max(slGris, na.rm = TRUE)) %>%
  filter(tot_slaktegris >= min_slaktegris) %>%
  filter(slGris >= max_slaktegris)%>%
  filter(slAdSvin  < tot_slaktegris * 0.01) %>%
  filter(anleggskode %in% utvalgte_slakterier$anleggskode) %>%
  ungroup()

# Ekskluderer besetninger som er
#   avlsbesetninger
#   purkeringnav
#   ble prøvetatt med mer enn 8 prøver i ett av de siste to årene
#   besetninger med smågris

store_slaktegris_produsenter <- store_slaktegris_produsenter %>%
  filter(!besetnings_id %in% substr(avlsgris$prodnr10, 1, 8)) %>%
  filter(!besetnings_id %in% substr(purkeringer$prodnr10, 1, 8)) %>%
  filter(!besetnings_id %in% virus_gris$gjeldende_prodnr8) %>%
  filter(!besetnings_id %in% prodtilskudd[which(prodtilskudd$P154 > 1 & prodtilskudd$P155 > 1), "gjeldende_prodnr8"])




#
### TILFELDIG UTVALG AVLBANT DE STØRSTE SLAKTEGRISBESETNINGER PÅ DE STØRSTE SLAKTERIENE ----
#

make_random <- function(data, colname = "random", seed = -1) {
  set.seed(seed)
  data[, colname] <- runif(n = dim(data)[1])
  return(data)
}

# Selekterer utvalgte besetninger

utvalg <- store_slaktegris_produsenter %>%
  select(anleggskode, besetnings_id) %>%
  arrange(besetnings_id) %>%
  make_random(colname = "random", seed = seed) %>%
  arrange(anleggskode, random) %>%
  group_by(anleggskode) %>%
  mutate(radnr = 1:n()) %>%
  ungroup() %>%
  mutate(statuskode = case_when(radnr <= n_selektert ~ 1,
                                TRUE ~ 0)) %>%
  filter(statuskode != 0) %>%
  select( -radnr, - random)

### KLARGJØR FOR OKWEB ----

# Standard kolonner i OKWEB-filer
OK_kolonner <- c("lopenr", "ok_aar", "ok_programkode", "ok_hensiktkode",
                 "mt_regionnr", "mt_region", "mt_avdelingnr", "mt_avdeling",
                 "eier_lokalitettype", "eier_lokalitetnr", "eier_lokalitet",
                 "postnr", "poststed", "ok_artkode", "ok_driftsformkode", "storrelseskategori", "kategori",
                 "materialekode", "ant_prover", "analyttkode",
                 "annen_aktortype", "annen_aktornr", "annen_aktor",
                 "provetakingstidspunkt", "provetakingstidspunkt_enhet",
                 "laboratorium", "journalnr_ekstern",
                 "mottatt", "statuskode", "status_dato", "prioritet_av_reserve",
                 "erstatt_eier_lokalitetstype", "erstatt_eier_lokalitetnummer", "erstatt_eier_lokalitetsnavn",
                 "aarsak_til_erstatning", "hist_eier_lokalitetstype", "hist_eier_lokalitetnummer", "hist_eier_lokalitetsnavn",
                 "utvalg_laget_dato", "adresse_2")


foretak <- LevReg_navn %>%
  filter(varekategorikode %in% c("170")) %>%
  distinct() %>%
  group_by(besetnings_id) %>%
  mutate(last_slaktedato = max(max_slaktedato)) %>%
  filter(last_slaktedato == max_slaktedato) %>%
  select(besetnings_id, prodnr10, produsentnavn)


okweb_slaktegris <- utvalg %>%
  left_join(foretak, by = c("besetnings_id" = "besetnings_id")) %>%
  mutate(ok_driftsformkode = "10202") %>%
  mutate(storrelseskategori = ">400 slakta gris") %>%
  mutate(annen_aktortype = "AUT") %>%
  left_join(anlegg, by = c("anleggskode" = "anleggskode")) %>%
  left_join(slakterier[, c("SlaktID", "Autnr", "gjeldende_komnr")], by = c("anleggskode" = "SlaktID")) %>%
  mutate(eier_lokalitetnr = coalesce(prodnr10, besetnings_id)) %>%
  rename(annen_aktornr = Autnr, annen_aktor = anleggsnavn, eier_lokalitet = produsentnavn) %>%
  add_MT_omrader(code_column = c("gjeldende_komnr" = "komnr"),
                 new_column = c("mt_regionnr" = "MT_regionnr", "mt_region" = "MT_region", "mt_avdelingnr" = "MT_avdelingnr", "mt_avdeling" = "MT_avdeling"),
                 position = "first") %>%
  mutate(eier_lokalitettype = case_when(nchar(eier_lokalitetnr) == 9 ~ "ORG",
                                        TRUE ~ "PROD")) %>%
  mutate(statuskode = 1) %>%
  mutate(ok_hensiktkode = "0200102") %>%
  mutate(ok_programkode = "01003") %>%
  mutate(materialekode = "05002") %>%
  mutate(ant_prover = 10) %>%
  mutate(analyttkode = "01")



okweb_avlsgris <- avlsgris %>%
  filter(driftsformkode %in% c("10103", "10104")) %>%
  rename(eier_lokalitetnr = prodnr10, ok_driftsformkode = driftsformkode) %>%
  mutate(komnr = substr(eier_lokalitetnr, 1, 4)) %>%
  add_MT_omrader(code_column = c("komnr"),
                 new_column = c("mt_regionnr" = "MT_regionnr", "mt_region" = "MT_region", "mt_avdelingnr" = "MT_avdelingnr", "mt_avdeling" = "MT_avdeling"),
                 position = "first") %>%
  mutate(eier_lokalitettype = "PROD") %>%
  mutate(statuskode = 1) %>%
  mutate(ok_hensiktkode = "0200102") %>%
  mutate(ok_programkode = "01003") %>%
  mutate(materialekode = "05002") %>%
  mutate(ant_prover = 10) %>%
  mutate(analyttkode = "01")

okweb_avlsgris_salmonella <- okweb_avlsgris %>%
  mutate(ok_programkode = "01008") %>%
  mutate(ok_hensiktkode = "0200105") %>%
  mutate(materialekode = "05001") %>%
  mutate(ant_prover = as.numeric(NA)) %>%
  mutate(analyttkode = "04060103")




okweb_purkeringer <- purkeringer %>%
  filter(driftsformkode == "1021101") %>%
  rename(eier_lokalitetnr = prodnr10, ok_driftsformkode = driftsformkode, annen_aktor = purkering) %>%
  mutate(komnr = substr(eier_lokalitetnr, 1, 4)) %>%
  add_MT_omrader(code_column = c("komnr"),
                 new_column = c("mt_regionnr" = "MT_regionnr", "mt_region" = "MT_region", "mt_avdelingnr" = "MT_avdelingnr", "mt_avdeling" = "MT_avdeling"),
                 position = "first") %>%
  mutate(kategori = "Purkeringnav") %>%
  mutate(eier_lokalitettype = "PROD") %>%
  mutate(statuskode = 1) %>%
  mutate(ok_hensiktkode = "0200102") %>%
  mutate(ok_programkode = "01003") %>%
  mutate(materialekode = "05002") %>%
  mutate(ant_prover = 10) %>%
  mutate(analyttkode = "01")


okweb <- bind_rows(okweb_avlsgris,
                   okweb_avlsgris_salmonella,
                   okweb_purkeringer,
                   okweb_slaktegris) %>%
  mutate(ok_aar = plan_aar) %>%
  # mutate(navn = toupper(eier_lokalitet)) %>%
  left_join(produsenter[, c("identifikator", "navn", "postnr")], by = c("eier_lokalitetnr" = "identifikator")) %>%
  mutate(eier_lokalitet = coalesce(eier_lokalitet, navn)) %>%
  add_poststed(new_column = "poststed") %>%
  mutate(ok_artkode = "03100101001") %>%
  mutate(status_dato = Sys.Date()) %>%
  mutate(utvalg_laget_dato = Sys.Date())



okweb[, base::setdiff(OK_kolonner, colnames(okweb))] <- NA
# okweb <- okweb[, c(OK_kolonner, base::setdiff(colnames(okweb), OK_kolonner))]
okweb <- okweb[, OK_kolonner]

okweb <- arrange(okweb, ok_hensiktkode, ok_driftsformkode, statuskode, prioritet_av_reserve, eier_lokalitetnr)

write.csv2(okweb,
           file = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/resultater/utvalgslister/OKWEB_alle_gris.csv"),
           row.names = FALSE,
           fileEncoding = "UTF-8")



### KONTROLLER ----
# data OKWEB_geit_brucella;
# set plan&planaar..OKWEB_geit_brucella;
# run;
source("./OK utvalg standard kontroll av okweb-filen.R")


### EKSPORT AV utvalg brucella geit TIL EXCEL-FILER ----
# * 2.  Tillaging av datafil med korrekt navn og rekkefølge på variablene
# koder må transformeres til variabelinnhold vha formater før eksporten
# 3.  Eksport til Excel;
#
# * TILLAGING AV DATAFIL MED UTVALGET FOR EKSPORT;
# * Proc sql benyttes for å kunne styre rekkefølgen av variablene;
#

# Eksporterer listen til Excel;
formatExcelsheet <- function (data, workbook, sheet, wrapHeadlineText = FALSE) {
  colnames(data) <- standardize_columns(data = data, property = "collabels")
  addWorksheet(wb = workbook, sheetName = sheet)
  writeData(wb = workbook, sheet = sheet, data, withFilter = TRUE)
  freezePane(wb = workbook, sheet = sheet, firstRow = TRUE)
  styleBold <- createStyle(textDecoration = "bold", wrapText = wrapHeadlineText)
  addStyle(wb = workbook, sheet = sheet, style = styleBold, rows = 1, cols = 1:dim(data)[2])
}

# Purkeringnav
export <- okweb %>%
  filter(statuskode == 1 & ok_driftsformkode == "1021101") %>%
  mutate(rapport = "Spesifikke virusinfeksjoner hos svin, Purkeringnav") %>%
  select(purkering = annen_aktor, aar = ok_aar, rapport, mt_regionnr, mt_region, mt_avdelingnr, mt_avdeling,
         prodnr = eier_lokalitetnr, virksomhet = eier_lokalitet, postnr, poststed, purkering = annen_aktor,
         ant_prover) %>%
  mutate(aar = as.character(aar)) %>%
  arrange(mt_regionnr, mt_avdelingnr, prodnr) %>%
  adorn_totals( where = "row", fill = "", name = "Sum") %>%
  select(aar, rapport, mt_regionnr, mt_region, mt_avdelingnr, mt_avdeling,
         prodnr, virksomhet, postnr, poststed, purkering,
         ant_prover) %>%

  bind_rows(c("aar" = ""),
            c("aar" = paste("Datauttrekket er gjort ", format(Sys.Date(),"%d/%m/%Y"))))

OK_wb <- createWorkbook()
# colnames(antprover_slakteri) <- standardize_columns(data = antprover_slakteri, property = "collabels")

sheetname <- paste0("Purkeringnav_utvalgt_", plan_aar)

formatExcelsheet(export, OK_wb, sheet = sheetname, wrapHeadlineText = TRUE)
styleBold <- createStyle(textDecoration = "bold", wrapText = FALSE)
addStyle(wb = OK_wb, sheet = sheetname, style = styleBold, rows = dim(export)[1] - 1, cols = 1:dim(export)[2])
setColWidths(wb = OK_wb, sheet = sheetname,
             cols = c(1:dim(export)[2]),
             widths = c(5, 10, 12.5, 16, 13, 30, 12, 30, 8, 15, 20, 8.5))

saveWorkbook(wb = OK_wb,
             file = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/Utvalgslister/Svin virusundersøkelser ", plan_aar, " purkeringnav.xlsx"),
             overwrite = TRUE)



# Avlsgrislister
export <- okweb %>%
  filter(ok_hensiktkode == "0200102", statuskode == 1 & substr(ok_driftsformkode, 1, 3) == "101") %>%
  mutate(rapport = "Salmonella og spesifikke virusinfeksjoner hos avlsbesetninger av gris, utvalgsliste") %>%
  mutate(`undersøkes for` = "Virus, Salmonella") %>%
  add_PJS_code_description(PJS_variable_type = "driftsform", code_colname = "ok_driftsformkode", new_column = "driftsform") %>%
  select(`undersøkes for`, aar = ok_aar, rapport, mt_regionnr, mt_region, mt_avdelingnr, mt_avdeling,
         prodnr = eier_lokalitetnr, virksomhet = eier_lokalitet, postnr, poststed, kategori = driftsform,
         ant_prover) %>%
  mutate(aar = as.character(aar)) %>%
  arrange(mt_regionnr, mt_avdelingnr, prodnr) %>%
  adorn_totals( where = "row", fill = "", name = "Sum") %>%
  select(aar, rapport, mt_regionnr, mt_region, mt_avdelingnr, mt_avdeling,
         prodnr, virksomhet, postnr, poststed, kategori, `undersøkes for`,
         ant_prover) %>%

  bind_rows(c("aar" = ""),
            c("aar" = paste("Datauttrekket er gjort ", format(Sys.Date(),"%d/%m/%Y"))))

OK_wb <- createWorkbook()
# colnames(antprover_slakteri) <- standardize_columns(data = antprover_slakteri, property = "collabels")

sheetname <- paste0("Avlsgris_utvalgt_", plan_aar)

formatExcelsheet(export, OK_wb, sheet = sheetname, wrapHeadlineText = TRUE)
styleBold <- createStyle(textDecoration = "bold", wrapText = FALSE)
addStyle(wb = OK_wb, sheet = sheetname, style = styleBold, rows = dim(export)[1] - 1, cols = 1:dim(export)[2])
setColWidths(wb = OK_wb, sheet = sheetname,
             cols = c(1:dim(export)[2]),
             widths = c(5, 10, 12.5, 16, 13, 30, 12, 30, 8, 15, 18, 15, 8.5))

saveWorkbook(wb = OK_wb,
             file = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/Utvalgslister/Svin avlsgrisbesetninger for salmonella og virusundersøkelser ", plan_aar, ".xlsx"),
             overwrite = TRUE)


# Slaktegrislister
export <- okweb %>%
  filter(ok_hensiktkode == "0200102", statuskode == 1 & ok_driftsformkode == "10202") %>%
  mutate(rapport = "Spesifikke virusinfeksjoner hos slaktegrisbesetninger, utvalgsliste") %>%
  add_PJS_code_description(PJS_variable_type = "driftsform", code_colname = "ok_driftsformkode", new_column = "driftsform") %>%
  select(aar = ok_aar, rapport, mt_regionnr, mt_region, mt_avdelingnr, mt_avdeling,
         prodnr = eier_lokalitetnr, virksomhet = eier_lokalitet, postnr, poststed, autnr = annen_aktornr, slakteri = annen_aktor) %>%
  mutate(aar = as.character(aar)) %>%
  arrange(mt_regionnr, mt_avdelingnr, prodnr) %>%
  # adorn_totals( where = "row", fill = "", name = "Sum") %>%
  # select(aar, rapport, mt_regionnr, mt_region, mt_avdelingnr, mt_avdeling,
  #        prodnr, virksomhet, postnr, poststed, autnr, slakteri,
  #        ant_prover) %>%

  bind_rows(c("aar" = ""),
            c("aar" = paste("Datauttrekket er gjort ", format(Sys.Date(),"%d/%m/%Y"))))

OK_wb <- createWorkbook()
# colnames(antprover_slakteri) <- standardize_columns(data = antprover_slakteri, property = "collabels")

sheetname <- paste0("Slaktegris_utvalgt_", plan_aar)

formatExcelsheet(export, OK_wb, sheet = sheetname, wrapHeadlineText = TRUE)
# styleBold <- createStyle(textDecoration = "bold", wrapText = FALSE)
# addStyle(wb = OK_wb, sheet = sheetname, style = styleBold, rows = dim(export)[1] - 1, cols = 1:dim(export)[2])
setColWidths(wb = OK_wb, sheet = sheetname,
             cols = c(1:dim(export)[2]),
             widths = c(5, 10, 12.5, 16, 13, 30, 12, 30, 8, 15, 5, 25))

saveWorkbook(wb = OK_wb,
             file = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/Utvalgslister/Svin slaktegris virusundersøkelser ", plan_aar, " utvalgte besetninger.xlsx"),
             overwrite = TRUE)



