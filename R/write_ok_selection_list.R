
write_ok_selection_list <- function(data,
                                    filename = paste0(NVIdb::set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/Utvalgslister/",
                                                      "Storfe ammeku BVD EBL IBR ", plan_aar, " Antall blodprøver per slakteri.xlsx"),
                                    sheet = paste0("Antall_blodprøver_ammeku_", plan_aar),
                                    dbtemplate) {
  # Standardiserer kolonnenavn
  okdata <- NVIdb::standardize_columns(data,
                                       standards = OK_column_standards,
                                       dbsource = dbtemplate,
                                       property = "colnames")

  # Plasserer kolonnene i fastsatt rekkefølge og fjerner overflødige kolonner
  okdata  <- NVIdb::standardize_columns(okdata,
                                        standards = OK_column_standards,
                                        dbsource = dbtemplate,
                                        property = "colorder", exclude = TRUE)

  okdata <- append_sum_line(data = okdata, column = c("ant_prover"), position = "left")

  okdata  <- append_date_generated_line(okdata)


  # Generere Excel-ark
  okwb <- openxlsx::createWorkbook()

  NVIpretty::add_formatted_worksheet(data = okdata,
                                     workbook = okwb,
                                     sheet = sheet,
                                     wrapHeadlineText = TRUE,
                                     collabels = TRUE,
                                     colwidths = TRUE,
                                     standards = OK_column_standards,
                                     dbsource = dbtemplate)


  style_sum_line(workbook = okwb, sheet = sheet, data = okdata)

  openxlsx::saveWorkbook(wb = okwb, file = filename, overwrite = TRUE)

}
