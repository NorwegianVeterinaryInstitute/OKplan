#' @title Writes an Excel file with the selection list.
#'
#' @description The selection list based on selected data from okplan file and
#'     uses standardize_columns to  select, format and style columns.
#'
#' @param data The data with units that should be tested.
#' @param filename The name of the Excel file that should be written.
#' @param filepath The path to the Excel file that should be written.
#' @param sheet The name of the Excel sheet with the list.
#' @param calculate_sum \[logical\] Should a line with the sum be appended. Defaults to TRUE.
#' @param dbsource The name of the dbtable in OK_column_standards that should
#'     be used for standardizing the columns.
#' @export


write_ok_selection_list <- function(data,
                                    sheet,
                                    filename,
                                    filepath,
                                    calculate_sum = TRUE,
                                    dbsource) {
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  # for (i in 1:length(data)) {
  checkmate::assert_data_frame(data, max.rows = (1048576 - 1), max.cols = 16384, add = checks)
  # }
  checkmate::assert_character(sheet, min.chars = 1, min.len = 1, max.len = length(data), unique = TRUE, add = checks)
  checkmate::assert_character(filename, min.chars = 1, len = 1, add = checks)
  # Remove trailing backslash or slash before testing path
  filepath <- sub("\\\\{1,2}$|/{1,2}$", "", filepath)
  checkmate::assert_directory_exists(filepath, add = checks)
  checkmate::assert_logical(calculate_sum, any.missing = FALSE, min.len = 1, add = checks)
  checkmate::assert_character(dbsource, min.len = 1, add = checks)
  checkmate::assert_choice(dbsource,
                           choices = unique(OKplan::OK_column_standards[, "table_db"]),
                           add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # GENERATE EXCEL WORKBOOK ----
  okwb <- openxlsx::createWorkbook()

  # for (i in 1:length(data)) {
  # i <- 1
  # STANDARDIZE COLUMNS ----
  # column names
  okdata <- NVIdb::standardize_columns(data,
                                       standards = OKplan::OK_column_standards,
                                       dbsource = dbsource,
                                       property = "colnames")

  # order columns and keep only designated columns
  okdata <- NVIdb::standardize_columns(data = okdata,
                                        standards = OKplan::OK_column_standards,
                                        dbsource = dbsource,
                                        property = "colorder", exclude = TRUE)

  # INCLUDE EXTRA INFORMATION ----
  # Append sum
  if (isTRUE(calculate_sum)) {
    okdata <- append_sum_line(data = okdata, column = c("ant_prover"), position = "left")
  }

  # Append date generated
  okdata <- append_date_generated_line(okdata)


  # STYLE EXCEL SHEET ----
  NVIpretty::add_formatted_worksheet(data = okdata,
                                     workbook = okwb,
                                     sheet = sheet,
                                     wrapHeadlineText = TRUE,
                                     collabels = TRUE,
                                     colwidths = TRUE,
                                     standards = OKplan::OK_column_standards,
                                     dbsource = dbsource)


  if (isTRUE(calculate_sum)) {
    style_sum_line(workbook = okwb, sheet = sheet, data = okdata)
  }
  # }
  # SAVE EXCEL WORKBOOK ----
  openxlsx::saveWorkbook(wb = okwb, file = file.path(filepath, filename), overwrite = TRUE)

}
