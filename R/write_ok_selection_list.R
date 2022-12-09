#' @title Writes an Excel file with the selection list.
#'
#' @description The selection list generated and ready formatted to
#'     be sent.
#' @details The data must originate from an "okplan" file and
#'     the function uses standardize_columns to  select, order, format and
#'     style the columns. To formatting information is edited in the
#'     general source file for column standards and included into
#'     \code{OKplan}.
#'
#'     When more than one worksheet should be added to a single workbook,
#'     use \code{add_worksheet = FALSE} for the first worksheet and
#'     \code{add_worksheet = TRUE} for the consecutive worksheet(s).
#'
#' @param data The data with units that should be tested.
#' @param filename The name of the Excel file that should be written.
#' @param filepath The path to the Excel file that should be written.
#' @param sheet The name of the Excel sheet with the list.
#' @param calculate_sum \[logical(1)\]. Should a line with the sum be
#'     appended? Defaults to \code{TRUE}.
#' @param footnote \[character(1)\]. Footnote to appended? Defaults to \code{NULL}.
#' @param footnote_heights \[integer(1)\]. Manually set row height for the
#'      footnote. Defaults to \code{NULL}.
#' @param dbsource The name of the dbtable in OK_column_standards that should
#'     be used for standardizing the columns.
#' @param add_worksheet \[logical(1)\]. Should a worksheet be added to
#'     an existing workbook? Defaults to \code{TRUE}.
#' @export
#'
write_ok_selection_list <- function(data,
                                    sheet,
                                    filename,
                                    filepath,
                                    calculate_sum = TRUE,
                                    footnote = NULL,
                                    footnote_heights = NULL,
                                    dbsource,
                                    add_worksheet = FALSE) {
  # Remove trailing backslash or slash before testing path
  filepath <- sub("\\\\{1,2}$|/{1,2}$", "", filepath)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  # for (i in 1:length(data)) {
  checkmate::assert_data_frame(data, max.rows = (1048576 - 1), max.cols = 16384, add = checks)
  # }
  checkmate::assert_character(sheet, min.chars = 1, min.len = 1, max.len = length(data), unique = TRUE, add = checks)
  checkmate::assert_character(filename, min.chars = 1, len = 1, add = checks)
  checkmate::assert_directory_exists(filepath, add = checks)
  if (isTRUE(add_worksheet)) {
    checkmate::assert_file_exists(file.path(filepath, filename), access = "r")
  }
  checkmate::assert_flag(calculate_sum, add = checks)
  checkmate::assert_string(footnote, min.chars = 1, null.ok = TRUE, add = checks)
  checkmate::assert_character(dbsource, min.len = 1, add = checks)
  checkmate::assert_choice(dbsource,
                           choices = unique(OKplan::OK_column_standards[, "table_db"]),
                           add = checks)
  checkmate::assert_flag(add_worksheet, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # GENERATE EXCEL WORKBOOK ----
  if (isTRUE(add_worksheet)) {
    okwb <- openxlsx::loadWorkbook(xlsxFile = file.path(filepath, filename))
  } else {
    okwb <- openxlsx::createWorkbook()
  }

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


  # Append footnote
  if (!is.null(footnote)) {
    okdata <- NVIpretty::append_text_line(okdata,
                                          text = footnote,
                                          empty_rows = 2)
  }


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

  if (!is.null(footnote)) {
    NVIpretty::style_text_line(workbook = okwb, sheet = sheet, data = okdata,
                               text = footnote,
                               wrap_text = TRUE,
                               merge_cells = TRUE,
                               heights = footnote_heights)
  }


  # }
  # SAVE EXCEL WORKBOOK ----
  openxlsx::saveWorkbook(wb = okwb, file = file.path(filepath, filename), overwrite = TRUE)

}
