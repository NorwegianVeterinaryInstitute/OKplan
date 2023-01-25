#' @title Writes an Excel file with the selection list.
#'
#' @description The selection list generated and ready formatted to
#'     be sent.
#' @details The data must originate from an "okplan" file and
#'     the function uses standardize_columns to  select, order, format and
#'     style the columns. The formatting information is either taken
#'     from \code{OKplan::OK_column_standards} or can be input as
#'     a list.
#'
#' When using \code{OKplan::OK_column_standards}, the formatting
#'     information is taken in accord with the argument \code{dbsource}.
#'     If the formatting needs to be edited, it must be edited in the
#'     general source file for column standards and incorporated into
#'     \code{OKplan}. As this can be a tedious process, there is a
#'     possibility to input the formatting information as a list.
#'
#' The list input to column_standards must follow a specific format.
#'     It is a list with at least three vectors:
#' * colname = a vector of all columns in in the source file that
#'     should be included in the Excel report with the selection list.
#' * collabel: A vector with the labels that should be used in the
#'     Excel report.
#' * colwidth: A vector with the column width that should be used
#'     in the Excel report.
#'
#' All vectors must have the same order and the same length.
#'     In addition one may input:
#' * colorder: the order of the columns to be used in the Excel report.
#'     The Default is to use the same order as they are entered in the vectors.
#' * table_db: input added as a possibility to keep on the same format
#'     as \code{OKplan::OK_column_standards}. Not necessary to input.
#' * column_db: input added as a possibility to keep on the same format
#'     as \code{OKplan::OK_column_standards}. Must be the same as
#'     \code{dbsource}. Not necessary to input.
#'
#' When more than one worksheet should be added to a single workbook,
#'     use \code{add_worksheet = FALSE} for the first worksheet and
#'     \code{add_worksheet = TRUE} for the consecutive worksheet(s).
#'
#'
#' @param data \[data.frame\]. The data with the selected units that should be reported.
#' @param sheet \[character(1)\]. The name of the Excel sheet with the list.
#' @param filename \[character(1)\]. The name of the Excel file that should be written.
#' @param filepath \[character(1)\]. The path to the Excel file that should be written.
#' @param column_standards \[data.frame | list\]. The column standards for OK data frames to be used as
#'      input for \code{NVIdb::standardize_columns} when generating the selection
#'      list, see details. Defaults to \code{OKplan::OK_column_standards}.
#' @param calculate_sum \[logical(1)\]. Should a line with the sum be
#'     appended? Defaults to \code{TRUE}.
#' @param footnote \[character(1)\]. Footnote to appended? Defaults to \code{NULL}.
#' @param footnote_heights \[integer(1)\]. Manually set row height for the
#'      footnote. Defaults to \code{NULL}.
#' @param dbsource \[character(1)\]. The name of the dbtable in OK_column_standards that should
#'     be used for standardizing the columns.
#' @param add_worksheet \[logical(1)\]. Should a worksheet be added to
#'     an existing workbook? Defaults to \code{TRUE}.
#' @md
#' @export
#'
write_ok_selection_list <- function(data,
                                    sheet,
                                    filename,
                                    filepath,
                                    column_standards = OKplan::OK_column_standards,
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
  # column_standards
  checkmate::assert(checkmate::check_class(column_standards, classes = c("data.frame")),
                    checkmate::check_class(column_standards, classes = c("list")),
                    add = checks)
  if (inherits(column_standards, what = "list")) {
    lengths_standard <- lengths(column_standards)
    NVIcheckmate::assert_integer(lengths_standard, lower = lengths_standard[1], upper = lengths_standard[1],
                                    min.len = 3, max.len = 6,
                                    comment = "When input as a list, all elements must have the same length",
    add = checks)

    checkmate::assert_subset(names(column_standards), choices = c("table_db", "colname_db", "colname", "collabel", "colwidth", "colorder"),
                             add = checks)
  }
  if (inherits(column_standards, what = "data.frame")) {
    checkmate::assert_data_frame(column_standards, min.rows = 1, min.cols = 6, add = checks)
  }

  checkmate::assert_flag(calculate_sum, add = checks)
  checkmate::assert_string(footnote, min.chars = 1, null.ok = TRUE, add = checks)
  checkmate::assert_character(dbsource, min.len = 1, add = checks)
  if (inherits(column_standards, what = "data.frame")) {
    checkmate::assert_choice(dbsource,
                             choices = unique(column_standards[, "table_db"]),
                             add = checks)
  }
  checkmate::assert_flag(add_worksheet, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  # column_standards = list("colname" = c("mt_region", "mt_avdeling"),
  #                 "collabel" = c("MT region", "MT avdeling"),
  #                 "colwidth" = c(35, 35))

  if (inherits(column_standards, what = "list")) {
    column_standards <- as.data.frame((column_standards))

    if (!"table_db" %in% colnames(column_standards)) {
      column_standards$table_db <- dbsource
    }

    if (!"colname_db" %in% colnames(column_standards)) {
      column_standards$colname_db <- column_standards$colname
    }

    if (!"colorder" %in% colnames(column_standards)) {
      column_standards$colorder <- c(1:dim(column_standards)[1])
    }
    colnames(column_standards)[which(colnames(column_standards) == "collabel")] <- "label_1_no"
    colnames(column_standards)[which(colnames(column_standards) == "colwidth")] <- "colwidth_Excel"
  }



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
                                       standards = column_standards,
                                       dbsource = dbsource,
                                       property = "colnames")

  # order columns and keep only designated columns
  okdata <- NVIdb::standardize_columns(data = okdata,
                                       standards = column_standards,
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
                                     standards = column_standards,
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
