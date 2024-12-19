#' @title Writes the sampling plan with the selection list to an Excel file.
#'
#' @description The sampling plan is output to an Excel sheet. The list with
#'     selected units is standardised and formatted in order to be submitted
#'     without further formatting.
#' @details The data must originate from an "okplan" file and
#'     the function uses
#'     \ifelse{html}{\code{\link[NVIdb:standardize_columns]{NVIdb::standardize_columns}}}{\code{NVIdb::standardize_columns}}.
#'     to select, order, format and style the columns. The formatting
#'     information is either taken from \code{\link{OK_column_standards}} or
#'     can be input as a \code{list}
#'
#' When using \code{\link{OK_column_standards}}, the formatting information is
#'     taken in accord with the argument \code{dbsource}. If the formatting
#'     needs to be edited, it must be edited in the general source file for
#'     column standards and thereafter, build it into a new version of \code{OKplan}.
#'     As this can be a tedious process, there is a possibility to input the
#'     formatting information as a \code{list} or as a csv-file that can be transformed
#'     to a \code{data.frame} with the same columns as \code{\link{OK_column_standards}}.
#'
#' The list input to column_standards must follow a specific format.
#'     It is a \code{list} with at least three named vectors:
#' \itemize{
#' \item \code{colname}: a vector of all columns in in the source file that
#'     should be included in the Excel report with the selection list.
#' \item \code{collabel}: A vector with the labels that should be used in the
#'     Excel report.
#' \item \code{colwidth}: A vector with the column width that should be used
#'     in the Excel report.
#' }
#'
#'     In addition one may input:
#'
#' \itemize{
#' \item \code{colorder}: the order of the columns to be used in the Excel report.
#'     The default is to use the same order as they are entered in the vectors.
#' \item \code{column_db}: input added as a possibility to keep on the same format
#'     as \code{\link{OK_column_standards}}. Not necessary to input.
#' \item \code{table_db}: input added as a possibility to keep on the same format
#'     as \code{\link{OK_column_standards}}. Must be the same as
#'     \code{dbsource}. Not necessary to input.
#' }
#'
#' All vectors must have the same order and the same length.
#'
#' When \code{calculate_sum} is \code{TRUE}, a line with the sum will be appended.
#'    The default is to calculate the sum of the column "ant_prover". If the sum
#'    should be calculated for one or more other columns, you may give the column
#'    names as input to the argument \code{column} that will be passed to
#'    \code{\link{append_sum_line}}. The sum will only be appended for columns
#'    that exist in the data.
#'
#' When more than one worksheet should be added to a single workbook,
#'     use \code{add_worksheet = FALSE} for the first worksheet and
#'     \code{add_worksheet = TRUE} for the consecutive worksheet(s).
#'
#' @param data [\code{data.frame}]\cr
#'     The sampling plan with the units to be reported.
#' @param sheet [\code{character(1)}]\cr
#'     The name of the Excel sheet.
#' @param filename [\code{character(1)}]\cr
#'     The name of the Excel file.
#' @param filepath [\code{character(1)}]\cr
#'     The path to the Excel file.
#' @param column_standards [\code{data.frame} | \code{list} | \code{character(1)}]\cr
#' The column standards to be used as input for
#'     \ifelse{html}{\code{\link[NVIdb]{standardize_columns}}}{\code{NVIdb::standardize_columns}}
#'     when formatting the sampling plan for output, see details. Defaults to
#'     \code{\link{OK_column_standards}}.
#' For giving alternatives to the standard table for column_standards using
#'     different formats, see details. Defaults to
#'     file.path(NVIdb::set_dir_NVI("ProgrammeringR", slash = FALSE),
#'               "standardization", "colnames", "column_standards.csv").
#' @param calculate_sum [\code{logical(1)}]\cr
#'     Should a line with the sum be appended? Defaults to \code{TRUE}.
#' @param footnote [\code{character(1)}]\cr
#'     Footnote to appended? Defaults to \code{NULL}.
#' @param footnote_heights [\code{integer(1)}]\cr
#'     Manually set row height for the footnote. Defaults to \code{NULL}.
#' @param dbsource [\code{character(1)}]\cr
#'     The name of the dbtable in \code{\link{OK_column_standards}} that should
#'     be used for standardising and formatting the sampling plan output.
#' @param add_worksheet [\code{logical(1)}]\cr
#'     Should a worksheet be added to an existing workbook? Defaults to
#'     \code{FALSE}.
#' @param \dots Other arguments to be passed to \code{\link{append_sum_line}}.
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
                                    add_worksheet = FALSE,
                                    ...) {

  # PREPARE ARGUMENTS BEFORE ARGUMENT CHECKING ----
  # Remove trailing backslash or slash before testing path
  filepath <- sub("\\\\{1,2}$|/{1,2}$", "", filepath)
  dots <- list(...)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  # data
  checkmate::assert_data_frame(data, max.rows = (1048576 - 1), max.cols = 16384, add = checks)
  # filename and filepath
  checkmate::assert_character(sheet, min.chars = 1, min.len = 1, max.len = length(data), unique = TRUE, add = checks)
  checkmate::assert_character(filename, min.chars = 1, len = 1, add = checks)
  checkmate::assert_directory_exists(filepath, add = checks)
  if (isTRUE(add_worksheet)) {
    checkmate::assert_file_exists(file.path(filepath, filename), access = "r", add = checks)
  }
  # column_standards
  # checkmate::assert(checkmate::check_class(column_standards, classes = c("data.frame")),
  #                   checkmate::check_class(column_standards, classes = c("list")),
  #                   add = checks)
  # if (inherits(column_standards, what = "list")) {
  #   lengths_standard <- lengths(column_standards)
  #   NVIcheckmate::assert_integer(lengths_standard, lower = lengths_standard[1], upper = lengths_standard[1],
  #                                min.len = 3, max.len = 6,
  #                                comment = "When input as a list, all elements must have the same length",
  #                                add = checks)
  #
  #   checkmate::assert_subset(names(column_standards), choices = c("table_db", "colname_db", "colname", "collabel", "colwidth", "colorder"),
  #                            add = checks)
  # }
  # if (inherits(column_standards, what = "data.frame")) {
  #   checkmate::assert_data_frame(column_standards, min.rows = 1, min.cols = 6, add = checks)
  # }
  checkmate::assert(checkmate::check_class(column_standards, classes = c("data.frame")),
                    checkmate::check_class(column_standards, classes = c("list")),
                    checkmate::check_class(column_standards, classes = c("character")),
                    add = checks)
  if (inherits(column_standards, what = "character")) {
    checkmate::assert_file_exists(column_standards, add = checks)
  }
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
  # calculate_sum
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

  # TRANSFORM column_standards FROM list TO data.frame
  # if (inherits(column_standards, what = "list")) {
  #   column_standards <- as.data.frame((column_standards))
  #
  #   if (!"table_db" %in% colnames(column_standards)) {
  #     column_standards$table_db <- dbsource
  #   }
  #
  #   if (!"colname_db" %in% colnames(column_standards)) {
  #     column_standards$colname_db <- column_standards$colname
  #   }
  #
  #   if (!"colorder" %in% colnames(column_standards)) {
  #     column_standards$colorder <- c(1:dim(column_standards)[1])
  #   }
  #   colnames(column_standards)[which(colnames(column_standards) == "collabel")] <- "label_1_no"
  #   colnames(column_standards)[which(colnames(column_standards) == "colwidth")] <- "colwidth_Excel"
  # }

  # GENERATE EXCEL WORKBOOK ----
  # create or load workbook
  if (isTRUE(add_worksheet)) {
    okwb <- openxlsx::loadWorkbook(xlsxFile = file.path(filepath, filename))
  } else {
    okwb <- openxlsx::createWorkbook()
  }

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
    if ("column" %in% names(dots)) {
      column <- dots$column
      } else {column <- "ant_prover"}
    column <- intersect(column, colnames(data))
    if (length(column) > 0) {
      okdata <- append_sum_line(data = okdata, column = column, position = "left")
    }
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

  # SAVE EXCEL WORKBOOK ----
  openxlsx::saveWorkbook(wb = okwb, file = file.path(filepath, filename), overwrite = TRUE)
}
