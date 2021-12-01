#' @title Style cell background in Excel sheet based on column name
#' @description Style the cell background with different colours. The background
#'     colours are chosen based on the column names, for example "paavist" or
#'     "tvilsom". In each column, all cells that are following the given
#'     rule, will get the same background colour.
#'
#' @details The columns will be styled with the colour in the palette that have
#'     the same name as the column. If the column name is not the same as the
#'     name of the colour in the palette, the vector of column names can be named
#'     with the corresponding colour name, see example.
#'
#' @param workbook The workbook object.
#' @param sheet The Excel sheet name.
#' @param data The data frame that have been exported to the Excel sheet. Used to
#'     find column number and number of rows in the workbook that should be styled.
#'     Can be \code{NULL} if both \code{colnames_in_data} and \code{nrows_in_data}
#'     are different from \code{NULL}.
#' @param colnames_in_data The column names of the data frame that has been exported to
#'     Excel, i.e. \code{colnames(data)}. Can be \code{NULL} if \code{data} has a non-NULL
#'     argument. Values in \code{colnames_in_data} have preference for values in \code{data}.
#' @param nrows_in_data The number of rows of the data frame that has been exported to
#'     Excel, i.e. \code{nrow(data)}. Can be \code{NULL} if \code{data} has a non-NULL
#'     argument. Values in \code{nrows_in_data} have preference for values in \code{data}.
#' @param rule Rule that should be sourced to \code{openxlsx::conditionalFormatting}.
#'     Defaults to "!= 0".
#' @param colname Vector with colour names from the palette that should be used
#'     for styling the columns. If the column name is not the same as the colour
#'     name in the palette, the colour name should be named with the column name
#'     in the data frame.
#' @param palette Palette with named colours.
#'
#' @return None. One or more columns in the workbook object is styled.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export


style_sum_line <- function(workbook = workbook,
                                        sheet = sheet,
                                        data = NULL,
                                        colnames_in_data = NULL,
                                        nrows_in_data = NULL,
                                        rule,
                                        colname,
                                        palette) {

  # MANAGE INPUT ARGUMENTS BEFORE CHECKING ----
  # Gather colnames and nrows from data
  if (is.null(colnames_in_data)) {colnames_in_data <- colnames(data)}
  if (is.null(nrows_in_data)) {nrows_in_data <- nrow(data)}

  # make table with colour and columnname
  colname <- set_name_vector(colname)
  colours <- cbind(unname(colname), unname(palette[names(colname)]))
  colnames(colours) <- c("colname", "colour")

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_class(workbook, classes = "Workbook", add = checks)
  checkmate::assert_character(sheet, len = 1, min.chars = 1, add = checks)
  checkmate::assert_data_frame(data, null.ok = TRUE, add = checks)
  checkmate::assert_character(colnames_in_data, min.len = 1, null.ok = TRUE, add = checks)
  checkmate::assert_integerish(nrows_in_data, lower = 2, len = 1, null.ok = TRUE, add = checks)
  NVIcheckmate::assert_non_null(x = list(data, colnames_in_data), add = checks)
  NVIcheckmate::assert_non_null(x = list(data, nrows_in_data), add = checks)
  checkmate::assert_character(rule, len = 1, add = checks)
  checkmate::assert_character(colname, min.len = 1, add = checks)
  checkmate::assert_character(palette, min.len = 1, add = checks)
  NVIcheckmate::assert_character(x = intersect(names(colname), names(palette)), min.len = 1,
                                 comment = "At least one column must be associated with a colour in the palette",
                                 add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # RUNNING SCRIPT ----
  # Style each column with colname
  openxlsx::addStyle(wb = workbook,
                   sheet = paste0("Antall_svaber_lnn_storfe_", plan_aar),
                   style = openxlsx::createStyle(textDecoration = "bold"),
                   cols = 1:dim(slakteriliste)[2],
                   rows = which(slakteriliste == "Sum", arr.ind = TRUE)[1] + 1)

}
