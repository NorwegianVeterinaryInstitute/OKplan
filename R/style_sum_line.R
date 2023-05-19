#' @title Style row with "Sum" in an Excel sheet
#' @description Style the font to bold for the row with the text "Sum" in one
#'     cell. It is possible to use other text decoration, see
#'     \ifelse{html}{\code{\link[openxlsx]{createStyle}}}{\code{openxlsx::createStyle}}.
#'     A line with the text "Sum" or another text as given as input to
#'     \code{text} will be styled.
#'
#' @details The whole line will be styled.
#'
#' @param workbook [\code{Workbook}]\cr
#'     A workbook object containing a worksheet.
#' @param sheet [\code{character(1)}]\cr
#'     The Excel sheet name.
#' @param data [\code{data.frame}]\cr
#'     The data that have been exported to the Excel sheet. Used to find column
#'     number and row number for the pretext for which the row should be styled.
#' @param text [\code{character(1)}]\cr
#'     The text in the cell for which the row should be styled.
#'     Defaults to "Sum".
#' @param text_decoration [\code{character(1)}]\cr
#'     The text decoration style that should be used, see
#'     \ifelse{html}{\code{\link[openxlsx]{createStyle}}}{\code{openxlsx::createStyle}}.
#'     Defaults to "bold".
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[openxlsx]{addStyle}}}{\code{openxlsx::addStyle}}.
#'
#' @return None. One row in the workbook object is styled.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export

style_sum_line <- function(workbook = workbook,
                           sheet = sheet,
                           data,
                           text = "Sum",
                           text_decoration = "bold",
                           ...) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_class(workbook, classes = "Workbook", add = checks)
  checkmate::assert_character(sheet, len = 1, min.chars = 1, add = checks)
  checkmate::assert_data_frame(data, add = checks)
  checkmate::assert_character(text, len = 1, add = checks)
  checkmate::assert_character(text_decoration, len = 1, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # STYLING ----
  # Style a row in the Excel sheet with the given text in a cell
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = openxlsx::createStyle(textDecoration = text_decoration),
                     cols = 1:dim(data)[2],
                     rows = which(data == text, arr.ind = TRUE)[1] + 1,
                     ...)
}
