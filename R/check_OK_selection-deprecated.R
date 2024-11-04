#' @title Control of standard output file with OK selection
#' @description Standard control by performing descriptive statistics of variables in the output file with OK selection.
#'
#' @details Under development. This should be rewritten to produce nicer output.
#'
#'     Gives descriptive statistics of the selection. This is used to see if the number of selected units per category are correct.
#'     If any mistakes are found, one must go back and correct in the script that produces the selection.
#'
#'     Before the control is run, the column names must have been standardized using \code{NVIdb::standardize_columns}.
#'
#' @param data Data frame with selection for a OK programme.
#'
#' @return Prints results of the control to the output window.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name check_OK_selection-deprecated
#' @usage check_OK_selection(data)
#' @keywords internal
#' @examples
#' \dontrun{
#' # Checking OK selection data in data frame okplan_MRSA
#' check_OK_selection(okplan_MRSA)
#' }
#'
NULL




#' @title check_OK_selection is Deprecated
#' @description \code{check_OK_selection} was deprecated 2022-12-15 to replace it
#'     with check_ok_selection with a standardised function name with lower case
#'     letters for OK. Use check_ok_selection with the additional parameters
#'     \code{purpose =} and \code{plan_aar =}.
#' @details The old help pages can be found at \code{help("check_OK_selection-deprecated")}.
#'     Information on deprecated function can be found at \code{help("OKplan-deprecated")}.
#' @param data The table with data describing the selection for a OK programme.
#' @param purpose String with descriptive text to be used in file name and
#'     heading of the report. Defaults to name of input data.
#' @param plan_aar The year for which the
#'     selection is planned. Defaults to next year.
#' @export
#' @rdname check_OK_selection-old
#'
check_OK_selection <- function(data,
                               purpose = deparse(substitute(data)),
                               plan_aar = as.numeric(format(Sys.Date(), "%Y")) + 1) {

  .Deprecated(new = "check_ok_selection",
              package = "OKplan",
              msg = paste("'check_OK_selection' is replaced by 'check_ok_selection'",
                          "to improve functionality and standarise name to lower",
                          "case. Remember to set the new input parameters 'purpose'",
                          "and 'plan_aar' when using' check_ok_selection'."))

  check_ok_selection(data = data,
                     purpose = purpose,
                     plan_aar = plan_aar)
}
