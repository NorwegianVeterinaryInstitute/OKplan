#' @title Checks the standard output data frame with the OK selection
#' @description Standard checks by performing descriptive statistics of
#'     variables in the standard output data frame with OK selection.
#'     \code{check_ok_selection} is a wrapper for
#'     \ifelse{html}{\code{\link[NVIbatch]{output_rendered}}}{\code{NVIbatch::output_rendered}}.
#'
#' @details Gives descriptive statistics of the OK selection. This should
#'     used to check if the number of selected units per category are
#'     in accord with the design of the surveillance programme. If any
#'     mistakes are found, one must correct in the script that generates
#'     the selection.
#'
#'     The check must be performed on a data frame with standardised
#'     column names. This is ensured by using column names as defined for
#'     "okplan" in \code{\link{OK_column_standards}}.
#'
#'     The default behavior is to display the resulting html-file in the
#'     browser. To save the result in a permanent file, use a permanent
#'     directory as input to \code{output_dir}. The resulting file
#'     can also be sent by email by using additional arguments, see
#'     \ifelse{html}{\code{\link[NVIbatch]{output_rendered}}}{\code{NVIbatch::output_rendered}}.
#'
#'     If checks are missing, are unnecessary or the headings are too
#'     cryptic, please contribute to improve the rmarkdown file
#'     "check_ok_selection.Rmd", see
#'     \href{../doc/Contribute_to_OKplan.html}{\code{vignette("Contribute_to_OKplan", package = "OKplan")}}.
#'
#' @param input [\code{character(1)}]\cr
#'     The path to the rmarkdown document with the checks.
#'     Defaults to "check_ok_selection.Rmd" in the \code{OKplan}.
#' @param output_file [\code{character(1)}]\cr
#'     The name of the output file.
#' @param output_dir [\code{character(1)}]\cr
#'     The directory to save the output file. Defaults to \code{NULL}.
#' @param data [\code{data.frame}]\cr
#'     The table with data describing the selection for a OK programme.
#' @param purpose [\code{character(1)}]\cr
#'     String with descriptive text to be used in file name and heading of the
#'     report.
#' @param plan_aar [\code{numeric(1)}]\cr
#'     The year for which the selection is planned. Defaults to next year.
#' @param display [\code{logical(1)} | \code{character(1)}]\cr
#'     Set "browser" for the default browser or "viewer" for the R studio
#'      viewer. `TRUE` equals "browser". If `FALSE`, don't display
#'      the results file. Defaults to "browser".
#' @param \dots Other arguments to be passed to
#'     \ifelse{html}{\code{\link[NVIbatch]{output_rendered}}}{\code{NVIbatch::output_rendered}}.
#'
#' @return Generates an html-file with the results of the checks to be displayed in the browser.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Checking OK selection data
#'
#' purpose = "ok_virus_svin"
#' plan_aar = 2023
#'
#' # Check
#' check_OK_selection(data = okplan_svin,
#'                    purpose = purpose,
#'                    plan_aar = plan_aar)
#' }
#'
check_ok_selection <- function(input = system.file('templates', "check_ok_selection.Rmd", package = "OKplan"),
                               output_file = paste0("Kontroll av okplan for ",
                                                    purpose,
                                                    " ",
                                                    format(Sys.Date(), "%Y%m%d"),
                                                    ".html"),
                               output_dir = NULL,
                               data = NULL,
                               purpose = NULL,
                               plan_aar = as.numeric(format(Sys.Date(), "%Y")) + 1,
                               display = "browser",
                               ...) {

  # PREPARE ARGUMENTS BEFORE CHECKING ----
  if (is.null(output_dir)) {output_dir = tempdir()}
  if (isTRUE(display)) {display = "browser"}

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_file(input, access = "r", add = checks)
  checkmate::assert_string(output_file, min.chars = 6, pattern = "\\.html$", ignore.case = TRUE, add = checks)
  checkmate::assert_directory(output_dir, access = "r", add = checks)
  checkmate::assert_data_frame(data, min.rows = 1, add = checks)
  checkmate::assert_string(purpose, min.chars = 1, add = checks)
  checkmate::assert_integerish(plan_aar,
                               lower = 1995, upper = (as.numeric(format(Sys.Date(), "%Y")) + 10),
                               any.missing = FALSE, all.missing = FALSE,
                               len = 1,
                               add = checks)
  checkmate::assert(checkmate::check_false(display),
                    checkmate::check_choice(display, choices = c("browser", "viewer")),
                    add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)


  # RUN output_rendered ----
  NVIbatch::output_rendered(input = input,
                            output_file = output_file,
                            output_dir = output_dir,
                            intermediates_dir = tempdir(),
                            params = list("data" = data, "purpose" = purpose, "plan_aar" = plan_aar),
                            display = display,
                            ...)
}
