#' @title Writes the surveillance selection to a standardised file.
#'
#' @description The surveillance selection is written to a standardised file.
#'     The file is standardised to include a standard set of columns and have
#'     the records in a standard order.
#' @details The data is saved as an "okplan" file that will be used the source
#'     file when the selection list is generated.
#'
#'     The function uses
#'     \ifelse{html}{\code{\link[NVIdb:standardize_columns]{NVIdb::standardize_columns}}}{\code{NVIdb::standardize_columns}}.
#'     to select and order the columns. The formatting information is taken from
#'     \code{\link{OK_column_standards}}.
#'
#' @param data [\code{data.frame}]\cr
#'     The sampling plan with the units to be reported.
#' @param filename [\code{character(1)}]\cr
#'     The name of the csv file including extension.
#' @param filepath [\code{character(1)}]\cr
#'     The path to the csv file.
#' @param sortvar [\code{character}]\cr
#'     The sort order for the records in the csv-file. Defaults to
#'     c("ok_hensiktkode", "ok_driftsformkode", "statuskode",
#'       "prioritet_av_reserve", "eier_lokalitetnr")
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[utils:write.csv2]{utils::write.csv2}}}{\code{utils::write.csv2}}.
#'
#' @return None. Saves a data frame with the selection in a standard csv file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' library(OKplan)
#' td <- tempdir()
#' okplan <- as.data.frame(list("ok_hensiktkode" = c("01002", "01002"),
#'                              "ok_driftsformkode" = c("010202", "010202"),
#'                              "statuskode" = c(1, 1),
#'                              "prioritet_av_reserve" = c(NA, NA),
#'                              "eier_lokalitetnr" = c("1101123456", "1102123456")))
#' write_okplan(data = okplan,
#'              filename = "okplan_species_disease",
#'              filepath = td)
#' }
save_okplan <- function(data,
                         filename,
                         filepath,
                         sortvar = c("ok_hensiktkode", "ok_driftsformkode",
                                     "statuskode", "prioritet_av_reserve",
                                     "eier_lokalitetnr"),
                         ...) {

  # PREPARE ARGUMENTS BEFORE ARGUMENT CHECKING ----
  ## Remove trailing backslash or slash before testing path
  filepath <- sub("\\\\{1,2}$|/{1,2}$", "", filepath)

  # ARGUMENT CHECKING ----
  ## Object to store check-results
  checks <- checkmate::makeAssertCollection()
  ## Perform checks
  ### data
  checkmate::assert_data_frame(data, add = checks)
  checkmate::assert_character(filename, min.chars = 1, len = 1, add = checks)
  ### filename and filepath
  checkmate::assert_character(filename, min.chars = 1, len = 1, add = checks)
  checkmate::assert_directory_exists(filepath)
  ### order
  checkmate::assert_subset(sortvar, choices = colnames(data), add = checks)
  ## Report check-results
  checkmate::reportAssertions(checks)


  # STANDARDISE AND SAVE DATA TO csv-FILE ----
  ## Standardise column order, exclude columns
  data <- NVIdb::standardize_columns(data,
                                     dbsource = "okplan",
                                     standards = OKplan::OK_column_standards,
                                     property = "colorder",
                                     exclude = TRUE)

  ## Sort data in the order given by sortvar
  data <- data[do.call(base::order, data[, sortvar]), ]

  ## write the data to the csv-file
  utils::write.csv2(data,
                    file = file.path(filepath, filename),
                    row.names = FALSE,
                    ...)
}
