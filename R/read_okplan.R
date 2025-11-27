#' @title Reads the surveillance selection from a standardised file.
#' @description Reads the surveillance selection from a standardised file. The
#'     file has previously been saved with \code{save_okplan}. This should have
#'     been standardised to include a standard set of columns and have
#'     the records in a standard order.
#' @details The data is an "okplan" csv-file.
#'     \ifelse{html}{\code{\link[NVIdb]{standardize_columns}}}{\code{NVIdb::standardize_columns}}
#'     is used to standardise identify character columns which is used as input
#'     to \code{colClasses} in
#'     \ifelse{html}{\code{\link[utils]{read.csv2}}}{\code{utils::read.csv2}}.
#' @param filename [\code{character(1)}]\cr
#'     The name of the csv file including extension.
#' @param filepath [\code{character(1)}]\cr
#'     The path to the csv file.
#' @param column_standards [\code{data.frame} | \code{list} | \code{character(1)}]\cr
#' The column standards to be used as input for
#'     \ifelse{html}{\code{\link[NVIdb]{standardize_columns}}}{\code{NVIdb::standardize_columns}}
#'     when formatting the sampling plan for output, see details. Defaults to
#'     \code{\link{OK_column_standards}}.
#' @param dbsource [\code{character(1)}]\cr
#'     The name of the dbtable in \code{\link{OK_column_standards}} that should
#'     be used for standardising and formatting the sampling plan output.
#'     Defaults to \code{"okplan"}.
#' @return \code{data.frame}.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' library(OKplan)
#' td <- tempdir()
#' okplan <- data.frame("ok_hensiktkode" = c("01002", "01002"),
#'                      "ok_driftsformkode" = c("010202", "010202"),
#'                      "statuskode" = c(1, 1),
#'                      "prioritet_av_reserve" = c(NA, NA),
#'                      "eier_lokalitetnr" = c("1101123456", "1102123456"))
#' save_okplan(data = okplan,
#'             filename = "okplan_species_disease",
#'             filepath = td)
#' okplan <- read_okplan(filename = "okplan_species_disease",
#'                       filepath = td)
#' }
read_okplan <- function(filename,
                        filepath,
                        column_standards = OKplan::OK_column_standards,
                        dbsource = "okplan") {
  
  # PREPARE ARGUMENTS BEFORE ARGUMENT CHECKING ----
  ## Remove trailing backslash or slash before testing path
  filepath <- sub("\\\\{1,2}$|/{1,2}$", "", filepath)
  
  # ARGUMENT CHECKING ----
  ## Object to store check-results
  checks <- checkmate::makeAssertCollection()
  ## Perform checks
  ### filename and filepath
  checkmate::assert_character(filename, min.chars = 1, len = 1, add = checks)
  checkmate::assert_directory_exists(filepath)
  ### column_standards
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
  ### dbsource
  checkmate::assert_character(dbsource, min.len = 1, add = checks)
  if (inherits(column_standards, what = "data.frame")) {
    checkmate::assert_choice(dbsource,
                             choices = unique(column_standards[, "table_db"]),
                             add = checks)
  }
  ## Report check-results
  checkmate::reportAssertions(checks)
  
  # READ STANDARD okplan csv-FILE ----
  # read the colclasses of the file to identify character variables
  colclasses <- NVIdb::standardize_columns(
    data = file.path(filepath, filename),
    standards = column_standards,
    dbsource = dbsource,
    property = "colclasses")
  
  # read the okplan file
  okplan <- utils::read.csv2(
    file = file.path(filepath, filename),
    colClasses = colclasses,
    fileEncoding = "UTF-8")
  
  return(okplan)
}
