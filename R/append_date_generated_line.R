#' @title Add new last row with the generated date
#' @description Function to add new row with the generated date. Before the date
#'     a pretext can be set.
#'
#' @details Two rows are added to the data frame, the first is empty, the second
#'     has the generated date in the first column.
#'
#' @param data [\code{data.frame}]\cr
#'     The data to which an additional row with the generate date should be added.
#' @param pretext [\code{character}]\cr
#'     The explaining text before the date value. Defaults to "Datauttrekket ble gjort".
#' @param date [\code{character}]\cr
#'     Date for generating the data. Defaults to
#'     \ifelse{html}{\code{\link[base]{Sys.Date}}}{\code{base::Sys.Date}}.
#'
#' @return \code{data.frame} with two additional rows, one empty and one with the generated date in the first column.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Add row with generated date using standard values
#' gris_virus_slaktegris_utvalg <- append_date_generated_line(gris_virus_slaktegris_utvalg)
#' }
#'
append_date_generated_line <- function(data,
                                       pretext = "Datauttrekket er gjort",
                                       date = format(Sys.Date(), "%d/%m/%Y")) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform assertions
  checkmate::assert_data_frame(data, add = checks)
  checkmate::assert_character(pretext, add = checks)
  checkmate::assert_character(date, add = checks)
  # Report errors
  checkmate::reportAssertions(checks)

  # APPEND LINE WITH GENERATED DATE ----
  # Transforms to data frame as this is more flexible than tibble, i.e. accepts
  #     adding character to numeric variable
  data <- as.data.frame(data)

  # Includes two empty rows at end of data frame
  data[(dim(data)[1] + 1):(dim(data)[1] + 2), ] <- NA

  # input time for generating the data
  data[dim(data)[1], 1] <- trimws(paste(pretext, date))

  return(data)
}
