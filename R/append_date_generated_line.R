#' @title Add new last row with generated date
#' @description Function to add new row with generated date. Before the date the pretext is "Datauttrekket ble gjort"
#'
#' @details Two rows are dded to the data frame, the first is empty, the second has the generated date in the first column. The
#'
#' @param data Data frame with data that should get new row with generated date
#' @param pretext The explaining text before the date value.
#' @param date Date for generating the data
#'
#' @return A data frame with two more rows, one empty and one with generated data in the first column.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Add row with generated date using standard values
#' gris_virus_slaktegris_utvalg <- append_date_generated_line(gris_virus_slaktegris_utvalg)
#' }
#'
append_date_generated_line <- function(data, pretext = "Datauttrekket er gjort", date = format(Sys.Date(), "%d/%m/%Y")) {

  # Argument checking
  checks <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = checks)
  checkmate::assert_character(pretext, add = checks)
  checkmate::assert_character(date, add = checks)
  checkmate::reportAssertions(checks)

  # Transforms to data frame as this is more flexible than tibble, i.e. accepts adding character to numeric variable
  data <- as.data.frame(data)

  # Includes two empty rows at end of data frame
  data[(dim(data)[1] + 1):(dim(data)[1] + 2), ] <- NA

  # input time for generating the data
  data[dim(data)[1], 1] <- trimws(paste(pretext, date))

  return(data)
}
