#' @title Append row with column sums
#' @description Appends a new row with column sums for selected columns. A pretext can be placed on the row.
#'
#' @details One row is appended to the data frame. The sum is calculated with na.rm = TRUE.
#'
#'     If a tibble, it is transformed to a data frame to avoid errors if the pretext is to be placed in a numeric variable.
#'
#' @param data Data frame to which a row should be appended.
#' @param column Character vector. The column names of columns to sum.
#' @param pretext The explaining text before the sum, defaults to "Sum".
#' @param position The position for the pretext, on of c("first", left", "none"). defaults to left.
#'
#' @return A data frame with an appended row with sums.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Append row with sum
#' gris_blodprover_slakteri <- append_sum_line(data = gris_blodprover_slakteri,
#'                                             col_name = c("ant_prover"),
#'                                             pretext = "Sum",
#'                                             position = "first")
#' }
#'


append_sum_line <- function(data, column, pretext = "Sum", position = "left") {

  # Removes tibble as tibble will not accept the the pretext (character variable) in a numeric variable
  data <- as.data.frame(data)

  # Sum for one or more columns
  if (length(column) == 1) {
    sum_column <- unname(sum(data[, column], na.rm = TRUE))
  } else {
    sum_column <- unname(colSums(data[, column], na.rm = TRUE))
  }

  # Append a line with the sum. The pretext is placed in accord with position
  if (position == "none") {
    data[dim(data)[1] + 1, c(column)] <- c(sum_column)
    }
  if (position == "first") {
    data[dim(data)[1] + 1, c(colnames(data)[1], column)] <- c(pretext, sum_column)
    }
  if (position == "left") {
    data[dim(data)[1] + 1, c((colnames(data)[which(colnames(data) == column[1]) - 1]), column)] <- c(pretext, sum_column)
  }

  # RETURN RESULTS ----
  return(data)
}
