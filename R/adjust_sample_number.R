#' @title Adjust the sample size per selected unit
#' @description Adds new column with an adjusted the sample size per selected
#'      unit. The total sample size is adjusted to be in accord with the total
#'      budgetted sample size.
#'
#' @details The sample size should first be estimated by percentage or similar.
#'
#' @param data Data frame
#' @param budget The total budgeted sample number.
#' @param col_estimated The name of the column with the sample size per unit that should be adjusted.
#' @param col_adjusted The name of the column with the adjusted sample size per unit.
#' @param adjust_by The maximum number of samples that one should adjust by.
#'
#' @return A data frame with a new column with an adjusted sample number.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#'

adjust_sample_number <- function(data, budget, col_estimated, col_adjusted = "justert_ant_prover", adjust_by) {

  # Justerer prøvetallet opp eller ned
  total_estimated <- sum(data[, col_estimated], na.rm = TRUE)

  difference <- c(as.numeric(total_estimated - budget) ,rep(NA, dim(data)[1] - 1))

  # Order data with largest sample size first
  data <- data[order(data[, col_estimated], decreasing = TRUE), ]

  # Only justify sample number when there is disagreement between budget and calculated number of samples
  if (total_estimated != budget) {
    # Adjust for each sampled unit with the unit having the largest sample size first
    for (i in c(1:dim(data)[1])) {

      # Justify by positive or negative number depending on whether sample size is too small or too large.
      # If the difference is larger than adjust_by, then adjust by adjust_by
      # Else adjust by 1 | -1
      # If no difference "adjust by" 0
      if (abs(difference[i]) > adjust_by) {
        justify <- ifelse(difference[i] > 0, -adjust_by, adjust_by)
      } else {
        justify <- ifelse(difference[i] > 0, -1, 1)
      }
      if (difference[i] == 0) {justify <- 0}

      # Make new column with
      data[i, col_adjusted] <- data[i, col_estimated] + justify
      if (i < dim(data)[1]) {
        difference[i+1] <- difference[i] + justify
      }

    }
  }
  return(data)
}
