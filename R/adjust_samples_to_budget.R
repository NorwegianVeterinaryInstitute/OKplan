#' @title Adjust the sample size per selected unit
#' @description Adds new column with an adjusted the sample size per selected
#'      unit. The total sample size is adjusted to be in accord with the total
#'      budgeted sample size.
#'
#' @details The sample size should first be estimated by percentage or similar.
#'
#' @param data Data frame
#' @param budget The total budgeted sample number.
#' @param sample_to_adjust The name of the column with the sample number per unit that should be adjusted.
#' @param adjusted_sample The name of the column with the adjusted sample number per unit.
#' @param adjust_by The maximum number of samples that one should adjust by.
#'
#' @return A data frame with a new column with an adjusted sample number.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # Add data frame with sample number to adjust
#' x <- as.data.frame(cbind(c(1:10),
#'                          c(24, 30, 36, 12, 6, 18, 6, 0, 0, 0)))
#' colnames(x) <- c("id", "sample")
#'
#' # Adjust total sample number to budget
#' x <- adjust_samples_to_budget(data = x,
#'                               budget = 150,
#'                               sample_to_adjust = "sample",
#'                               adjusted_sample = "new_sample",
#'                               adjust_by = 4)
#'

adjust_samples_to_budget <- function(data, budget, sample_to_adjust, adjusted_sample = "justert_ant_prover", adjust_by) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform assertions
  checkmate::assert_data_frame(x = data, all.missing = FALSE, add = checks)
  checkmate::assert_integerish(budget, lower = 1, len = 1, any.missing = FALSE, add = checks)
  checkmate::assert_choice(sample_to_adjust, choices = colnames(data), add = checks)
  checkmate::assert_character(adjusted_sample, len = 1, min.chars = 1, any.missing = FALSE, add = checks)
  checkmate::assert_integerish(adjust_by, lower = 1, len = 1, any.missing = FALSE, add = checks)
  # Report errors
  checkmate::reportAssertions(checks)

  # INITILIZE VARIABLES ----
  total_estimated <- sum(data[, sample_to_adjust], na.rm = TRUE)
  n_units <- length(which(data[, sample_to_adjust] > 0))
  difference <- c(as.numeric(total_estimated - budget) ,rep(NA, dim(data)[1] - 1))

  # ADJUST SAMPLE NUMBER ----
  # Order data with largest sample size first
  data <- data[order(data[, sample_to_adjust], decreasing = TRUE), ]

  # Only justify sample number when there is disagreement between budget and calculated number of samples
  if (total_estimated != budget) {
    # Adjust for each sampled unit with the unit having the largest sample size first
    for (i in c(1:dim(data)[1])) {

      # Justify by positive or negative number depending on whether sample size is too small or too large.
      # If the difference is larger than adjust_by, then adjust by adjust_by
      # Else adjust by 1 | -1
      # If no difference "adjust by" 0
      if (abs(difference[i]) >= adjust_by) {
        justify <- ifelse(difference[i] > 0, -adjust_by, adjust_by)
      } else {
        justify <- ifelse(difference[i] > 0,
                          floor(- difference[i] / (n_units - i)),
                          ceiling(- difference[i] / (n_units - i)))
      }
      if (difference[i] == 0) {justify <- 0}

      # Make new column with adjusted number
      data[i, adjusted_sample] <- data[i, sample_to_adjust] + justify
      if (i < dim(data)[1]) {
        difference[i+1] <- difference[i] + justify
      }

    }
  }
  # RETURN RESULT ----
  return(data)
}