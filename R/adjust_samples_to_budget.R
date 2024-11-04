#' @title Adjust the sample size per selected unit
#' @description Adds a new column with an adjusted sample number per selected
#'      unit. The total sample size is adjusted to be in accord with the total
#'      budgeted sample number.
#'
#' @details The sample number per unit should first have been estimated, for
#'     example can the sample number per abattoir be the total number of
#'     samples distributed on the abattoirs in accord with the slaughter volume
#'     at each abattoir. Often will rounding errors lead to a difference between
#'     the total budgeted sample number and the total estimated sample number.
#'     Therefore, the estimated sample number need to be adjusted.
#'
#'     The estimated sample number is first adjusted for the unit with the
#'     largest sample number. Thereafter, for the unit with the next largest
#'     sample number and so on.
#'
#'     The sample number will often be estimated so that it is a multiplicand
#'     multiplied by of a given number (multiplier). For example, if equal
#'     number of samples should be taken every month the multiplier can be 12,
#'     if the samples are pooled five and five, the multiplier can be 5. If the
#'     argument \code{adjust_by} is given the multiplier, the sample number will
#'     be adjusted by the multiplier unless the difference that should be adjusted
#'     is less than the multiplier. In that case, the sample number will be
#'     adjusted by a number less than the \code{adjust_by}.
#'
#' @param data [\code{data.frame}]\cr
#'     Data including a column with the sample number that should be adjusted.
#' @param group [\code{character}]\cr
#'     Vector with group variables. Defaults to \code{NULL}.
#' @param budget [\code{numeric(1)} | \code{character(1)}]\cr
#'     The total budgeted sample number or a column in data with the budget
#'     number of samples (per group).
#' @param sample_to_adjust [\code{character(1)}]\cr
#'     The name of the column with the sample number per unit that should be
#'     adjusted.
#' @param adjusted_sample [\code{character(1)}]\cr
#'     The name of the new column with the adjusted sample number per unit.
#'     Defaults to "justert_ant_prover".
#' @param adjust_by [\code{numeric(1)}]\cr
#'     The maximum number of samples that a sample can be adjust by. Defaults
#'     to 1.
#'
#' @return A data frame with a new column with an adjusted sample number.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @importFrom rlang .data
#' @export
#' @examples
#' library(OKplan)
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
#' # Adjust total sample number to budget per group
#' total_budget <- 60 # same budget for all groups
#'
#' # Add data frame with sample number to adjust
#' x <- as.data.frame(cbind(c(1:10),
#'                          c(rep("x", 5), rep("y", 5)),
#'                          c(24, 18, 6, 0, 30, 36, 12, 6, 0, 0)))
#' colnames(x) <- c("id", "xy", "sample")
#'
#' x2 <- adjust_samples_to_budget(data = x,
#'                                group = "xy",
#'                                budget = total_budget,
#'                                sample_to_adjust = "sample",
#'                                adjusted_sample = "new_sample",
#'                                adjust_by = 6)
adjust_samples_to_budget <- function(data,
                                     group = NULL,
                                     budget,
                                     sample_to_adjust,
                                     adjusted_sample = "justert_ant_prover",
                                     adjust_by = 1) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform assertions
  checkmate::assert_data_frame(x = data, all.missing = FALSE, add = checks)
  checkmate::assert_subset(group, choices = colnames(data), add = checks)
  checkmate::assert(checkmate::check_integerish(budget, lower = 1, len = 1, any.missing = FALSE),
                    checkmate::check_choice(budget, choices = colnames(data)),
                    add = checks)
  checkmate::assert_choice(sample_to_adjust, choices = colnames(data), add = checks)
  checkmate::assert_character(adjusted_sample, len = 1, min.chars = 1, any.missing = FALSE, add = checks)
  checkmate::assert_integerish(adjust_by, lower = 1, len = 1, any.missing = FALSE, add = checks)
  # Report errors
  checkmate::reportAssertions(checks)

  # INITILIZE VARIABLES ----
  nogroup <- rep("x", dim(data)[1])
  if (budget %in% colnames(data)) {
    difference <- cbind(data[, c(sample_to_adjust, budget)],
                        nogroup)
  } else {
    budget <- rep(budget, dim(data)[1])
    difference <- cbind(data[, sample_to_adjust], budget,
                        nogroup)
    colnames(difference)[1] <- sample_to_adjust
  }
  if (!is.null(group)) {
    difference <- cbind(difference, data[, group])
    colnames(difference)[(dim(difference)[2] - length(group) + 1):dim(difference)[2]] <- group
  }
  difference <- as.data.frame(difference)
  difference[, sample_to_adjust] <- as.numeric(difference[, sample_to_adjust])
  group <- c("nogroup", group)

  difference <- difference |>
    dplyr::mutate(original_order = 1:dplyr::n()) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group, sample_to_adjust)))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::mutate(total_estimated = sum(dplyr::across(dplyr::all_of(sample_to_adjust)), na.rm = TRUE)) |>
    dplyr::mutate(included = dplyr::case_when(dplyr::across(dplyr::all_of(sample_to_adjust)) > 0 ~ 1,
                                              TRUE ~ 0)) |>
    # dplyr::mutate(n_units = sum(.data$included, na.rm = TRUE)) |>
    dplyr::mutate(n_units = sum(dplyr::across(dplyr::all_of("included")), na.rm = TRUE)) |>
    dplyr::mutate(difference = .data$total_estimated - as.numeric(.data$budget)) |>
    # dplyr::mutate(difference = as.numeric(dplyr::all_of("total_estimated")) - as.numeric(dplyr::all_of("budget"))) |> # This don't work
    dplyr::ungroup() |>
    # dplyr::group_by(dplyr::across(dplyr::all_of(group)), .data$included) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group, "included")))) |>
    dplyr::mutate(n_seq = 1:dplyr::n()) |>
    dplyr::ungroup()


  # ADJUST SAMPLE NUMBER ----
  # If total_estimated = budget, make new column adjusted_sample based on sample_to_adjust
  # if (total_estimated == budget) {
  if (all(difference == 0)) {
    data[, adjusted_sample] <- data[, sample_to_adjust]
  }

  # Only justify sample number when there is disagreement between budget and calculated number of samples
  #  if (total_estimated != budget) {
  if (any(difference != 0)) {
    # Adjust for each sampled unit with the unit having the largest sample size last
    # for (i in 1:c(dim(data)[1])) {
    for (i in c(dim(data)[1]:1)) {
      # i <- 45
      # Justify by positive or negative number depending on whether sample size is too small or too large.
      # If the difference is larger than adjust_by, then adjust by adjust_by
      # Else adjust by 1 | -1
      # If no difference "adjust by" 0
      if (abs(difference[i, "difference"]) >= adjust_by) {
        justify <- ifelse(difference[i, "difference"] > 0, -adjust_by, adjust_by)
      } else {
        justify <- ifelse(difference[i, "difference"] > 0,
                          floor(-difference[i, "difference"] / (difference[i, "n_seq"])),
                          ceiling(-difference[i, "difference"] / (difference[i, "n_seq"])))
      }
      if (difference[i, "difference"] == 0) {justify <- 0}

      # Make new column with adjusted number
      #     data[i, adjusted_sample] <- data[i, sample_to_adjust] + justify
      difference[i, adjusted_sample] <- difference[i, sample_to_adjust] + justify
      #     if (i < dim(data)[1]) {
      if (i > 1) {
        # if (is.null(group) || identical(data[i, group], data[i + 1, group])) {
        #   difference[i + 1] <- difference[i] + justify
        j <- i
        while ((is.null(group) & j >= 1) || (identical(difference[j, group], difference[j - 1, group]) & j >= 1)) {
          if (j == i) {difference[j - 1, "difference"] <- difference[j, "difference"] + justify}
          if (j != i) {difference[j - 1, "difference"] <- difference[j, "difference"]}
          j <- j - 1
        }
      }
    }
    difference <- difference[order(difference$original_order), ]
    data[, adjusted_sample] <- difference[, adjusted_sample]
  }

  # RETURN RESULT ----
  return(data)
}
