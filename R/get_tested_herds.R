#' @title Gets herds tested within an surveillance programme
#' @description Gets herds that have been sampled or tested within
#'     a surveillance programme for the selected years. You can
#'     choose between herds that have submitted samples or herds
#'     for which a certain number of samples have been examined
#'     for a specific disease.
#' @details For programmes having several surveillance streams, it is possible
#'     to select surveillance streams based on species and production type. The
#'     species and/or production type must be written as in the eos-table. Be
#'     aware that species and production type may be missing and for production
#'     type it may often be wrong. Therefore, selection by production type and/or
#'     species may remove saker that you would want to keep. No selection is
#'     performed when the species or production type is missing from the
#'     eos-table.
#'
#' It is possible to define a minimum requirement of number of
#'     samples received or tested. For programmes covering several
#'     infections, it is necessary to input the disease for which
#'     the samples should have been tested. The disease name must
#'     be given as it is written in the column name for the number
#'     of examined samples.
#'
#' The eos_table name is the same name as the table name in the EOS data base.

#' @param eos_table [\code{character(1)}]\cr
#'     EOS table name.
#' @param year [\code{numeric}]\cr
#'     One or more years that should be selected. Defaults
#'     to previous year.
#' @param species [\code{character}]\cr
#'     The species that should be selected. Defaults to \code{NULL}.
#' @param production [\code{character}]\cr
#'     The production type(s) for which number of tested samples should be
#'     calculated. Defaults to \code{NULL}.
#' @param disease [\code{character(1)}]\cr
#'     The disease for which number of tested samples should be calculated.
#'     Defaults to \code{NULL}.
#' @param min_prover [\code{numeric(1)}]\cr
#'     Minimum number of samples that should have been received or examined for
#'     the herd to be counted as sampled or tested. No check is performed if
#'     equal -1. Defaults to -1.
#' @param tested [\code{logical(1)}]\cr
#'     If \code{TRUE}, the number of tested samples, If \code{FALSE}, the number
#'     of received samples. Defaults to \code{FALSE}.
#' @return \code{data.frame} with tested or sampled locations.
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export

get_tested_herds <- function(eos_table,
                             year = as.numeric(format(Sys.Date(), "%Y")) - 1,
                             species = NULL,
                             production = NULL,
                             disease = NULL,
                             min_prover = -1,
                             tested = FALSE) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_string(eos_table, min.chars = 6, pattern = "^prove", ignore.case = TRUE, add = checks)
  checkmate::assert_integerish(year,
                               lower = 1995, upper = (as.numeric(format(Sys.Date(), "%Y"))),
                               any.missing = FALSE, all.missing = FALSE,
                               min.len = 1,
                               add = checks)
  checkmate::assert_character(species, min.len = 1, null.ok = TRUE, add = checks)
  checkmate::assert_character(production, min.len = 1, null.ok = TRUE, add = checks)
  checkmate::assert_string(disease, min.chars = 1, null.ok = TRUE, add = checks)
  checkmate::assert_integerish(min_prover,
                               lower = -1,
                               any.missing = FALSE, all.missing = FALSE,
                               len = 1,
                               null.ok = TRUE,
                               add = checks)
  checkmate::assert_flag(tested, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)


  dfx <- NVIdb::read_eos_data(eos_table = eos_table,
                              year = year)
  dfx$original_sort_order <- seq_len(nrow(dfx))

  dfx[which(nchar(dfx$eier_lokalitetnr) == 10), "eier_lokalitetnr"] <-
    substr(dfx[which(nchar(dfx$eier_lokalitetnr) == 10), "eier_lokalitetnr"], 1, 8)

  dfx <- subset(dfx, !is.na(dfx$eier_lokalitetnr) & dfx$eier_lokalitetnr != "")

  # Select species
  if (!is.null(species) & "art" %in% colnames(dfx)) {
    dfx <- subset(dfx, dfx$art %in% species)
  }
  # Select production type
  if (!is.null(production) & "driftsform" %in% colnames(dfx)) {
    dfx <- subset(dfx, dfx$driftsform %in% production)
  }

  if (dim(dfx)[1] > 0) {
    column_ant <- grep("ant_", colnames(dfx), value = TRUE)
    column_sum <- gsub("ant_", "sum_", column_ant)
    for (column_name in column_ant) {
      dfx[, column_name] <- as.numeric(dfx[, column_name])
    }

    agg_dfx <- stats::aggregate(x = dfx[, column_ant], by = list(dfx$eier_lokalitetnr), FUN = "sum", na.rm = TRUE)

    colnames(agg_dfx) <- c("eier_lokalitetnr", column_sum)
    dfx <- merge(dfx, agg_dfx, by = "eier_lokalitetnr")

    # Select herd above minimum number of samples
    if (min_prover > -1) {
      if (isFALSE(tested)) {
        if (any(grepl("sum_prover", column_sum))) {
          dfx <- subset(dfx, dfx$sum_prover >= min_prover)
        } else {
          if (length(column_sum) == 1) {
            dfx <- subset(dfx, dfx[, column_sum] >= min_prover)
            warning(paste("The number of received samples could not be calculated,",
                          "but the number of tested samples were calculated using",
                          paste(column_ant, collapse = ", ")))
          } else {
            stop(paste0("The number of received samples could not be calculated, ",
                        "and the number of tested samples were given in the columns ",
                        paste(column_ant, collapse = ", "),
                        ". You need to specify the disease to calculate the number of tested samples."))
          }
        }
      }
      if (isTRUE(tested)) {
        if (any(grepl(paste0("sum_und_", tolower(disease), "$"), column_sum))) {
          dfx <- subset(dfx, dfx[, paste0("sum_und_", tolower(disease))] >= min_prover)
        } else {
          if (length(column_sum) == 1) {
            dfx <- subset(dfx, dfx[, column_sum] >= min_prover)
            if (!any(grepl(paste0("sum_und_", tolower(disease)), column_sum))) {
              warning(paste("The number of tested samples were calculated using", column_ant))
            }
          } else {
            stop(paste0("The number of tested samples were given in the columns ",
                        paste(column_ant, collapse = ", "),
                        ". You need to specify the disease to calculate the number of tested samples."))
          }
        }
      }
    }
  }

  if (dim(dfx)[1] == 0) {
    warning(paste("There where no saker fulfilling the selection criterea.",
                  "Please check the selection criterea."))
  }
  # Sorts data in original order and removes sort key
  dfx <- dfx[order(dfx$original_sort_order), ]
  dfx$original_sort_order <- NULL

  return(dfx)
}

# To avoid fail in CMD check for global variables
utils::globalVariables("eier_lokalitetnr")
