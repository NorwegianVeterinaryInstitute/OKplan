#' @title Gets herds tested within an surveillance programme 
#' @description Gets herds that have been sampled or tested within 
#'     a surveillance programmet for the selected years. You can 
#'     choose between herds that have submitted samples or herds 
#'     for which a certain number of samples have been examined 
#'     for a specific disease.
#' @details For programmes having several surveillance streams, 
#'     it is possible to select surveillance streams based on 
#'     species and production type. 
#'     
#'     It is possible to define a minimum requirement of number of 
#'     samples received or tested. For programmes covering several 
#'     infections, it is necessary to input the disease for which 
#'     the samples should have been tested. The disease name must 
#'     be given as it is written in the column name for the number 
#'     of examined samples. 

#' @param eos_table EOS table name. 
#' @param year One or more years that should be selected. Defaults 
#'     to previous year. 
#' @param species The species that should be selected. Defaults to 
#'     \code{NULL} 
#' @param disease The disease for which number of tested samples should 
#'     be calculated. Defaults to \code{NULL}. 
#' @param production The production type for which number of tested 
#'     samples should be calculated. Defaults to \code{NULL} 
#' @param min_prover Minimum number of samples that should have been 
#'     received or examined for the herd to be counted as sampled or 
#'     tested. No check is done if equal -1. Defaults to -1. 
#' @param tested If \code{TRUE}, the number of tested samples, If 
#'     \code{FALSE}, the number of received samples. Defaults to 
#'     \code{FALSE}. 
#' @return data.frame with tested or sampled locations. 
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export

get_tested_herds <- function(eos_table,
                             year = as.numeric(format(Sysdate(), "%Y")) - 1,
                             species = NULL, 
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
                               len = 1,
                               add = checks)
  checkmate::assert_character(species, min.len= 1, null.ok = TRUE, add = checks)
  checkmate::assert_character(production, min.len = 1, null.ok = TRUE, add = checks)
  checkmate::assert_string(disease, min.chars = 1, null.ok = TRUE, add = checks)
  checkmate::assert_integerish(min_prover, 
                               lower = - 1, 
                               any.missing = FALSE, all.missing = FALSE,
                               len = 1,
                               null.ok =TRUE, 
                               add = checks)
  checkmate::assert_flag(tested, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  
  dtx <- read_eos_data(eos_table = eos_table,
                       year = year) 
  dtx$original_sort_order <- seq_len(nrow(dtx)) 
  dtx <- standardize_eos_data(dtx, dbsource = eos_table) 
  
  dtx[which(nchar(eier_lokalitetnr) ==10), "eier_lokalitetnr"] <- substr(dtx[which(nchar(eier_lokalitetnr) == 10), "eier_lokalitetnr"], 1, 8)
  
  dtx <- subset(dtx, !is.na(dtx$eier_lokalitetnr) & dtx$eier_lokalitetnr != "") 
  
  # Select species
  if (!is.null(species)) {
    dtx <- subset(dtx, dtx$art %in% species) 
  } 
  # Select production type
  if (!is.null(production)) {
    dtx <- subset(dtx, dtx$driftsform %in% production) 
  } 
  
  column_ant <- grep("ant_", colnames(dtx), value = TRUE) 
  column_sum <- gsub("ant_", "sum_", column_ant) 
  agg_dtx <- aggregate(x = dtx[, column_ant] , by = dtx$eier_lokalitetnr, FUN = sum) 
  colnames(agg_dtx)[2:(length(column_ant) + 1)] <- column_sum
  merge(dtx, agg_dtx, by = "eier_lokalitetnr") 
  
  # Select herd above minimum number of samples 
  if (min_prover > - 1) {
    if (isFALSE(tested)) {
      if (any(isTRUE(grep("sum_prover", column_sum)))) {
        dtx <- subset(dtx, dtx$sum_prover >= min_prover) 
      } else {
        if (lenght(column_sum) == 1) {
          dtx <- subset(dtx, dtx[, "column_sum"] >= min_prover) 
          warning(paste("The number of received samples could not be calculated, but the number of tested samples were calculated using", column_ant)) 
        } else {
          # error("The number of received samples could not be calculated, and the number of tested samples were given in the columns", column_ant, ". You need to specify the disease to calculate the number of tested samples."))  
        } 
      } 
    } 
    if (isTRUE(tested)) {
      if (any(isTRUE(grep(paste0("sum_und_", tolower(disease)), column_sum)))) {
        dtx <- subset(dtx, dtx[, paste0("sum_und_", tolower(disease))] >= min_prover) 
      } else {
        if (lenght(column_sum) == 1) {
          dtx <- subset(dtx, dtx[, "column_sum"] >= min_prover) 
          warning(paste("The number of tested samples were calculated using", column_ant)) 
        } else {
          # error("The number of tested samples were given in the columns", column_ant, ". You need to specify the disease to calculate the number of tested samples."))  
        } 
      } 
    } 
    dtx <- subset(dtx, dtx[, paste0("sum_und_", tolower(disease))] >= min_prover) 
  } 

# Sorts data in original order and removes sort key 
dtx <- dtx[order(dtx$original_sort_order), ]
dtx$original_sort_order <- NULL

return(dtx)
} 


