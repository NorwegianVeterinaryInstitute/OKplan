#' @title Control of standard output file with OK selection
#' @description Standard control by performing descriptive statistics of variables in the output file with OK selection.
#'
#' @details Under development. This should be rewritten to produce nicer output.
#'
#'     Gives descriptive statistics of the selection. This is used to see if the number of selected units per category are correct.
#'     If any mistakes are found, one must go back and correct in the script that produces the selection.
#'
#'     Before the control is run, the column names must have been standardized using \code{NVIdb::standardize_columns}.
#'
#' @param data Data frame with selection for a OK programme.
#'
#' @return Prints results of the control to the output window.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Control OK selection data
#'
#' # Read example data
#' okplan_MRSA <- read.csv2(file = paste0(set_dir_NVI("OKprogrammer"), "Rutine", plan_aar, "/planlegging/resultater/utvalgslister/data_MRSA_alle_gris.csv"),
#'                         colClasses = colclasses,
#'                         fileEncoding = "UTF-8")
#'
#' # Control
#' control_OK_selection(okplan_MRSA)
#'
#' }
#'
#'
control_OK_selection <- function(data) {

  # Antall besetninger og prøver som skal testes fordelt på grupper
  print(ftable(data[, c("ok_hensiktkode", "kategori", "statuskode")], exclude = NULL))

  print("Totalt antall besetninger og prøver som skal testes")
  ktr <- data %>%
    poorman::group_by(ok_artkode, statuskode) %>%
    poorman::summarise(antall = n(), ant_prover = sum(ant_prover, na.rm = TRUE), .groups = "keep") %>%
    poorman::ungroup()
  print(ktr)

  print("Antall utvalgte besetninger med mer enn en registrering per prodnr8")
  ktr <- data %>%
    poorman::add_count(ok_hensiktkode, eier_lokalitetnr) %>%
    poorman::ungroup() %>%
    poorman::filter(n > 1) %>%
    poorman::select(eier_lokalitetnr, eier_lokalitet, postnr, poststed)
  print(ktr)

  print("Utvalgte besetninger med missing prodnr8 eller missing navn")
  print(subset(data[, c("eier_lokalitetnr", "eier_lokalitet", "postnr", "poststed")],
               is.na(data$eier_lokalitetnr) | trimws(data$eier_lokalitetnr) == "" |
                 is.na(data$eier_lokalitet) | trimws(data$eier_lokalitet) == ""))



  # variabelfrekvenser
  print(ftable(data[, c("ok_hensiktkode", "statuskode", "kategori")], exclude = NULL))


  print(ftable(data[, c("ok_programkode", "analyttkode")], exclude = NULL))
  print(ftable(data[, c("mt_region")], exclude = NULL))
  print(ftable(data[, c("mt_avdeling")], exclude = NULL))
  print(ftable(data[, c("ok_aar")], exclude = NULL))
  print(ftable(data[, c("ok_programkode")], exclude = NULL))
  print(ftable(data[, c("ok_hensiktkode")], exclude = NULL))
  print(ftable(data[, c("eier_lokalitettype")], exclude = NULL))
  print(ftable(data[, c("analyttkode")], exclude = NULL))
  print(ftable(data[, c("annen_aktortype")], exclude = NULL))
  print(ftable(data[, c("annen_aktornr")], exclude = NULL))
  print(ftable(data[, c("annen_aktor")], exclude = NULL))
  print(ftable(data[, c("ant_prover", "statuskode")], exclude = NULL))
  print(ftable(data[, c("ok_artkode")], exclude = NULL))
  print(ftable(data[, c("ok_driftsformkode")], exclude = NULL))
  print(ftable(data[, c("storrelseskategori", "statuskode")], exclude = NULL))
  print(ftable(data[, c("kategori", "statuskode")], exclude = NULL))
  print(ftable(data[, c("materialekode", "statuskode")], exclude = NULL))
  print(ftable(data[, c("statuskode")], exclude = NULL))
  print(ftable(data[, c("status_dato")], exclude = NULL))
  print(ftable(data[, c("prioritet_av_reserve")], exclude = NULL))
  print(ftable(data[, c("utvalg_laget_dato")], exclude = NULL))

  # &oMTreg*&ostatus &oMTavd*&ostatus

  # &otidspunkt  &otidsenhet  &olab &oRefNr
  # &omottatt
  #
}


