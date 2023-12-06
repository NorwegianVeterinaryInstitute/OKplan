#' @title Get the holidays or working days
#' @description Get the holidays or working days for one year
#'
#' @details Performs common cleaning of PJSdata by removing samples that usually
#'     should not be included when analyzing PJSdata. The cleaning is dependent
#'     on having the following columns eier_lokalitettype, eierlokalitetnr and
#'     hensiktkode.
#'
#'     \code{abroad = "exclude"} will exclude samples that have eier_lokalitet
#'     of type "land" and eier_lokalitetnr being different from NO. Samples
#'     registered on other types than LAND are not excluded.
#'
#'     \code{quality = "exclude"} will exclude all samples registered s quality
#'     assurance and ring trials, i.e. hensiktkode starting with "09".
#'
#' @param year Data frame with data extracted from PJS.
#' @param type If equal "exclude", samples from abroad are excluded. Allowed
#'     values are c("exclude", "include").
#' @param exclude_trapped_days If equal "exclude", samples registered as quality assurance
#'     and ring trials are excluded. Allowed values are c("exclude", "include").
#'
#' @return data frame with selected dates.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' 
#'  public_holidays <- get_holiday(year = 2024,
#'                                 type = "public")
#'                                 
#'  workdays <- get_holiday(year = 2024,
#'                          type = "workday",
#'                          exclude_trapped_days = TRUE)
#'                          
#'  workdays <- get_holiday(year = 2024,
#'                          type = "workday",
#'                          exclude_trapped_days = c("easter", "xmas"))
#' }
#'
# date
# Date.
# 
# day_of_week
# Integer. 1 = Monday, 7 = Sunday
# 
# mon_to_fri
# Integer. 1 between Monday and Friday, 0 between Saturday and Sunday
# 
# sat_to_sun
# Integer. 1 between Saturday and Sunday, 0 between Monday and Friday
# 
# public_holiday
# Integer. 1 if public holiday (helligdag), 0 if not public holiday
# 
# freeday
# Integer. 1 if public holiday (helligdag) or sat_to_sun==1, 0 otherwise
# 
# workday
# Integer. 1 if freeday==0, 0 if freeday==1


get_holiday <- function (year, 
                         type = "all", 
                         exclude_trapped_days = FALSE) {
  
  ### ARGUMENT CHECKING ---- 
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  checkmate::assert_integerish(year,
                               lower = 1971,
                               upper = as.numeric(format(Sys.Date(), "%Y")) + 5,
                               len = 1,
                               any.missing = FALSE,
                               all.missing = FALSE,
                               unique = TRUE)
  type <- NVIcheckmate::match_arg(x = type,
                                  choices = c("holiday", "public", "sunday", "saturday",
                                              "weekend", "workday", "all"),
                                  several.ok = FALSE,
                                  ignore.case = TRUE,
                                  add = checks)
  checkmate::assert(checkmate::check_false(exclude_trapped_days),
                    checkmate::check_subset(exclude_trapped_days, choices = c("easter", "xmas", "trapped")),
                    add = checks)
  
  # Report check-results
  checkmate::reportAssertions(checks)
  
  ### NATIONAL HOLIDAYS ---- 
  # Calculate Easter day
  # reference
  K <- floor(year/100)
  M <- 15 + floor((3 * K + 3)/4) - floor((8 * K + 13)/25)
  S <- 2 - floor((3 * K + 3)/4)
  A <- year %% 19
  D <- (19*A+M) %% 30
  R <- floor((D+A/11)/29)
  OG <- 21 + D - R
  SZ <- 7 - ((year + floor(year/4)+S) %% 7) 
  OE <- 7 - ((OG-SZ) %% 7) 
  
  easterday <- as.Date(paste0(year, "-03-01")) - 1 + OG + OE
  easter <- rep(easterday, 4) + c(-3, -2, 0, 1)
  easter_trapped <- rep(easterday, 3) + c(-6, -5, -4)
  pentacost <- rep(easterday, 3) + c(39, 49, 50)
  non_moveable <- as.Date(paste0(year, c("-01-01", "-05-01", "-05-17", "-12-25", "-12-26"))) 
  days_before_newyear <- 7 
  if (as.numeric(format(as.Date(paste0(year, "-12-31")), "%u")) <= 3) {
    days_before_newyear <- days_before_newyear + as.numeric(format(as.Date(paste0(year, "-12-31")), "%u"))
  }
  # as.Date(paste0(year, "-12-31")) - as.numeric(format(as.Date(paste0(year, "-12-31")), "%u"))
  
  xmas_trapped <- rep(as.Date(paste0(year, "-12-31")), days_before_newyear) + c(-(days_before_newyear - 1):0)
  ### CATEGORISE INTO HOLIDAYS ---- 
  # create data frame with all dates for year[i]
  dates  <- as.data.frame(matrix(data = c(as.Date(paste0(year, "-01-01")):as.Date(paste0(year, "-12-31"))),
                                 dimnames = list(NULL, "date")))
  dates$date <- as.Date(dates$date, origin = "1970-01-01")
  
  # Assign weekday number
  dates$weekday <- format(dates$date, format = "%u")
  
  # Assign weekend
  dates$weekend <- "0"
  dates[which(dates$weekday %in% c("6", "7")), "weekend"] <- "1"
  
  # Assign public holidays
  dates$public <- "0"
  dates[which(dates$date %in% easter), "public"] <- "e"
  dates[which(dates$date %in% pentacost), "public"] <- "p"
  dates[which(dates$date %in% non_moveable), "public"] <- "n"
  
  # Assign holidays
  dates$holiday <- "0"
  dates[which(dates$weekend == "1" | dates$public != "0"), "holiday"] <- "1"
  
  # Assign workday
  dates$workday <- +(!as.numeric(dates$holiday))
  
  # assign trapped days
  dates$trapped <- "0"
  if ("easter" %in% exclude_trapped_days) {
    dates[which(dates$date %in% easter_trapped), "trapped"] <- "e"
  }
  if ("xmas" %in% exclude_trapped_days) {
    dates[which(dates$date %in% xmas_trapped), "trapped"] <- "x"
  }
  
  if ("trapped" %in% exclude_trapped_days) {
    dates$behind <- c(NA, dates[c(1:(length(dates$holiday) - 1)), "holiday"])
    dates$ahead <- c(dates[c(2:length(dates$holiday)), "holiday"], "1")
    dates[which(dates$ahead == "1" & dates$behind == "1" & dates$holiday == "0" ), "trapped"] <- "t"
    dates[, c("behind", "ahead")] <- c(NULL, NULL)
  }

  ### SELECT ROWS TO REPORT ----
  # if ("easter" %in% type) {
  #   dates[which(dates$holiday == "e") , "select"] <- 1
  # }
  # if ("moving" %in% type) {
  #   dates[which(dates$holiday %in% c("e", "p")) , "select"] <- 1
  # }
  if ("public" %in% type) {
    dates[which(dates$public %in% c("e", "p", "n")), "select"] <- 1
  }
  if ("sunday" %in% type) {
    dates[which(dates$weekday == 7) , "select"] <- 1
  }
  if ("saturday" %in% type) {
    dates[which(dates$weekday == 6), "select"] <- 1
  }
  if ("weekend" %in% type) {
    dates[which(dates$weekend == 1), "select"] <- 1
  }
  if ("holiday" %in% type) {
    dates[which(dates$holiday %in% c("e", "p", "n", "6", "7")) , "select"] <- 1
  }
  if ("workday" %in% type) {
    dates[which(dates$workday %in% c("1")), "select"] <- 1
    if ("easter" %in% exclude_trapped_days) {
      dates[which(dates$trapped == "e"), "select"] <- 0
    }
    if ("xmas" %in% exclude_trapped_days) {
      dates[which(dates$trapped == "x"), "select"] <- 0
    }
    
    if ("trapped" %in% exclude_trapped_days | isTRUE(exclude_trapped_days)) {
      dates[which(dates$trapped == "t"), "select"] <- 0
    }
  }
  
  if (!"all" %in% type) {
    dates <- subset(dates, dates$select == 1)
    dates <- dates[, c("date", "weekday", type)]
  }
  
  return(dates)
}
