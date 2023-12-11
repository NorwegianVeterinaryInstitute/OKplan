#' @title Get the holidays or working days 
#' @description Get the holidays or working days within one year. 
#'    The function is intended for use when planning sampling to 
#'    excluded days or weeks from the sampling plan. 
#'
#' @details \code{type} is used to select the type of holiday or 
#'    workday. Valid input are c("holiday", "sat_to_sun", "public_holiday", 
#'    "workday"). public_holiday are the non-moveable holidays, 
#'    Easter and Pentacost; sat_to_sun are Saturdays and Sundays; and 
#'    holiday are public_holiday and sat_to_sun combined. workday is
#'    the opposite of holiday when \code{exclude_trapped_days} = 
#'    \code{FALSE}.
#'     
#' \code{exclude_trapped_days} is used to exclude trapped days 
#'    and other days that many often takes a day off, i.e. the 
#'    Easter week and the Christmas week. It is only Valid for 
#'    workday and has no effect on the other types. Input 
#'    "trapped" or \code{TRUE} will exclude trapped days, 
#'    "easter" will exclude Monday to Wednesday before Thursday 
#'    and "xmas" will exclude the days in the week of Christmas 
#'    eve until New years eve. 
#'    
#'    The output is a data frame with the selected dates and the 
#'    day_of_week (integer) when \code{output} = "selected". When
#'    \code{output} = "raw" the data frame includes all dates and 
#'    the additional columns c("holiday", "sat_to_sun", "public_holiday", 
#'    "workday", "trapped" and "public"), see below for description.
#'     
#'    The output data frame for \code{output} = "raw":
#' \tabular{ll}{
#'    \strong{Column name} \tab \strong{Values} \cr
#'    date \tab Date. \cr
#'    day_of_week \tab Week day number, Monday = 1, Sunday = 7.
#'    sat_to_sun \tab Saturday and Sunday = 1, otherwise 0. \cr
#'    public_holiday \tab Public holidays = 1 otherwise = 0. \cr
#'    holiday \tab Saturday, Sunday and public holidays = 1, otherwise = 0. \cr
#'    workday \tab Working day, the opposite of holiday when \code{exclude_trapped_days} = \code{FALSE}. \cr
#'    public \tab Easter = "e", Pentacost = "p", non-moveable = "n", otherwise NA.
#'    trapped \tab trapped days (t), Easter week days (e) and/or Xmas week days (x) otherwise NA. \cr
#' }
#' 
#'  When \code{output} %in% c("fhi", "cstime") the data frame is
#'    formatted as the table cstime::nor_workdays_by_date  
#'    created by National Public Health Institute (FHI).
#'    
#' The function is limited to years from 1968, as before 1968
#'    Saturday was a normal working day in Norway. Be aware that 
#'    Saturday was a normal school day in Norway until and including 
#'    1972. 
#'
#' @param year [\code{integer(1)}]\cr
#'     Year. 
#' @param type [\code{character}]\cr
#'     The type(s) of holiday or workday, see details. Defaults to "workday".
#' @param exclude_trapped_days [\code{character} | \code{logical(1)}]\cr
#'     Should trapped days and common days off be excluded from workday?, 
#'     see details. Defaults to \code{FALSE}.
#' @param output [\code{character(1)}]\cr
#'     The output format of the data frame, see details. Defaults
#'     to "selected".
#'
#' @return data frame with the selected dates.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # Selects the public holidays
#'  public_holidays <- get_holiday(year = 2024,
#'                                 type = "public_holiday")
#'      
#' # Selects workdays except the trapped days                          
#'  workdays <- get_holiday(year = 2024,
#'                          type = "workday",
#'                          exclude_trapped_days = TRUE)
#'                          
#' # Selects workdays except days in Easter and Christmas week
#'  workdays <- get_holiday(year = 2024,
#'                          type = "workday",
#'                          exclude_trapped_days = c("easter", "xmas"))
#' 


get_holiday <- function (year, 
                         type = "workday", 
                         exclude_trapped_days = FALSE,
                         output = "selected") {
  
  ### ARGUMENT CHECKING ---- 
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  checkmate::assert_integerish(year,
                               lower = 1968,
                               len = 1,
                               any.missing = FALSE,
                               all.missing = FALSE,
                               unique = TRUE)
  type <- NVIcheckmate::match_arg(x = type,
                                  choices = c("holiday", "public_holiday", 
                                              "sat_to_sun", "workday"),
                                  several.ok = FALSE,
                                  ignore.case = TRUE,
                                  add = checks)
  checkmate::assert(checkmate::check_flag(exclude_trapped_days),
                    checkmate::check_subset(exclude_trapped_days, choices = c("easter", "trapped", "xmas")),
                    add = checks)
  output <- NVIcheckmate::match_arg(x = output,
                                  choices = c("cstime", "fhi", "raw", "selected"),
                                  several.ok = FALSE,
                                  ignore.case = TRUE,
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
  dates$day_of_week <- as.numeric(format(dates$date, format = "%u"))
  
  # Assign sat_to_sun
  dates$sat_to_sun <- 0
  dates[which(dates$day_of_week %in% c(6, 7)), "sat_to_sun"] <- 1
  
  # Assign public holidays
  dates$public <- NA_character_
  dates[which(dates$date %in% easter), "public"] <- "e"
  dates[which(dates$date %in% pentacost), "public"] <- "p"
  dates[which(dates$date %in% non_moveable), "public"] <- "n"
  
  dates$public_holiday <- 0
  dates[!is.na(dates$public), "public_holiday"] <- 1
  
  # Assign holidays
  dates$holiday <- 0
  dates[which(dates$sat_to_sun == 1 | dates$public_holiday == 1), "holiday"] <- 1
  
  # Assign workday
  dates$workday <- +(!dates$holiday)
  
  # Assign trapped days
  dates$trapped <- NA
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
  if (output == "selected") {
  if ("sat_to_sun" %in% type) {
    dates[which(dates$sat_to_sun == 1), "select"] <- 1
  }
  if ("public_holiday" %in% type) {
    dates[which(dates$public_holiday == 1), "select"] <- 1
  }
  if ("holiday" %in% type) {
    dates[which(dates$holiday == 1) , "select"] <- 1
  }
  if ("workday" %in% type) {
    dates[which(dates$workday == 1), "select"] <- 1
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
    dates <- subset(dates, dates$select == 1)
    dates <- dates[, c("date", "day_of_week")]
  }
  
  if (output == "raw") {
    dates <- dates[, c("date", "day_of_week", "sat_to_sun", "public_holiday", 
                       "holiday", "workday", "public",  "trapped")]
  }
  
  if (output %in% c("fhi", "cstime")) {
    dates$mon_to_fri <- +(!dates$sat_to_sun)
    dates <- dates[, c("date", "day_of_week", "mon_to_fri", "sat_to_sun", "public_holiday", 
                       "holiday", "workday")]
    colnames(dates)[6] <- "freeday"
  }
  
  return(dates)
}
